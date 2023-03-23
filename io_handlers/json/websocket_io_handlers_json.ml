open Core
open Async_kernel
module Parse_error = String

module On_event = struct
  type t =
    { on_message : Jsonaf.t -> unit
    ; on_error : Parse_error.t -> [ `Continue_parsing | `Stop_parsing ]
    }
end

type t =
  { mutable continue : (read, Iobuf.seek) Iobuf.t -> Jsonaf.t Angstrom.Unbuffered.state
  ; ws : Websocket_io_handlers.byte_stream Websocket_io_handlers.t
  ; mutable on_event : On_event.t option
  ; to_send : Faraday.t
  ; flushed : (unit, read_write) Bvar.t
  }

let parser = Jsonaf_kernel.Parser.t_without_trailing_whitespace

let wrap_continue continue =
  stage (fun iobuf ->
    continue
      (Iobuf.Expert.buf iobuf)
      ~off:(Iobuf.Expert.lo iobuf)
      ~len:(Iobuf.length iobuf)
      Angstrom.Unbuffered.Incomplete)
;;

let init_parser () =
  match Angstrom.Unbuffered.parse parser with
  | Done _ | Fail _ -> assert false
  | Partial { committed; continue } ->
    assert (committed = 0);
    unstage (wrap_continue continue)
;;

let on_readable t =
  match t.on_event with
  | None -> ()
  | Some { on_message; on_error } ->
    let recv_window = Websocket_io_handlers.recv_window t.ws in
    let iobuf = Netkit.Recv_window.as_iobuf recv_window in
    let rec loop () =
      match t.continue iobuf with
      | Done (read, message) ->
        on_message message;
        Iobuf.advance iobuf read;
        t.continue <- init_parser ();
        loop ()
      | Fail (consumed, _marks, error) ->
        (match on_error error with
         | `Stop_parsing -> ()
         | `Continue_parsing ->
           (* Sometimes the returned [consume] is 0 even though we have failed parsing. We
              still want to advance in this case if we can, in order to move past the bad
              byte *)
           let advance =
             match consumed with
             | 0 -> if Iobuf.length iobuf > 0 then 1 else 0
             | _ -> consumed
           in
           Iobuf.advance iobuf advance;
           t.continue <- init_parser ();
           if advance > 0 then loop ())
      | Partial { committed; continue } ->
        Iobuf.advance iobuf committed;
        t.continue <- unstage (wrap_continue continue);
        if committed > 0 then loop ()
    in
    loop ();
    Netkit.Recv_window.notify_maybe_bytes_consumed recv_window
;;

let rec on_sendable t =
  let send_window = Websocket_io_handlers.send_window t.ws in
  if Netkit.Send_window.bytes_queued send_window = 0 then Bvar.broadcast t.flushed ();
  let iobuf = Netkit.Send_window.prepare_write_available send_window in
  match Faraday.operation t.to_send with
  | `Yield | `Close -> ()
  | `Writev iovecs ->
    let rec loop iovecs ~written =
      match iovecs with
      | [] -> written
      | (iovec : Bigstring.t Faraday.iovec) :: rest ->
        let len_to_write = Int.min (Iobuf.length iobuf) iovec.len in
        Iobuf.Fill.bigstring iobuf iovec.buffer ~str_pos:iovec.off ~len:len_to_write;
        let written = written + len_to_write in
        if len_to_write = iovec.len then loop rest ~written else written
    in
    let written = loop iovecs ~written:0 in
    Faraday.shift t.to_send written;
    (match Netkit.Send_window.finish_write send_window ~flush:Yes with
     | Queued | Closed | Nothing_to_flush -> ()
     | Send_window_grew -> on_sendable t)
;;

let create ws =
  { continue = init_parser ()
  ; ws
  ; on_event = None
  ; to_send = Faraday.create 32_000
  ; flushed = Bvar.create ()
  }
;;

let start t ~on_message ~on_error =
  t.on_event <- Some { on_message; on_error };
  Websocket_io_handlers.start
    t.ws
    { on_readable = (fun () -> on_readable t); on_sendable = (fun () -> on_sendable t) }
;;

let send t message =
  Jsonaf.Serializer.serialize message t.to_send;
  on_sendable t
;;

let flushed t =
  match Netkit.Send_window.bytes_queued (Websocket_io_handlers.send_window t.ws) with
  | 0 -> Deferred.unit
  | _ -> Bvar.wait t.flushed
;;
