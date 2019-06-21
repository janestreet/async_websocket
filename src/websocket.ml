open Core
open Async

module Websocket_role = struct
  type t = Client | Server [@@deriving sexp_of]

  (*  https://tools.ietf.org/html/rfc6455#section-5.3 *)
  let should_mask = function
    | Client -> true
    | Server -> false
end

(* https://tools.ietf.org/html/rfc6455#section-7.4 *)
module Connection_close_reason : sig
  type t =
    | Normal_closure
    | Endpoint_going_away
    | Protocol_error
    | Cannot_accept_data
    | Reserved_0
    | No_status_code
    | Closed_abnormally
    | Invalid_message_sent
    | Policy_violation
    | Message_too_large
    | Invalid_handshake
    | Unexpected_condition
    | Tls_handshake_failure
  [@@deriving sexp_of]

  val of_int : int -> t Or_error.t
  val to_int : t -> int
end = struct
  module T = struct
    type t =
      | Normal_closure
      | Endpoint_going_away
      | Protocol_error
      | Cannot_accept_data
      | Reserved_0
      | No_status_code
      | Closed_abnormally
      | Invalid_message_sent
      | Policy_violation
      | Message_too_large
      | Invalid_handshake
      | Unexpected_condition
      | Tls_handshake_failure
    [@@deriving sexp, compare, enumerate]
  end
  include T
  include Comparable.Make(T)

  let to_int = function
    | Normal_closure        -> 1000
    | Endpoint_going_away   -> 1001
    | Protocol_error        -> 1002
    | Cannot_accept_data    -> 1003
    | Reserved_0            -> 1004
    | No_status_code        -> 1005
    | Closed_abnormally     -> 1006
    | Invalid_message_sent  -> 1007
    | Policy_violation      -> 1008
    | Message_too_large     -> 1009
    | Invalid_handshake     -> 1010
    | Unexpected_condition  -> 1011
    | Tls_handshake_failure -> 1015
  ;;

  let of_int_map =
    List.map all ~f:(fun t -> to_int t, t)
    |> Int.Map.of_alist_exn
  ;;

  let of_int code =
    match Core.Map.find of_int_map code with
    | Some t -> Ok t
    | None -> error_s [%message "Unknown close code" (code : int)]
end

(* RFC 6455: The WebSocket Protocol: http://tools.ietf.org/html/rfc6455 *)

type raw = {
  reader : Reader.t;
  writer : Writer.t;
  closed : (Connection_close_reason.t * string) Ivar.t;
}

type t = {
  raw   : raw;
  pipes : string Pipe.Reader.t * string Pipe.Writer.t;
} [@@deriving fields]

module Opcode = struct
  type t =
    | Continuation
    | Text
    | Binary
    | Close
    | Ping
    | Pong
    | Ctrl of int
    | Nonctrl of int

  let of_int i = match i land 0xf with
    | 0x0                   -> Continuation
    | 0x1                   -> Text
    | 0x2                   -> Binary
    | 0x8                   -> Close
    | 0x9                   -> Ping
    | 0xA                   -> Pong
    | i when i > 2 && i < 8 -> Nonctrl i
    | i                     -> Ctrl i
  ;;

  let to_int = function
    | Continuation       -> 0x0
    | Text               -> 0x1
    | Binary             -> 0x2
    | Close              -> 0x8
    | Ping               -> 0x9
    | Pong               -> 0xA
    | Ctrl i | Nonctrl i -> i
  ;;
end

module Frame = struct
  type t =
    { opcode : Opcode.t
    ; final : bool
    ; content : string
    }

  (* Extensions aren't implemented *)
  let create ~opcode ?(final=true) content =
    { opcode; final; content }


  (* See rfc6455 - 5.5.1
     The Close frame MAY contain a body (the "Application data" portion of
     the frame) that indicates a reason for closing, such as an endpoint
     shutting down, an endpoint having received a frame too large, or an
     endpoint having received a frame that does not conform to the format
     expected by the endpoint.  If there is a body, the first two bytes of
     the body MUST be a 2-byte unsigned integer (in network byte order)
     representing a status code with value /code/ defined in Section 7.4.
     Following the 2-byte integer, the body MAY contain UTF-8-encoded data
     with value /reason/, the interpretation of which is not defined by
     this specification.  This data is not necessarily human readable but
     may be useful for debugging or passing information relevant to the
     script that opened the connection.  As the data is not guaranteed to
     be human readable, clients MUST NOT show it to end users. *)
  let create_close ~code ?final content =
    let len = String.length content in
    let content' = Bytes.create (len + 2) in
    Bytes.From_string.blit ~src:content ~src_pos:0 ~dst:content' ~dst_pos:2 ~len;
    Binary_packing.pack_unsigned_16_big_endian ~buf:content' ~pos:0 code;
    create ~opcode:Close ?final (Bytes.to_string content')

  let bit_is_set idx v = (v lsr idx) land 1 = 1

  let set_bit v idx b =
    if b then v lor (1 lsl idx) else v land (lnot (1 lsl idx))

  let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)

  let random_bytes ~len =
    Bytes.init len ~f:(fun _ -> Char.of_int_exn (Random.int 128))
  ;;

  let xor_with_mask mask msg =
    for i = 0 to Bytes.length msg - 1 do
      Bytes.set msg i (Char.of_int_exn (
        Char.to_int (Bytes.get mask (i mod 4)) lxor Char.to_int (Bytes.get msg i)
      ))
    done
  ;;

  let write_int16 writer n =
    let buf = Bytes.create 2 in
    Binary_packing.pack_unsigned_16_big_endian ~buf ~pos:0 n;
    Writer.write_bytes ~pos:0 ~len:2 writer buf;
  ;;

  let write_int64 writer n =
    let buf = Bytes.create 8 in
    Binary_packing.pack_signed_64_big_endian ~buf ~pos:0 n;
    Writer.write_bytes ~pos:0 ~len:8 writer buf;
  ;;

  let write_frame ~masked writer frame =
    if Writer.is_closed writer
    then ()
    else begin
      let content = Bytes.of_string frame.content in
      let len = Bytes.length content in
      let opcode = Opcode.to_int frame.opcode in
      let payload_length =
        match len with
        | n when n < 126      -> len
        | n when n < 1 lsl 16 -> 126
        | _                   -> 127
      in
      let hdr = 0 in
      let hdr = set_bit hdr 15 frame.final in
      let hdr = hdr lor (opcode lsl 8) in
      let hdr = set_bit hdr 7 masked in
      let hdr = hdr lor payload_length in
      let buf = Bytes.create 2 in
      Binary_packing.pack_unsigned_16_big_endian ~buf ~pos:0 hdr;
      Writer.write_bytes ~len:2 ~pos:0 writer buf;
      begin match len with
      | n when n < 126 -> ()
      | n when n < (1 lsl 16) -> write_int16 writer n
      | n         -> write_int64 writer (Int64.of_int n)
      end;
      if masked then (
        let mask = random_bytes ~len:4 in
        Writer.write_bytes ~pos:0 ~len:4 writer mask;
        xor_with_mask mask content
      );
      Writer.write_bytes ~pos:0 ~len  writer content
    end
  ;;

  let close_cleanly ~code ~reason (ws : raw) =
    Ivar.fill_if_empty ws.closed (code,reason);
    return `Eof
  ;;

  let read_int64 (ws : raw) =
    let buf = Bytes.create 8 in
    Reader.really_read ws.reader ~len:8 buf >>= function
    | `Ok -> return (`Ok (Binary_packing.unpack_signed_64_big_endian ~buf ~pos:0))
    | `Eof _ ->
      close_cleanly
        ~code:Connection_close_reason.Protocol_error
        ~reason:"Did not receive correct length"
        ws
  ;;

  let read_int16 (ws : raw) =
    let buf = Bytes.create 2 in
    Reader.really_read ws.reader ~len:2 buf >>= function
    | `Ok -> return (
      `Ok (Int64.of_int (Binary_packing.unpack_unsigned_16_big_endian ~buf ~pos:0))
    )
    | `Eof _ ->
      close_cleanly
        ~code:Connection_close_reason.Protocol_error
        ~reason:"Did not receive correct length"
        ws
  ;;



  let read_frame ({reader; writer=_; closed} as ws) =
    if Ivar.is_full closed then return `Eof
    else begin
      let buf = Bytes.create 2 in
      Reader.really_read reader ~len:2 buf
      >>= function
      | `Eof n ->
        close_cleanly
          ~code:Connection_close_reason.Protocol_error
          ~reason:(sprintf "Expected 2 byte header, got %d" n)
          ws
      | `Ok ->
        let header_part1 = Char.to_int (Bytes.get buf 0) in
        let header_part2 = Char.to_int (Bytes.get buf 1) in
        let final  = bit_is_set 7 header_part1 in
        let opcode = Opcode.of_int (int_value 0 4 header_part1) in
        let masked = bit_is_set 7 header_part2 in
        let length = int_value 0 7 header_part2 in

        let payload_length_deferred =
          match length with
          | 126            -> read_int16 ws
          | 127            -> read_int64 ws
          | i when i < 126 -> return (`Ok (Int64.of_int i))
          | n              ->
            close_cleanly
              ~code:Connection_close_reason.Protocol_error
              ~reason:(sprintf "Invalid payload length %d" n)
              ws
        in
        payload_length_deferred
        >>= function
        | `Eof               -> return `Eof
        | `Ok payload_length ->
          let payload_length = Int64.to_int_exn payload_length in
          let mask = Bytes.create 4 in
          let read_mask =
            if masked
            then begin
              Reader.really_read reader ~len:4 mask >>= function
              | `Ok -> return (`Ok ())
              | `Eof n ->
                close_cleanly
                  ~code:Connection_close_reason.Protocol_error
                  ~reason:(sprintf "Expected 4 byte mask, got %d" n)
                  ws
            end
            else return (`Ok ())
          in
          read_mask
          >>= function
          | `Eof -> return `Eof
          | `Ok () ->
            let content = Bytes.create payload_length in
            (if payload_length = 0 then return `Ok
             else Reader.really_read reader ~len:payload_length content)
            >>= function
            | `Ok -> (
                if masked then xor_with_mask mask content;
                return (`Ok (create ~opcode ~final (Bytes.to_string content)))
              )
            | `Eof n ->
              close_cleanly
                ~code:Connection_close_reason.Protocol_error
                ~reason:(sprintf "Read %d bytes, expected %d bytes" n payload_length)
                ws
    end
end

module Pipes = struct
  let recv_pipe ~masked (ws : raw) =
    let rec read_one r =
      Frame.read_frame {ws with reader = r}
      >>= function
      | `Eof -> return `Eof
      | `Ok frame ->
        match frame.opcode with
        | Close ->
          begin
            Frame.close_cleanly
              ~code:Connection_close_reason.Normal_closure
              ~reason:"Received close message"
              ws
            >>= fun eof ->
            (* flush to make sure the close frame was sent to the client *)
            Writer.flushed ws.writer
            >>| fun () ->
            eof
          end
        | Ping  -> (Frame.write_frame ws.writer ~masked {frame with opcode = Pong}; read_one r)
        | Pong  -> read_one r
        | _     -> return (`Ok frame.content)
    in
    Reader.read_all ws.reader read_one
  ;;

  let send_pipe ~opcode ~masked (ws : raw) =
    let write_message msg =
      Frame.write_frame ws.writer ~masked (Frame.create ~opcode msg)
    in
    let to_client_r, to_client_w = Pipe.create () in
    let to_client_closed =
      Writer.transfer ws.writer to_client_r write_message
    in
    upon to_client_closed (fun () ->
      ignore begin
        Frame.close_cleanly
          ~code:Connection_close_reason.Normal_closure
          ~reason:"Pipe was closed"
          ws
      end
    );
    to_client_w
  ;;
end

let magic_string = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let sec_websocket_accept_header_value ~sec_websocket_key =
  let module C = Crypto.Cryptokit in
  C.transform_string (C.Base64.encode_compact ())
    (C.hash_string (C.Hash.sha1 ()) (sec_websocket_key ^ magic_string))
  ^ "=" (* cryptokit leaves out a trailing equals sign *)


let close ~code ~reason ~masked (ws: raw) =
  Frame.write_frame ws.writer ~masked (Frame.create_close ~code reason);
  (* Wait for the writer to be flushed before actually closing it,
     otherwise the closing frame won't be sent. *)
  Writer.flushed ws.writer
  >>= fun () ->
  Writer.close ws.writer
  >>= fun () ->
  Reader.close ws.reader

;;

let close_finished { raw = { closed; writer; reader}; pipes = _ } =
  Ivar.read closed
  >>= fun res ->
  (* Always close writers before readers due to the way TCP writers work *)
  Writer.close_finished writer
  >>= fun () ->
  Reader.close_finished reader
  >>| fun () ->
  res

let create ?(opcode=`Text) ~(role : Websocket_role.t) reader writer =
  let opcode = match opcode with
    | `Text   -> Opcode.Text
    | `Binary -> Opcode.Binary
  in
  let masked = Websocket_role.should_mask role in
  let closed = Ivar.create () in
  let ws = { reader; writer; closed } in
  don't_wait_for begin
    Ivar.read closed
    >>= fun (code,reason) ->
    close ~code:(Connection_close_reason.to_int code) ~reason ~masked ws
  end;
  let reader = Pipes.recv_pipe ~masked ws in
  let writer = Pipes.send_pipe ~opcode ~masked ws in
  { pipes = reader, writer; raw = ws }
