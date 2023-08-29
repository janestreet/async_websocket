open Core
open Async
module Opcode = Opcode
module Connection_close_reason = Connection_close_reason
module Frame_reader = Frame.Frame_reader
module Frame = Frame
module Iobuf_writer = Frame.Iobuf_writer
module Content_reassembler = Content_reassembler

module Websocket_role = struct
  type t =
    | Client
    | Server
  [@@deriving sexp_of]

  (*  https://tools.ietf.org/html/rfc6455#section-5.3 *)
  let should_mask = function
    | Client -> true
    | Server -> false
  ;;
end

(* RFC 6455: The WebSocket Protocol: http://tools.ietf.org/html/rfc6455 *)

type raw =
  { reader : Reader.t
  ; writer : Writer.t
  ; closed : (Connection_close_reason.t * string * Info.t option) Ivar.t
  }

type t =
  { raw : raw
  ; pipes : string Pipe.Reader.t * string Pipe.Writer.t
  ; read_opcode_bus : (Opcode.t -> unit) Bus.Read_write.t
  ; masked : bool
  }
[@@deriving fields ~getters]

let close_cleanly ~code ~reason ~info ws =
  Ivar.fill_if_empty ws.closed (code, reason, info)
;;

let transport t =
  let reader, writer = t.pipes in
  Async_rpc_kernel.Pipe_transport.create
    Async_rpc_kernel.Pipe_transport.Kind.string
    reader
    writer
;;

let frame_received t = read_opcode_bus t |> Bus.read_only

let send_ping t msg =
  Frame.write_frame t.raw.writer ~masked:t.masked (Frame.create ~opcode:Ping msg)
;;

module Pipes = struct
  let close_info ?frame ?partial_content ?unconsumed_data () =
    let partial_content =
      Option.filter partial_content ~f:(fun s -> not (String.is_empty s))
    in
    match frame, partial_content, unconsumed_data with
    | None, None, None -> None
    | _ ->
      Some
        (Info.create_s
           [%sexp
             { frame : (Frame.t option[@sexp.option])
             ; partial_content : (string option[@sexp.option])
             ; unconsumed_data : (string option[@sexp.option])
             }])
  ;;

  let recv_pipe ~read_opcode_bus ~masked (ws : raw) =
    Pipe.create_reader ~close_on_exception:true (fun writer ->
      let content_handler content =
        Pipe.write_without_pushback_if_open writer (Iobuf.to_string content)
      in
      let ping_handler ~content =
        Frame.write_frame
          ws.writer
          ~masked
          { Frame.opcode = Pong; final = true; content = Iobuf.to_string content }
      in
      let protocol_error_handler ~reason ~partial_content ~frame =
        close_cleanly
          ~code:Protocol_error
          ~reason
          ~info:(close_info ?frame ?partial_content ())
          ws
      in
      let close_handler ~code ~reason ~partial_content =
        close_cleanly ~code ~reason ~info:(close_info ?partial_content ()) ws
      in
      let content_reassembler =
        Content_reassembler.create
          ~content_handler
          ~ping_handler
          ~close_handler
          ~protocol_error_handler
          ()
      in
      let%bind reader_result =
        let frame_reader =
          let frame_handler
                ~(opcode : Opcode.t)
                ~(final : bool)
                ~(content : (read, Iobuf.no_seek) Iobuf.t)
                ~masked:
                _
            =
            if not (Bus.is_closed read_opcode_bus) then Bus.write read_opcode_bus opcode;
            Content_reassembler.process_frame content_reassembler ~opcode ~final ~content
          in
          Frame_reader.create ~frame_handler
        in
        Reader.read_one_iobuf_at_a_time ws.reader ~handle_chunk:(fun iobuf ->
          (match Frame_reader.consume_all_available_frames frame_reader iobuf with
           | `Consumed_as_much_as_possible | `Consumed_until_incomplete_frame -> `Continue
           | `Cannot_parse_uint64_length -> `Stop `Cannot_parse_uint64_length)
          |> return)
      in
      (match reader_result with
       | `Eof ->
         if Ivar.is_empty ws.closed
         then
           close_cleanly
             ~code:Protocol_error
             ~reason:"Pipe close unexpectedly without a 'Close' websocket frame"
             ~info:
               (close_info
                  ?partial_content:
                    (Content_reassembler.partial_content_string content_reassembler)
                  ())
             ws
       | `Eof_with_unconsumed_data unconsumed_data ->
         if Ivar.is_empty ws.closed
         then
           close_cleanly
             ~code:Protocol_error
             ~reason:"Pipe close with an incomplete websocket frame"
             ~info:
               (close_info
                  ?partial_content:
                    (Content_reassembler.partial_content_string content_reassembler)
                  ~unconsumed_data
                  ())
             ws
       | `Stopped `Cannot_parse_uint64_length ->
         if Ivar.is_empty ws.closed
         then
           close_cleanly
             ~code:Cannot_accept_data
             ~reason:"Frame with uint64 length that could not be parsed"
             ~info:
               (close_info
                  ?partial_content:
                    (Content_reassembler.partial_content_string content_reassembler)
                  ())
             ws);
      return ())
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
      close_cleanly
        ~code:Connection_close_reason.Normal_closure
        ~reason:"Pipe was closed"
        ~info:None
        ws);
    to_client_w
  ;;
end

let magic_string = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let sec_websocket_accept_header_value ~sec_websocket_key =
  let module C = Crypto.Cryptokit in
  C.transform_string
    (C.Base64.encode_compact ())
    (C.hash_string (C.Hash.sha1 ()) (sec_websocket_key ^ magic_string))
  (* cryptokit leaves out a trailing equals sign *)
  ^ "="
;;

let close
      ~code
      ~reason
      ~masked
      { raw = ws; pipes = pipe_reader, pipe_writer; read_opcode_bus = _; masked = _ }
  =
  Frame.write_frame ws.writer ~masked (Frame.create_close ~code reason);
  (* Wait for the writer to be flushed before actually closing it,
     otherwise the closing frame won't be sent. *)
  let%bind () = Writer.flushed ws.writer in
  let%bind () = Writer.close ws.writer in
  let%bind () = Reader.close ws.reader in
  Pipe.close_read pipe_reader;
  Pipe.close pipe_writer;
  return ()
;;

let close_finished
      { raw = { closed; writer; reader }
      ; pipes = pipe_reader, pipe_writer
      ; read_opcode_bus = _
      ; masked = _
      }
  =
  let%bind res = Ivar.read closed in
  (* Always wait for writer closing before readers due to the way TCP writers work *)
  let%bind () = Writer.close_finished writer in
  let%bind () = Reader.close_finished reader in
  let%bind () = Pipe.closed pipe_reader in
  let%bind () = Pipe.closed pipe_writer in
  return res
;;

let create ?(opcode = `Text) ~(role : Websocket_role.t) reader writer =
  let opcode =
    match opcode with
    | `Text -> Opcode.Text
    | `Binary -> Opcode.Binary
  in
  let masked = Websocket_role.should_mask role in
  let closed = Ivar.create () in
  let ws = { reader; writer; closed } in
  let read_opcode_bus =
    Bus.create_exn
      [%here]
      Bus.Callback_arity.Arity1
      ~on_subscription_after_first_write:Bus.On_subscription_after_first_write.Allow
      ~on_callback_raise:(fun (_ : Error.t) -> ())
  in
  don't_wait_for
    (let%map () = Ivar.read closed |> Deferred.ignore_m in
     Bus.close read_opcode_bus);
  let reader = Pipes.recv_pipe ~read_opcode_bus ~masked ws in
  let writer = Pipes.send_pipe ~opcode ~masked ws in
  let when_reader_closes__close_t =
    let%map () = Pipe.closed reader in
    close_cleanly
      ~code:Connection_close_reason.Normal_closure
      ~reason:"Pipe was closed"
      ~info:None
      ws
  in
  let when_closed__close_everything t =
    let%bind code, reason, _info = Ivar.read closed in
    close ~code:(Connection_close_reason.to_int code) ~reason ~masked t
  in
  don't_wait_for when_reader_closes__close_t;
  let t = { pipes = reader, writer; raw = ws; read_opcode_bus; masked } in
  don't_wait_for (when_closed__close_everything t);
  t
;;

let monitor_pongs
      ?(time_source = Time_source.wall_clock ())
      ~ping_every
      ~concerning_pong_response_delay
      ~on_concerning_pong_response_delay
      t
  =
  let pong_received = Bvar.create () in
  let (_ : (Opcode.t -> unit) Bus.Subscriber.t) =
    Bus.subscribe_exn t.read_opcode_bus [%here] ~f:(function
      | Pong -> Bvar.broadcast pong_received ()
      | Continuation | Text | Binary | Close | Ping | Ctrl _ | Nonctrl _ -> ())
  in
  don't_wait_for
    (Deferred.repeat_until_finished () (fun () ->
       Deferred.choose
         [ Deferred.choice (Bvar.wait pong_received) (fun () -> `Repeat ())
         ; Deferred.choice
             (Time_source.after time_source concerning_pong_response_delay)
             (fun () ->
                on_concerning_pong_response_delay ();
                `Repeat ())
         ; Deferred.choice (Ivar.read t.raw.closed) (fun (_, _, _) -> `Finished ())
         ]));
  Time_source.every time_source ping_every (fun () -> send_ping t "")
;;

let%expect_test "partial frame handling" =
  let write_frames frames =
    let fname = "frame.txt" in
    let%bind writer = Writer.open_file ~append:false fname in
    let () = List.iter ~f:(Frame.write_frame ~masked:false writer) frames in
    let%bind () = Writer.close writer in
    let%bind contents = Reader.file_contents fname in
    let%bind () = Unix.unlink fname in
    return contents
  in
  let read_partial ~len s =
    let fname = sprintf "content-%d" len in
    let%bind () = Writer.save fname ~contents:(String.sub s ~pos:0 ~len) in
    let%bind reader = Reader.open_file fname
    and writer = Writer.open_file "/dev/null" in
    let ws = create ~role:Server reader writer in
    let r, _w = pipes ws in
    let%bind q = Pipe.read_all r in
    let%bind code, reason, info = close_finished ws in
    print_s
      [%sexp
        { input_size = (len : int)
        ; content_read = (q : string Queue.t)
        ; close_code = (code : Connection_close_reason.t)
        ; close_reason = (reason : string)
        ; other_info = (info : (Info.t option[@sexp.option]))
        }];
    let%bind () = Reader.close reader
    and () = Writer.close writer in
    Unix.unlink fname
  in
  let print_all_partials frames =
    let%bind contents = write_frames frames in
    print_s [%sexp "full_contents", (contents : string)];
    let rec loop len =
      if len >= String.length contents
      then Deferred.unit
      else (
        let%bind () = read_partial contents ~len in
        loop (len + 1))
    in
    loop 0
  in
  let print_partial ~len frames =
    let%bind contents = write_frames frames in
    print_s [%sexp "full_contents", (contents : string)];
    read_partial contents ~len
  in
  let print_frames frames =
    let%bind contents = write_frames frames in
    print_s [%sexp "full_contents", (contents : string)];
    read_partial contents ~len:(String.length contents)
  in
  let text_frame ?final txt = Frame.create ?final ~opcode:Text txt in
  let continuation_frame ?final txt = Frame.create ?final ~opcode:Continuation txt in
  let close_frame txt = Frame.create_close ~code:2 txt in
  let%bind () = print_all_partials [ text_frame "hello"; close_frame "reason" ] in
  [%expect
    {|
    (full_contents "\129\005hello\136\b\000\002reason")
    ((input_size 0) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close unexpectedly without a 'Close' websocket frame"))
    ((input_size 1) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\129"))))
    ((input_size 2) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\129\005"))))
    ((input_size 3) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\129\005h"))))
    ((input_size 4) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\129\005he"))))
    ((input_size 5) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\129\005hel"))))
    ((input_size 6) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\129\005hell"))))
    ((input_size 7) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close unexpectedly without a 'Close' websocket frame"))
    ((input_size 8) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136"))))
    ((input_size 9) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b"))))
    ((input_size 10) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b\000"))))
    ((input_size 11) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b\000\002"))))
    ((input_size 12) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b\000\002r"))))
    ((input_size 13) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b\000\002re"))))
    ((input_size 14) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b\000\002rea"))))
    ((input_size 15) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b\000\002reas"))))
    ((input_size 16) (content_read (hello)) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((unconsumed_data "\136\b\000\002reaso")))) |}];
  let%bind () = print_frames [ text_frame "hello"; close_frame "reason" ] in
  [%expect
    {|
    (full_contents "\129\005hello\136\b\000\002reason")
    ((input_size 17) (content_read (hello)) (close_code (Unknown 2))
     (close_reason reason)) |}];
  let%bind () = print_frames [ text_frame "hello"; text_frame "hello" ] in
  [%expect
    {|
    (full_contents "\129\005hello\129\005hello")
    ((input_size 14) (content_read (hello hello)) (close_code Protocol_error)
     (close_reason "Pipe close unexpectedly without a 'Close' websocket frame")) |}];
  let%bind () =
    print_frames
      [ text_frame ~final:false "hel"; continuation_frame "lo"; close_frame "reason" ]
  in
  [%expect
    {|
    (full_contents "\001\003hel\128\002lo\136\b\000\002reason")
    ((input_size 19) (content_read (hello)) (close_code (Unknown 2))
     (close_reason reason)) |}];
  let%bind () = print_frames [ text_frame ~final:false "hello"; text_frame "bye" ] in
  [%expect
    {|
    (full_contents "\001\005hello\129\003bye")
    ((input_size 12) (content_read ()) (close_code Protocol_error)
     (close_reason
      "Bad frame in the middle of a fragmented message: Expecting control or continuation frame")
     (other_info
      ((frame ((opcode Text) (final true) (content bye)))
       (partial_content hello)))) |}];
  let%bind () =
    print_partial ~len:8 [ text_frame ~final:false "hello"; text_frame "bye" ]
  in
  [%expect
    {|
    (full_contents "\001\005hello\129\003bye")
    ((input_size 8) (content_read ()) (close_code Protocol_error)
     (close_reason "Pipe close with an incomplete websocket frame")
     (other_info ((partial_content hello) (unconsumed_data "\129")))) |}];
  let%bind () = print_frames [ continuation_frame "hello" ] in
  [%expect
    {|
    (full_contents "\128\005hello")
    ((input_size 7) (content_read ()) (close_code Protocol_error)
     (close_reason
      "Received continuation message without a previous non-control frame to continue.")
     (other_info ((frame ((opcode Continuation) (final true) (content hello)))))) |}];
  Deferred.unit
;;
