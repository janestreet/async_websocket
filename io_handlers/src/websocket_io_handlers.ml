open Core
open Netkit
open Websocket

(* https://www.rfc-editor.org/rfc/rfc6455#section-5.5

   "All control frames MUST have a payload length of 125 bytes or less
   and MUST NOT be fragmented."
*)
let max_control_frame_payload_len = 125

module Pending_control_frame : sig
  (** A pre-allocated buffer into which we write the payload for a control frame. We keep
      track of whether a write has been enqueued into the buffer and is thus "pending",
      awaiting its time to be sent on the network. Note that we only ever write a single
      frame to the buffer, so we cannot queue up multiple pings in a row (for instance).
  *)
  type t

  val create : unit -> t
  val is_pending : t -> bool
  val clear_is_pending : t -> unit
  val payload : t -> (read_write, Iobuf.seek) Iobuf.t
  val prepare_write_even_if_already_pending : t -> (read_write, Iobuf.seek) Iobuf.t
  val finish_write : t -> unit
end = struct
  type t =
    { mutable is_pending : bool
    ; payload : (read_write, Iobuf.seek) Iobuf.t
    }
  [@@deriving fields]

  let create () =
    let payload = Iobuf.create ~len:max_control_frame_payload_len in
    { is_pending = false; payload }
  ;;

  let clear_is_pending t = t.is_pending <- false

  let prepare_write_even_if_already_pending t =
    Iobuf.reset t.payload;
    t.payload
  ;;

  let finish_write t =
    Iobuf.flip_lo t.payload;
    t.is_pending <- true
  ;;
end

type close_handler = code:Connection_close_reason.t -> reason:string -> unit

module Close_state : sig
  type t
  type send_close = code:Connection_close_reason.t -> reason:string -> unit -> unit

  val create : close_handler:close_handler -> send_close:send_close -> t
  val on_close_frame_received : t -> payload:(read, Iobuf.no_seek) Iobuf.t -> unit
  val on_closing_with_error : t -> code:Connection_close_reason.t -> reason:string -> unit
  val on_close_frame_sent : t -> unit
  val on_endpoint_close_finished : t -> unit
  val may_send_frame : t -> opcode:Opcode.t -> bool
  val may_read_frame : t -> bool
end = struct
  type phase =
    | Not_closed
    | Close_frame_received
    | Close_frame_sent
    | Closing_with_error
    | Closed
  [@@deriving sexp_of]

  type send_close = code:Connection_close_reason.t -> reason:string -> unit -> unit

  type t =
    { mutable phase : phase
    ; mutable code : Connection_close_reason.t
    ; mutable reason : string
    ; send_close : send_close
    ; close_handler : close_handler
    }

  let create ~close_handler ~send_close =
    (* Initialize [code] and [reason] with their default values (rfc6455):

       "If this Close control frame contains no status code, _The WebSocket
       Connection Close Code_ is considered to be 1005."

       "_The WebSocket Connection Close Reason_ is defined as
       the UTF-8-encoded data following the status code (Section 7.4)
       contained in the first Close control frame received by the
       application implementing this protocol.  If there is no such data in
       the Close control frame, _The WebSocket Connection Close Reason_ is
       the empty string."
    *)
    let code = Connection_close_reason.No_status_code in
    let reason = "" in
    { phase = Not_closed; code; reason; send_close; close_handler }
  ;;

  let on_closing_with_error t ~code ~reason =
    match t.phase with
    | Closing_with_error | Closed -> ()
    | Close_frame_sent ->
      t.phase <- Closed;
      t.close_handler ~code ~reason
    | Not_closed | Close_frame_received ->
      t.phase <- Closing_with_error;
      t.code <- code;
      t.reason <- reason;
      t.send_close ~code ~reason ()
  ;;

  let on_close_frame_received t ~payload =
    let maybe_set_code_and_reason_from_payload t payload =
      if Iobuf.length payload >= 2
      then (
        t.code <- Connection_close_reason.of_int (Iobuf.Peek.int16_be payload ~pos:0);
        t.reason <- Iobuf.Peek.stringo ~len:(Iobuf.length payload - 2) payload ~pos:2)
      else (
        t.code <- No_status_code;
        t.reason <- "")
    in
    match t.phase with
    | Not_closed ->
      t.phase <- Close_frame_received;
      maybe_set_code_and_reason_from_payload t payload;
      (* Don't call [close_handler] here because that's going to happen after it sends the
         close frame. Typical [close_handler]s will close the endpoint, so if we called it
         here, we'd never get a chance to send our close frame response.

         "After both sending and receiving a Close message, an endpoint
         considers the WebSocket connection closed and MUST close the
         underlying TCP connection."
      *)
      t.send_close ~code:t.code ~reason:t.reason ()
    | Close_frame_received ->
      (* We should never get here because we stop reading frames once we hit the
         first close frame. *)
      ()
    | Close_frame_sent ->
      t.phase <- Closed;
      maybe_set_code_and_reason_from_payload t payload;
      t.close_handler ~code:t.code ~reason:t.reason
    | Closing_with_error | Closed -> t.phase <- Closed
  ;;

  let on_close_frame_sent t =
    match t.phase with
    | Not_closed | Close_frame_sent -> t.phase <- Close_frame_sent
    | Closing_with_error | Close_frame_received ->
      t.phase <- Closed;
      t.close_handler ~code:t.code ~reason:t.reason
    | Closed -> t.phase <- Closed
  ;;

  let on_endpoint_close_finished t =
    (match t.phase with
     | Closing_with_error | Closed -> ()
     | Close_frame_received ->
       (* It may be incorrect for the other end to close the connection
          before we can send a close frame.

          But at least the fact that we *received* their close frame should imply
          that we received all the data they had meant send. *)
       ()
     | Close_frame_sent | Not_closed ->
       (* "If _The WebSocket Connection is Closed_ and no Close control frame was received
          by the endpoint (such as could occur if the underlying transport connection
          is lost), _The WebSocket Connection Close Code_ is considered to be 1006." *)
       t.close_handler
         ~code:Closed_abnormally
         ~reason:
           "Endpoint closed unexpectedly without a receiving a 'Close' websocket frame");
    t.phase <- Closed
  ;;

  let may_send_frame t ~opcode =
    match t.phase with
    | Not_closed -> true
    | Closing_with_error | Close_frame_received ->
      (* Responding with a Close frame should be ok, but nothing else  *)
      [%equal: Opcode.t] opcode Close
    | Close_frame_sent | Closed ->
      (* "The application MUST NOT send any more data frames after sending a
         Close frame." *)
      false
  ;;

  let may_read_frame t =
    match t.phase with
    | Not_closed | Close_frame_sent -> true
    | Closing_with_error | Close_frame_received | Closed -> false
  ;;
end

type finish_read_result =
  | Call_app_on_readable
  | Dont_call_app_on_readable

module Byte_stream_reader = struct
  type t = { app_recv_window : Recv_window.For_network.t }

  let create ~recv_window_size =
    let app_recv_window =
      Recv_window.For_network.create
        ~size:recv_window_size
        ~max_bytes_to_advertise:recv_window_size
        ()
    in
    { app_recv_window }
  ;;

  let is_app_ready_for_more_data t =
    Recv_window.bytes_to_advertise (t.app_recv_window :> Recv_window.t) > 0
  ;;

  type state_before_read = int

  let prep_to_read t = Recv_window.bytes_available (t.app_recv_window :> Recv_window.t)

  let finish_read t ~state_before_read:avail_before =
    let avail_after_read =
      Recv_window.bytes_available (t.app_recv_window :> Recv_window.t)
    in
    if avail_after_read > avail_before
    then Call_app_on_readable
    else Dont_call_app_on_readable
  ;;

  let handle_data_frame_payload t ~payload_fragment =
    let len =
      min
        (Iobuf.length payload_fragment)
        (Recv_window.bytes_to_advertise (t.app_recv_window :> Recv_window.t))
    in
    let dst = Recv_window.For_network.prepare_write t.app_recv_window ~len in
    ignore (Iobuf.Blit_consume_and_fill.blit_maximal ~src:payload_fragment ~dst : int);
    Recv_window.For_network.finish_write t.app_recv_window
  ;;
end

module Message_stream_reader = struct
  type reading_phase =
    [ `App_consuming_message
    | `Waiting_for_complete_message
    ]

  type t =
    { reassembled_message_buf : (read_write, Iobuf.seek) Iobuf.t
    ; recv_message : (read_write, Iobuf.seek) Iobuf.t
    ; mutable reading_phase : reading_phase
    }

  let create ~recv_window_size =
    let reassembled_message_buf = Iobuf.create ~len:recv_window_size in
    let recv_message = Iobuf.sub_shared reassembled_message_buf in
    Iobuf.resize recv_message ~len:0;
    { reassembled_message_buf
    ; recv_message
    ; reading_phase = `Waiting_for_complete_message
    }
  ;;

  let verify_if_app_consuming_is_done t =
    match t.reading_phase with
    | `Waiting_for_complete_message -> ()
    | `App_consuming_message ->
      (match Iobuf.length t.recv_message = 0 with
       | true ->
         Iobuf.reset t.reassembled_message_buf;
         t.reading_phase <- `Waiting_for_complete_message
       | false -> ())
  ;;

  let is_app_ready_for_more_data t =
    verify_if_app_consuming_is_done t;
    match t.reading_phase with
    | `App_consuming_message ->
      (* In the [Message_stream] mode, only one message is served at a time.
         So don't read anything until the app is done consuming the previous message. *)
      false
    | `Waiting_for_complete_message -> true
  ;;

  type state_before_read = reading_phase

  let prep_to_read t =
    verify_if_app_consuming_is_done t;
    t.reading_phase
  ;;

  let finish_read t ~(state_before_read : state_before_read) =
    match state_before_read, t.reading_phase with
    | `Waiting_for_complete_message, `App_consuming_message -> Call_app_on_readable
    | `App_consuming_message, _
    | `Waiting_for_complete_message, `Waiting_for_complete_message ->
      Dont_call_app_on_readable
  ;;

  let handle_data_frame_payload
        t
        ~payload_fragment
        ~final
        ~total_frame_payload_len
        ~payload_pos
    =
    match t.reading_phase with
    | `App_consuming_message ->
      (* Should never reach here because we shouldn't be reading more data frames until
         the current message was consumed. *)
      ()
    | `Waiting_for_complete_message ->
      (* If we are receiving a fragmented message, or a large frame that is consumed
         piecemeal, copy the payload_fragment to a buffer to reassemble the message. *)
      (match payload_pos = 0 with
       | true ->
         Iobuf.resize t.reassembled_message_buf ~len:0;
         (* When we receive the *start* of a frame, make sure there's space for the
            payload for the entire frame. *)
         Iobuf.flip_lo t.reassembled_message_buf;
         (match
            Netkit.Ensure_space.ensure_space
              t.reassembled_message_buf
              total_frame_payload_len
          with
          | Ok | Grew -> ());
         Iobuf.flip_hi t.reassembled_message_buf
       | false ->
         (* We get here when receiving a portion of a frame other than the start, when
            the frame itself got fragmented.
            No need to ensure_space here because that was already ensured earlier. *)
         ());
      let copied_len =
        Iobuf.Blit_consume_and_fill.blit_maximal
          ~src:payload_fragment
          ~dst:t.reassembled_message_buf
      in
      let copied_end_of_frame = payload_pos + copied_len = total_frame_payload_len in
      let reassembled_message_buf_contains_whole_message = final && copied_end_of_frame in
      (match reassembled_message_buf_contains_whole_message with
       | true ->
         Iobuf.flip_lo t.reassembled_message_buf;
         Iobuf.set_bounds_and_buffer ~src:t.reassembled_message_buf ~dst:t.recv_message;
         t.reading_phase <- `App_consuming_message
       | false -> t.reading_phase <- `Waiting_for_complete_message)
  ;;
end

(* These type aliases are used both for tagging what kind of read mode we're in, as well
   as for capturing some state during the read loop where we need a different type between
   the two readers
*)
type byte_stream = Byte_stream_reader.state_before_read
type message_stream = Message_stream_reader.state_before_read

module Read_mode = struct
  type 'a t =
    | Byte_stream : Byte_stream_reader.t -> byte_stream t
    | Message_stream : Message_stream_reader.t -> message_stream t
end

type 'read_mode t =
  { net_recv_window : Recv_window.t
  ; net_send_window : Send_window.t
  ; app_send_window : Send_window.For_network.t
  ; read_mode : 'read_mode Read_mode.t
  ; pending_ping : Pending_control_frame.t
  ; pending_pong : Pending_control_frame.t
  ; pending_close : Pending_control_frame.t
  ; incoming_control_payload : (read_write, Iobuf.seek) Iobuf.t
  ; mutable close_state : Close_state.t
  ; mutable close_frame_queued : bool
  ; mutable app_handlers : Io_handlers.t option
  ; mutable receiving_fragmented_message : bool
  ; mutable sending_fragmented_message : bool
  ; role : [ `Client of Random.State.t | `Server ]
  ; outgoing_message_format : [ `Text | `Binary ]
  ; frame_reader : Frame.Frame_reader.Expert.t
  ; frame_writer : Frame.Iobuf_writer.t
  ; frame_received_rw : (Websocket.Opcode.t -> unit) Bus.Read_write.t
  ; mutable inside_on_readable : bool
  ; mutable on_readable_scheduled : bool
  }
[@@deriving fields]

let frame_received t = Bus.read_only t.frame_received_rw

module Writer = struct
  type write_frame_result =
    | Written
    | Not_sent_because_lacked_send_window_space
    | Not_sent_because_not_allowed

  let maybe_write_frame t ~src ~dst ~opcode =
    match Close_state.may_send_frame t.close_state ~opcode with
    | false -> Not_sent_because_not_allowed
    | true ->
      let capacity_of_frame_bytes_to_send = Iobuf.length dst in
      let desired_payload_bytes_to_send = Iobuf.length src in
      let capacity_of_payload_bytes_to_send =
        Frame.Iobuf_writer.max_content_bytes
          t.frame_writer
          ~max_frame_bytes:capacity_of_frame_bytes_to_send
      in
      let feasible_payload_bytes_to_send =
        min capacity_of_payload_bytes_to_send desired_payload_bytes_to_send
      in
      let can_send_all_payload =
        desired_payload_bytes_to_send = feasible_payload_bytes_to_send
      in
      (match Opcode.to_kind opcode with
       | Control ->
         (match can_send_all_payload with
          | false ->
            (* Control frames may not be fragmented *)
            Not_sent_because_lacked_send_window_space
          | true ->
            let payload_window =
              Frame.Iobuf_writer.start_write
                t.frame_writer
                dst
                ~opcode
                ~final:true
                ~content_len:feasible_payload_bytes_to_send
            in
            Iobuf.Blit_consume_and_fill.blit
              ~src
              ~dst:payload_window
              ~len:feasible_payload_bytes_to_send;
            Frame.Iobuf_writer.finish_write_exn t.frame_writer;
            Written)
       | Non_control ->
         (* In a worst (pathological) case, this will send 1 byte of payload per frame.
            This prioritizes the latency of the [feasible_payload_bytes_to_send] over
            throughput. *)
         (match feasible_payload_bytes_to_send > 0 with
          | false -> Not_sent_because_lacked_send_window_space
          | true ->
            let final = can_send_all_payload in
            let opcode =
              (* "A fragmented message consists of a single frame with the FIN bit
                 clear and an opcode other than 0, followed by zero or more frames
                 with the FIN bit clear and the opcode set to 0, and terminated by
                 a single frame with the FIN bit set and an opcode of 0." *)
              match t.sending_fragmented_message with
              | false -> opcode
              | true -> Continuation
            in
            let payload_window =
              Frame.Iobuf_writer.start_write
                t.frame_writer
                dst
                ~opcode
                ~final
                ~content_len:feasible_payload_bytes_to_send
            in
            Iobuf.Blit_consume_and_fill.blit
              ~src
              ~dst:payload_window
              ~len:feasible_payload_bytes_to_send;
            Frame.Iobuf_writer.finish_write_exn t.frame_writer;
            t.sending_fragmented_message <- not final;
            Written))
  ;;

  let maybe_write_write_control_frame t ctrl_frame ~dst ~opcode =
    if Pending_control_frame.is_pending ctrl_frame
    then (
      match
        maybe_write_frame t ~src:(Pending_control_frame.payload ctrl_frame) ~dst ~opcode
      with
      | Written ->
        Pending_control_frame.clear_is_pending ctrl_frame;
        if [%equal: Opcode.t] opcode Close then t.close_frame_queued <- true
      | Not_sent_because_not_allowed -> Pending_control_frame.clear_is_pending ctrl_frame
      | Not_sent_because_lacked_send_window_space -> ())
  ;;

  let rec send_loop t =
    let app_send_window = Send_window.For_network.as_iobuf t.app_send_window in
    let dst = Send_window.prepare_write_available t.net_send_window in
    maybe_write_write_control_frame t t.pending_close ~dst ~opcode:Close;
    maybe_write_write_control_frame t t.pending_ping ~dst ~opcode:Ping;
    maybe_write_write_control_frame t t.pending_pong ~dst ~opcode:Pong;
    ignore
      (maybe_write_frame
         t
         ~src:app_send_window
         ~dst
         ~opcode:
           (match t.outgoing_message_format with
            | `Text -> Text
            | `Binary -> Binary)
       : write_frame_result);
    match Send_window.finish_write t.net_send_window ~flush:Yes with
    | Queued | Nothing_to_flush -> ()
    | Closed -> Send_window.For_network.close t.app_send_window
    | Send_window_grew -> send_loop t
  ;;
end

let on_sendable t =
  match t.app_handlers with
  | None -> ()
  | Some io_handlers ->
    let available_before =
      Send_window.bytes_available (t.app_send_window :> Send_window.t)
    in
    Writer.send_loop t;
    let available_after =
      Send_window.bytes_available (t.app_send_window :> Send_window.t)
    in
    if available_after > available_before then io_handlers.on_sendable ();
    if t.close_frame_queued && Send_window.bytes_queued t.net_send_window = 0
    then (
      t.close_frame_queued <- false;
      Close_state.on_close_frame_sent t.close_state)
;;

let prepare_ping t =
  match Close_state.may_send_frame t.close_state ~opcode:Ping with
  | false -> `May_not_send_ping_anymore
  | true ->
    (match Pending_control_frame.is_pending t.pending_ping with
     | true -> `Ping_already_queued
     | false ->
       let iobuf =
         Pending_control_frame.prepare_write_even_if_already_pending t.pending_ping
       in
       `Prepared iobuf)
;;

let finish_ping t =
  Pending_control_frame.finish_write t.pending_ping;
  on_sendable t
;;

let send_close t ~code ~reason =
  match Close_state.may_send_frame t.close_state ~opcode:Close with
  | false -> `May_not_send_close_anymore
  | true ->
    (match Pending_control_frame.is_pending t.pending_close with
     | true -> `Close_already_queued
     | false ->
       let iobuf =
         Pending_control_frame.prepare_write_even_if_already_pending t.pending_close
       in
       Iobuf.Fill.uint16_be_trunc iobuf (Connection_close_reason.to_int code);
       Iobuf.Fill.stringo iobuf reason;
       Pending_control_frame.finish_write t.pending_close;
       on_sendable t;
       `Close_queued)
;;

let close_with_error t ~code ~reason =
  Close_state.on_closing_with_error t.close_state ~code ~reason
;;

module Reader = struct
  let is_app_ready_for_more_data (type a) (t : a t) =
    match t.read_mode with
    | Byte_stream byte_stream_reader ->
      Byte_stream_reader.is_app_ready_for_more_data byte_stream_reader
    | Message_stream message_stream_reader ->
      Message_stream_reader.is_app_ready_for_more_data message_stream_reader
  ;;

  (* [frame_reading_loop] is responsible for transforming the raw datastream into
     a stream of frames.
     It triggers [partial_frame_handler], which is responsible for handling frames. *)
  let rec frame_reading_loop t =
    match is_app_ready_for_more_data t with
    | false -> ()
    | true ->
      let result =
        Frame.Frame_reader.Expert.consume_frame_even_if_incomplete_payload
          t.frame_reader
          (Recv_window.Expert.as_writable_iobuf t.net_recv_window)
      in
      (match result with
       | Consumed_one_frame ->
         (* There may be more than one frame accumulated in the [net_recv_window] *)
         frame_reading_loop t
       | Incomplete_frame | No_frame -> ()
       | Cannot_parse_uint64_length ->
         close_with_error
           t
           ~code:Message_too_large
           ~reason:"Received a frame with uint64 length that could not be parsed")
  ;;

  let prep_read (type state_before_read) (t : state_before_read t) : state_before_read =
    match t.read_mode with
    | Byte_stream byte_stream_reader -> Byte_stream_reader.prep_to_read byte_stream_reader
    | Message_stream message_stream_reader ->
      Message_stream_reader.prep_to_read message_stream_reader
  ;;

  let finish_read
        (type state_before_read)
        (t : state_before_read t)
        ~(state_before_read : state_before_read)
    =
    match t.read_mode with
    | Byte_stream byte_stream_reader ->
      Byte_stream_reader.finish_read byte_stream_reader ~state_before_read
    | Message_stream message_stream_reader ->
      Message_stream_reader.finish_read message_stream_reader ~state_before_read
  ;;

  let on_readable t ~app_on_readable =
    let state_before_read = prep_read t in
    frame_reading_loop t;
    (match finish_read t ~state_before_read with
     | Dont_call_app_on_readable -> ()
     | Call_app_on_readable -> app_on_readable ());
    Recv_window.notify_maybe_bytes_consumed t.net_recv_window
  ;;
end

let on_readable t =
  t.inside_on_readable <- true;
  t.on_readable_scheduled <- false;
  (match t.app_handlers with
   | None -> ()
   | Some io_handlers ->
     (match Close_state.may_read_frame t.close_state with
      | false -> ()
      | true ->
        Reader.on_readable t ~app_on_readable:io_handlers.on_readable;
        (* We might have scheduled a pong/close to be sent, so jump to the writer code. *)
        Writer.send_loop t));
  t.inside_on_readable <- false
;;

(* Partial_frame_handler is responsible for all of the frame handling aspects.

   But notably it delegates the "how data payloads are delivered to the app" to
   the [handle_data_frame_payload] of [Byte_stream_reader] or [Message_stream_reader].
*)
module Partial_frame_handler = struct
  let check_masking t ~masked =
    match masked, t.role with
    | `Payload_was_not_masked, `Server ->
      (* "The server MUST close the connection upon receiving a
         frame that is not masked." *)
      close_with_error
        t
        ~code:Policy_violation
        ~reason:"received a frame from the client that wasn't masked";
      `Mask_error
    | `Payload_was_masked, `Client _ ->
      (* "A client MUST close a connection if it detects a masked
         frame." *)
      close_with_error
        t
        ~code:Policy_violation
        ~reason:"received a frame from the server that was masked";
      `Mask_error
    | `Payload_was_masked, `Server | `Payload_was_not_masked, `Client _ -> `Mask_ok
  ;;

  let check_fragmentation t ~opcode ~final ~total_frame_payload_len =
    match Opcode.to_kind opcode with
    | Control ->
      (* "All control frames MUST have a payload length of 125 bytes or less
         and MUST NOT be fragmented." *)
      (match final && total_frame_payload_len <= max_control_frame_payload_len with
       | false ->
         close_with_error t ~code:Protocol_error ~reason:"invalid control frame";
         `Fragmentation_error
       | true -> `Fragmentation_ok)
    | Non_control ->
      let is_continuation = [%equal: Opcode.t] opcode Continuation in
      (match is_continuation, t.receiving_fragmented_message with
       | true, false ->
         close_with_error
           t
           ~code:Protocol_error
           ~reason:
             "Received continuation message without a previous non-control frame to \
              continue.";
         `Fragmentation_error
       | false, true ->
         close_with_error
           t
           ~code:Protocol_error
           ~reason:
             "Bad frame in the middle of a fragmented message: Expecting control or \
              continuation frame";
         `Fragmentation_error
       | true, true | false, false -> `Fragmentation_ok)
  ;;

  let handle_payload
        (type a)
        (t : a t)
        ~payload_fragment
        ~(opcode : Opcode.t)
        ~final
        ~total_frame_payload_len
        ~payload_pos
    =
    Bus.write t.frame_received_rw opcode;
    match opcode with
    | Ping ->
      (* It's okay to discard any pending pong, per RFC:

         "If an endpoint receives a Ping frame and has not yet sent Pong
         frame(s) in response to previous Ping frame(s), the endpoint MAY
         elect to send a Pong frame for only the most recently processed Ping
         frame."
      *)
      let iobuf =
        Pending_control_frame.prepare_write_even_if_already_pending t.pending_pong
      in
      ignore
        (Iobuf.Blit_consume_and_fill.blit_maximal ~src:payload_fragment ~dst:iobuf : int);
      Pending_control_frame.finish_write t.pending_pong
    | Ctrl _ -> close_with_error t ~code:Protocol_error ~reason:"Reserved control opcode"
    | Nonctrl _ ->
      close_with_error t ~code:Protocol_error ~reason:"Reserved non-control opcode"
    | Pong ->
      Iobuf.advance payload_fragment (Iobuf.length payload_fragment)
    | Close ->
      Close_state.on_close_frame_received
        t.close_state
        ~payload:(Iobuf.no_seek payload_fragment);
      Iobuf.advance payload_fragment (Iobuf.length payload_fragment)
    | Continuation | Text | Binary ->
      (* Finally, handle the data payload for the app. *)
      (match t.read_mode with
       | Byte_stream byte_stream_reader ->
         Byte_stream_reader.handle_data_frame_payload byte_stream_reader ~payload_fragment
       | Message_stream message_stream_reader ->
         Message_stream_reader.handle_data_frame_payload
           message_stream_reader
           ~payload_fragment
           ~final
           ~total_frame_payload_len
           ~payload_pos);
      t.receiving_fragmented_message <- not final
  ;;

  let partial_frame_handler
        t
        ~(opcode : Opcode.t)
        ~final
        ~total_frame_payload_len
        ~payload_pos
        ~payload_fragment
        ~masked
    =
    (* A _message_ can be fragmented into frames.
       A _frame_ can be consumed piecemeal (e.g. if only a portion of the frame has arrived).

       Here we may be receiving either a full or an incomplete frame.

       - An incomplete frame has only a portion of the payload of the full frame (i.e.
         [Iobuf.length payload <> total_frame_payload_len]).

       - An incomplete frame will appear here more than once, but with differents
         [payload_pos] and [payload] st. Sum {Iobuf.length payload} = [total_frame_payload_len].

       - The {full,incomplete} frame may be part of a fragmented message.
    *)
    let t = Lazy.force t in
    match check_masking t ~masked with
    | `Mask_error -> ()
    | `Mask_ok ->
      let frame_kind = Opcode.to_kind opcode in
      (match check_fragmentation t ~opcode ~final ~total_frame_payload_len with
       | `Fragmentation_error -> ()
       | `Fragmentation_ok ->
         (match frame_kind with
          | Control ->
            let is_frame_complete =
              total_frame_payload_len = Iobuf.length payload_fragment
            in
            (match is_frame_complete with
             | true ->
               handle_payload
                 t
                 ~payload_fragment
                 ~opcode
                 ~final
                 ~total_frame_payload_len
                 ~payload_pos
             | false ->
               ignore
                 (Iobuf.Blit_consume_and_fill.blit_maximal
                    ~src:payload_fragment
                    ~dst:t.incoming_control_payload
                  : int);
               if total_frame_payload_len = Iobuf.length_lo t.incoming_control_payload
               then (
                 Iobuf.flip_lo t.incoming_control_payload;
                 handle_payload
                   t
                   ~payload_fragment:(Iobuf.read_only t.incoming_control_payload)
                   ~opcode
                   ~final
                   ~total_frame_payload_len
                   ~payload_pos;
                 Iobuf.reset t.incoming_control_payload))
          | Non_control ->
            handle_payload
              t
              ~payload_fragment
              ~opcode
              ~final
              ~total_frame_payload_len
              ~payload_pos))
  ;;
end

let create
      ?(send_window_size = 32 * 1024)
      ?(outgoing_message_format = `Text)
      net_recv_window
      net_send_window
      ~read_mode
      ~role
      ~close_handler
  =
  let app_send_window =
    Send_window.For_network.create
      ~size:send_window_size
      ~congestion_window:send_window_size
      ()
  in
  let frame_writer = Frame.Iobuf_writer.create ~role in
  let frame_received_rw =
    Bus.create_exn
      [%here]
      Bus.Callback_arity.Arity1
      ~on_subscription_after_first_write:Bus.On_subscription_after_first_write.Allow
      ~on_callback_raise:(fun error -> raise_s [%sexp (error : Error.t)])
  in
  let send_close t_lazy ~code ~reason () =
    ignore
      (send_close (Lazy.force t_lazy) ~code ~reason
       : [ `May_not_send_close_anymore | `Close_already_queued | `Close_queued ])
  in
  let rec t =
    lazy
      { net_recv_window
      ; net_send_window
      ; app_send_window
      ; read_mode
      ; pending_ping = Pending_control_frame.create ()
      ; pending_pong = Pending_control_frame.create ()
      ; pending_close = Pending_control_frame.create ()
      ; incoming_control_payload = Iobuf.create ~len:max_control_frame_payload_len
      ; close_state = Close_state.create ~close_handler ~send_close:(send_close t)
      ; close_frame_queued = false
      ; app_handlers = None
      ; receiving_fragmented_message = false
      ; sending_fragmented_message = false
      ; role
      ; outgoing_message_format
      ; frame_reader =
          Frame.Frame_reader.Expert.create
            ~partial_frame_handler:(Partial_frame_handler.partial_frame_handler t)
      ; frame_writer
      ; frame_received_rw
      ; inside_on_readable = false
      ; on_readable_scheduled = false
      }
  in
  Lazy.force t
;;

let maybe_run_or_schedule_on_readable t =
  match t.inside_on_readable with
  | false -> on_readable t
  | true ->
    (match t.on_readable_scheduled with
     | true -> ()
     | false ->
       (* Coalescing to avoid scheduling [on_readable] when it's already scheduled. *)
       t.on_readable_scheduled <- true;
       Async_kernel.schedule (fun () -> on_readable t))
;;

let create_byte_stream
      ?(recv_window_size = 32 * 1024)
      ?(send_window_size = 32 * 1024)
      ?(outgoing_message_format = `Text)
      net_recv_window
      net_send_window
      ~role
      ~close_handler
  =
  let byte_stream_reader = Byte_stream_reader.create ~recv_window_size in
  let t =
    create
      ~send_window_size
      ~outgoing_message_format
      net_recv_window
      net_send_window
      ~read_mode:(Byte_stream byte_stream_reader)
      ~role
      ~close_handler
  in
  Recv_window.For_network.set_maybe_new_room byte_stream_reader.app_recv_window (fun () ->
    if Byte_stream_reader.is_app_ready_for_more_data byte_stream_reader
    then maybe_run_or_schedule_on_readable t);
  t
;;

let create_message_stream
      ?(recv_window_size = 32 * 1024)
      ?(send_window_size = 32 * 1024)
      ?(outgoing_message_format = `Text)
      net_recv_window
      net_send_window
      ~role
      ~close_handler
  =
  create
    ~send_window_size
    ~outgoing_message_format
    net_recv_window
    net_send_window
    ~read_mode:(Message_stream (Message_stream_reader.create ~recv_window_size))
    ~role
    ~close_handler
;;

let recv_message (t : message_stream t) =
  let (Message_stream message_stream) = t.read_mode in
  Iobuf.read_only message_stream.recv_message
;;

let notify_maybe_message_consumed (t : message_stream t) =
  let (Message_stream message_stream) = t.read_mode in
  if Message_stream_reader.is_app_ready_for_more_data message_stream
  then maybe_run_or_schedule_on_readable t
;;

let start t app_handlers =
  t.app_handlers <- Some app_handlers;
  Send_window.For_network.set_flush_callback t.app_send_window (fun () ->
    let available_before =
      Send_window.bytes_available (t.app_send_window :> Send_window.t)
    in
    Writer.send_loop t;
    let available_after =
      Send_window.bytes_available (t.app_send_window :> Send_window.t)
    in
    if available_after > available_before then Send_window_grew else Closed);
  on_readable t;
  on_sendable t
;;

let recv_window (t : byte_stream t) =
  let (Byte_stream byte_stream_reader) = t.read_mode in
  (byte_stream_reader.app_recv_window :> Recv_window.t)
;;

let send_window t = (t.app_send_window :> Send_window.t)
let set_endpoint_close_finished t = Close_state.on_endpoint_close_finished t.close_state

let io_handlers t =
  { Io_handlers.on_readable = (fun () -> on_readable t)
  ; on_sendable = (fun () -> on_sendable t)
  }
;;
