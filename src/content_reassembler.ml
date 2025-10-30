open Core

type t =
  { mutable partial_content : (read_write, Iobuf.seek, Iobuf.global) Iobuf.t
  ; mutable partial_opcode : Opcode.t
  (** [partial_content] and [partial_opcode] together tracks partial messages. Whenever
      [partial_content] has content in its buffer, [partial_opcode] indicates the type of
      the message in the buffer.

      Otherwise the value of [partial_opcode] is set to [Continuation]. It's invalid for a
      frame opcode and we use that to indicate [None] to avoid allocation. *)
  ; content_handler :
      content:(read, Iobuf.no_seek, Iobuf.global) Iobuf.t -> opcode:Opcode.t -> unit
  ; ping_handler : content:(read, Iobuf.no_seek, Iobuf.global) Iobuf.t -> unit
  ; close_handler :
      code:Connection_close_reason.t
      -> reason:string
      -> partial_content:string option
      -> unit
  ; protocol_error_handler :
      reason:string -> partial_content:string option -> frame:Frame.t option -> unit
  }

let partial_content_status t =
  match Iobuf.length_lo t.partial_content = 0 with
  | false -> `Has_partial_content
  | true -> `No_partial_content
;;

let partial_content_string t =
  match partial_content_status t with
  | `No_partial_content -> None
  | `Has_partial_content ->
    Iobuf.flip_lo t.partial_content;
    let partial_content = Iobuf.to_string t.partial_content in
    Iobuf.flip_hi t.partial_content;
    Some partial_content
;;

let clear_partial_content t = Iobuf.reset t.partial_content

let finalise_content t =
  let partial_content = partial_content_string t in
  clear_partial_content t;
  partial_content
;;

let create
  ~content_handler
  ~ping_handler
  ~close_handler
  ~protocol_error_handler
  ?(initial_buffer_size = 8192)
  ()
  =
  let partial_content = Iobuf.create ~len:initial_buffer_size in
  { content_handler
  ; partial_content
  ; partial_opcode = Continuation
  ; ping_handler
  ; close_handler
  ; protocol_error_handler
  }
;;

let append_content t (content : (read, Iobuf.no_seek, Iobuf.global) Iobuf.t) =
  let content_len = Iobuf.length content in
  let available_len = Iobuf.length t.partial_content in
  let has_enough_space = available_len >= content_len in
  if not has_enough_space
  then (
    let old_capacity = Iobuf.capacity t.partial_content in
    let new_len = 2 * (old_capacity + content_len) in
    let new_iobuf = Iobuf.create ~len:new_len in
    Iobuf.flip_lo t.partial_content;
    Iobuf.Blit_consume_and_fill.blito ~src:t.partial_content ~dst:new_iobuf ();
    t.partial_content <- new_iobuf);
  Iobuf.Blit_fill.blito ~src:content ~dst:t.partial_content ()
;;

let process_frame
  t
  ~(opcode : Opcode.t)
  ~(final : bool)
  ~(content : (read, Iobuf.no_seek, Iobuf.global) Iobuf.t)
  =
  match opcode with
  | Close ->
    let content = Iobuf.sub_shared__local content in
    let code =
      if Iobuf.length content >= 2
      then Connection_close_reason.of_int (Iobuf.Consume.int16_be content)
      else Unknown 0
    in
    let reason = Iobuf.Consume.stringo content in
    t.close_handler ~code ~reason ~partial_content:None
  | Ping -> t.ping_handler ~content
  | Pong | Ctrl (_ : int) -> ()
  | Text | Binary | Nonctrl (_ : int) ->
    (match partial_content_status t, final with
     | `No_partial_content, true -> t.content_handler ~content ~opcode
     | `No_partial_content, false ->
       t.partial_opcode <- opcode;
       append_content t content
     | `Has_partial_content, (true | false) ->
       t.protocol_error_handler
         ~reason:
           "Bad frame in the middle of a fragmented message: Expecting control or \
            continuation frame"
         ~partial_content:(finalise_content t)
         ~frame:(Some { opcode; final; content = Iobuf.to_string content }))
  | Continuation ->
    (match partial_content_status t, final with
     | `No_partial_content, (true | false) ->
       t.protocol_error_handler
         ~reason:
           "Received continuation message without a previous non-control frame to \
            continue."
         ~partial_content:None
         ~frame:(Some { opcode; final; content = Iobuf.to_string content })
     | `Has_partial_content, false ->
       append_content t (Iobuf.no_seek__local content) [@nontail]
     | `Has_partial_content, true ->
       append_content t (Iobuf.no_seek__local content);
       Iobuf.flip_lo t.partial_content;
       (match t.partial_opcode with
        | Continuation -> failwith "Unexpected partial_opcode is None"
        | opcode ->
          t.content_handler
            ~content:(Iobuf.read_only (Iobuf.no_seek t.partial_content))
            ~opcode;
          t.partial_opcode <- Continuation;
          clear_partial_content t))
;;
