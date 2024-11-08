open Core

(** [Content_reassembler] can process fragmented content from websocket frames and
    reassemble fragmented content into a contiguous buffer.

    It maintains an internal buffer which is reused to avoid allocs (most of the time).
    The internal buffer doubles in size whenever needed to avoid quadratic operations.

    This can be used together with [Frame.Frame_reader] to implement the reader
    side of a websocket connection. *)

type t

val create
  :  content_handler:(local_ (read, Iobuf.no_seek) Iobuf.t -> unit)
       (** [content_handler] is called when text, binary, or non-control content is
      completed. If the content was fragmented, [content_handler] is only
      called on the final frame, when the full, unfragmented contents has been
      collected. *)
  -> ping_handler:(content:local_ (read, Iobuf.no_seek) Iobuf.t -> unit)
       (** [ping_handler] is called whenever a ping message is received. Since ping
      messages cannot be fragmented, [content] corresponds to the content of
      a single ping frame. *)
  -> close_handler:
       (code:Connection_close_reason.t
        -> reason:string
        -> partial_content:string option
        -> unit)
       (** [close_handler] is called whenever a close message is received. Since
      close messages cannot be fragmented, [content] corresponds to the content
      of a single close frame. [partial_content], if it exists, is the
      partially built content of the fragmented message being collected. *)
  -> protocol_error_handler:
       (reason:string -> partial_content:string option -> frame:Frame.t option -> unit)
       (** [protocol_error_handler] is called whenever the websocket protocol is
      violated somehow. [partial_content], if it exists, is the partially built
      content of the fragmented message being collected. [frame] is the frame
      that violated the protocol. *)
  -> ?initial_buffer_size:int
  -> unit
  -> t

val process_frame
  :  t
  -> opcode:Opcode.t
  -> final:bool
  -> content:local_ (read, Iobuf.no_seek) Iobuf.t
  -> unit

(** [partial_content_string] yields the content of the contents of the most
    recent (yet unfinished) message, if there is any content. *)
val partial_content_string : t -> string option
