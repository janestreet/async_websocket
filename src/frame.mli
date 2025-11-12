open! Core
open Async

type t =
  { opcode : Opcode.t
  ; final : bool
  ; content : string
  }
[@@deriving sexp_of, quickcheck, equal]

module Error : sig
  type t =
    { code : Connection_close_reason.t
    ; message : string
    }
  [@@deriving sexp_of]
end

val write_frame : masked:bool -> Writer.t -> t -> unit
val create : opcode:Opcode.t -> ?final:bool -> string -> t
val create_close : code:int -> ?final:bool -> string -> t

(** [frame_bytes] calculates how many bytes a websocket frame would consume. *)
val frame_bytes : content_len:int -> masked:bool -> int

(** [max_content_bytes] calculates the most content that can be contained in the payload
    of a frame with up to [max_frame_bytes]. *)
val max_content_bytes : max_frame_bytes:int -> masked:bool -> int

module Iobuf_writer : sig
  type t

  val create : role:[ `Client of Random.State.t | `Server ] -> t
  val required_frame_bytes : t -> content_len:int -> int
  val max_content_bytes : t -> max_frame_bytes:int -> int

  (** Starts writing a frame to the given iobuf; and returns a "content window" iobuf
      pointing to websocket frame's content.

      Caller must:
      - Provide an iobuf with at least [required_frame_bytes] space.
      - Advance/write exactly [content_len] bytes to the content window.
      - Invoke [finish_write]. *)
  val start_write
    :  t
    -> (read_write, Iobuf.seek, Iobuf.global) Iobuf.t
    -> opcode:Opcode.t
    -> final:bool
    -> content_len:int
    -> (read_write, Iobuf.seek, Iobuf.global) Iobuf.t

  (** Finishes writing a frame to the given iobuf. Raises if the iobuf still has a nonzero
      length. *)
  val finish_write_exn : t -> unit
end

module Frame_reader : sig
  module Read_result : sig
    type t =
      | No_frame
      | Incomplete_frame
      | Consumed_one_frame
      | Cannot_parse_uint64_length
    [@@deriving sexp]
  end

  type t

  val create
    :  frame_handler:
         (opcode:Opcode.t
          -> final:bool
          -> content:(read, Iobuf.no_seek, Iobuf.global) Iobuf.t
          -> masked:[ `Content_was_masked | `Content_was_not_masked ]
               (** [masked] indicates if the received content was masked, which is useful
                   for a websocket server to enforce that clients are masking the content.

                   Unmasking will be applied whenever possible. The handler will always
                   receive unmasked content. *)
          -> unit)
    -> t

  (** The input iobuf will advance by the amount of bytes consumed.

      If the frame's content is masked, [consume_frame] will modify the iobuf inline to
      unmask it. *)
  val consume_frame : t -> (read_write, Iobuf.seek, Iobuf.global) Iobuf.t -> Read_result.t

  (** Useful when we only received a partial frame but still want to know expected the
      size of the full frame. *)
  val expected_frame_bytes : (read, Iobuf.seek, Iobuf.global) Iobuf.t -> int option

  val consume_all_available_frames
    :  t
    -> (read_write, Iobuf.seek, Iobuf.global) Iobuf.t
    -> [ `Consumed_as_much_as_possible
       | `Consumed_until_incomplete_frame
       | `Cannot_parse_uint64_length
       ]

  module Expert : sig
    type t

    type partial_frame_handler =
      opcode:Opcode.t
      -> final:bool
      -> total_frame_payload_len:int
           (** [total_frame_payload_len] indicates the totality of the payload in the
               frame. Only a fraction of which may be available for consumption in
               [payload]. *)
      -> payload_pos:int
           (** Indicates the offset into the totality of the payload where [payload]
               starts *)
      -> payload_fragment:(read, Iobuf.seek, Iobuf.global) Iobuf.t
      -> masked:[ `Payload_was_masked | `Payload_was_not_masked ]
           (** [masked] indicates if the received payload was masked, which is useful for
               a websocket server to enforce that clients are masking the payload.

               Unmasking will be applied whenever possible. The handler will always
               receive unmasked payload. *)
      -> unit

    val create : partial_frame_handler:partial_frame_handler -> t

    (** [consume_frame_even_if_incomplete_payload] is useful for consuming payload data
        when:

        (i) the full frame payload is not available in the input iobuf (e.g. we haven't
        received the full frame yet); and (ii) the caller's ability to consume payload is
        limited (e.g. the downstream app buffer can't be resized)

        It's meant to be invoked repeately on the same iobuf whenever there's more input
        data, or whenever the caller can handle more payload data. *)
    val consume_frame_even_if_incomplete_payload
      :  t
      -> (read_write, Iobuf.seek, Iobuf.global) Iobuf.t
      -> Read_result.t

    val maybe_consume_header
      :  local_ ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t
      -> payload_availability:
           [< `Entire_payload_must_be_available | `Incomplete_payload_ok ]
      -> mask:local_ bytes
      -> f:
           local_ (masked:[> `Mask | `No_mask_needed ]
                   -> final:bool
                   -> opcode:Opcode.t
                   -> payload_length:int
                   -> unit)
      -> [> `Cannot_parse_uint64_length
         | `Consumed_header
         | `Incomplete_frame
         | `Incomplete_frame_header
         | `No_frame_header
         ]

    val get_payload_length
      :  local_ ([> read ], Iobuf.seek, Iobuf.global) Iobuf.t
      -> local_ [ `Cannot_parse_uint64_length
                | `Incomplete_frame_header
                | `No_frame_header
                | `Payload_length of int
                ]
  end
end
