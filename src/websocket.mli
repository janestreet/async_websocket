open! Core
open Async
module Opcode = Opcode
module Connection_close_reason = Connection_close_reason
module Frame_reader = Frame.Frame_reader
module Frame = Frame
module Iobuf_writer = Frame.Iobuf_writer
module Content_reassembler = Content_reassembler

type 'msg t

val sec_websocket_accept_header_value : sec_websocket_key:string -> string

module Websocket_role : sig
  type t =
    | Client
    | Server
  [@@deriving sexp_of]
end

(** [create ?opcode ~role reader writer] creates a websocket connection.

    The websocket accepts any type of incoming frame and only writes frames of type
    [opcode]. *)
val create
  :  ?opcode:[ `Text | `Binary ]
  -> role:Websocket_role.t
  -> Reader.t
  -> Writer.t
  -> string t

(** [Frame_content.t] is used to provide more flexible types of pipes. See [create']. *)
module Frame_content : sig
  module Opcode : sig
    type t =
      | Text
      | Binary
      | Nonctrl of int
    [@@deriving sexp_of]
  end

  type t =
    { opcode : Opcode.t
    ; content : string
    }
  [@@deriving sexp_of]
end

(** Similar to [create], but allows receiving and sending different types of websocket
    frames. *)
val create' : role:Websocket_role.t -> Reader.t -> Writer.t -> Frame_content.t t

(** Gets a pair of ['msg Pipe.{Reader,Writer}.t] that can be used to receieve and send
    websocket frames. *)
val pipes : 'msg t -> 'msg Pipe.Reader.t * 'msg Pipe.Writer.t

(** Get an RPC transport over websocket *)
val transport : string t -> Async_rpc_kernel.Rpc.Transport.t

val close_finished
  :  _ t
  -> (Connection_close_reason.t * string * Info.t option) Deferred.t

val frame_received : _ t -> (Opcode.t -> unit) Bus.Read_only.t
val send_ping : _ t -> string -> unit

(** Listen for pongs frames, and report when it has been too long since the last pong
    frame. Since pong frames don't typically arrive unsolicited (though it is legal if
    they do), we send ping frames at the specified interval. *)
val monitor_pongs
  :  ?time_source:Time_source.t
  -> ping_every:Time_ns.Span.t
  -> concerning_pong_response_delay:Time_ns.Span.t
  -> on_concerning_pong_response_delay:(unit -> unit)
  -> _ t
  -> unit
