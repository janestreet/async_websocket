open! Core
open Async
module Opcode = Opcode
module Connection_close_reason = Connection_close_reason
module Frame_reader = Frame.Frame_reader
module Frame = Frame
module Iobuf_writer = Frame.Iobuf_writer
module Content_reassembler = Content_reassembler

type t

val sec_websocket_accept_header_value : sec_websocket_key:string -> string

module Websocket_role : sig
  type t =
    | Client
    | Server
  [@@deriving sexp_of]
end

val create
  :  ?opcode:[ `Text | `Binary ]
  -> role:Websocket_role.t
  -> Reader.t
  -> Writer.t
  -> t

val pipes : t -> string Pipe.Reader.t * string Pipe.Writer.t
val transport : t -> Async_rpc_kernel.Rpc.Transport.t
val close_finished : t -> (Connection_close_reason.t * string * Info.t option) Deferred.t
val frame_received : t -> (Opcode.t -> unit) Bus.Read_only.t
val send_ping : t -> string -> unit

(** Listen for pongs frames, and report when it has been too long since the last
    pong frame. Since pong frames don't typically arrive unsolicited (though it
    is legal if they do), we send ping frames at the specified interval. *)
val monitor_pongs
  :  ?time_source:Time_source.t
  -> ping_every:Time_ns.Span.t
  -> concerning_pong_response_delay:Time_ns.Span.t
  -> on_concerning_pong_response_delay:(unit -> unit)
  -> t
  -> unit
