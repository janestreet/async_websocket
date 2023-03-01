(** [Websocket_io_handlers] provides a websocket implementation that can be conveniently
    composed via io_handlers. This is handy if you're using netkit for network IO.
*)

open! Core
open Netkit

(** The close handler is called at the earliest of:

    - We received a close frame from the other side and sent a close frame in reply
    - We sent a close frame to the other side and received their close frame in reply
    - [set_endpoint_close_finished] is called
    - We hit some error and want to tear down the connection

    It's safe (and expected) to commence closing the underlying endpoint in the close
    handler.
*)
type close_handler = code:Websocket.Connection_close_reason.t -> reason:string -> unit

type byte_stream
type message_stream
type 'a t

(** In byte stream mode, this library will put new bytes into the [recv_window t] as
    soon as they are received. So your [on_readable] may be called when there is an
    incomplete message in the recv window. This is useful if you are using a protocol
    like JSON on top of the websocket stream that contains its own message structure.
*)
val create_byte_stream
  :  ?recv_window_size:int
  -> ?send_window_size:int
  -> ?outgoing_message_format:[ `Text | `Binary ]
  -> Recv_window.t
  -> Send_window.t
  -> role:[ `Client of Random.State.t | `Server ]
  -> close_handler:close_handler
  -> byte_stream t

(** In the message stream mode, [recv_window] will contain one message at a time. Your
    [on_readable] will only be called once a full message (which may be split across
    multiple websocket frames) has been received. Your app must consume everything in
    the [recv_message] before a new message becomes available and invoke
    [notify_maybe_message_consumed].
    This is useful if you're not able to parse partial messages and need to see an entire
    message at a time to process it.
*)
val create_message_stream
  :  ?recv_window_size:int
  -> ?send_window_size:int
  -> ?outgoing_message_format:[ `Text | `Binary ]
  -> Recv_window.t
  -> Send_window.t
  -> role:[ `Client of Random.State.t | `Server ]
  -> close_handler:close_handler
  -> message_stream t

val start : _ t -> Io_handlers.t -> unit
val recv_window : byte_stream t -> Recv_window.t
val recv_message : message_stream t -> (read, Iobuf.seek) Iobuf.t

(** Report that [recv_message] has been consumed.

    This function is a no-op if not every byte in [recv_message] has been consumed, hence
    the [_maybe_].
*)
val notify_maybe_message_consumed : message_stream t -> unit

(** Any data written to [send_window] while [Send_window.bytes_queued] > 0 will be
    appended to the same message (possibly fragmented across multiple frames using the
    websocket fragmentation functionality).

    To send a separate message, wait until [Send_window.bytes_queued] = 0.

*)
val send_window : _ t -> Send_window.t

(** [(io_handlers t).on_readable] should be called whenever there is new data in the recv
    window passed to [create].

    [(io_handlers t).on_sendable] should be called whenever there is new room in the send
    window passed to [create]. *)
val io_handlers : _ t -> Io_handlers.t

(** [set_endpoint_close_finished] must be invoked whenever the underlying connection is
    closed. *)
val set_endpoint_close_finished : _ t -> unit

(** Call [prepare_ping] whenever you want to send a ping packet. Write your payload into
    the resulting iobuf and call [finish_ping] when done. The ping will be sent at the
    next available opportunity.

    [`May_not_send_ping_anymore] means that the connection is in the process of being
    closed and no new frames may be sent.

    [`Ping_already_queued] means that you previously called [prepare_ping] and that ping
    hasn't yet been sent. That might be because you forgot to call [finish_ping] or
    because the network is congested and we haven't been able to send the ping yet.
*)
val prepare_ping
  :  _ t
  -> [ `May_not_send_ping_anymore
     | `Ping_already_queued
     | `Prepared of (read_write, Iobuf.seek) Iobuf.t
     ]

val finish_ping : _ t -> unit

(** Send a close frame and initiates the closing handshake. *)
val send_close
  :  _ t
  -> code:Websocket.Connection_close_reason.t
  -> reason:string
  -> [ `May_not_send_close_anymore | `Close_already_queued | `Close_queued ]
