(** Allows reading and writing JSON to a websocket connection managed by
    {!Websocket_io_handlers}. Uses jsonaf for parsing and serialization. *)

open! Core
open Async_kernel

(** An error that occurred when parsing a JSON message.

    For example, such a message could be {| unexpected character: 'a' |}.
*)
module Parse_error : Identifiable

type t

val create : Websocket_io_handlers.byte_stream Websocket_io_handlers.t -> t

val start
  :  t
  -> on_message:(Jsonaf.t -> unit)
  -> on_error:(Parse_error.t -> [ `Continue_parsing | `Stop_parsing ])
  -> unit

(** Send a Json message on the websocket.

    It is not guaranteed that two [send] calls will result in two separate
    websocket frames. If these are the desired semantics, the user should call [flushed]
    before sending each message. *)
val send : t -> Jsonaf.t -> unit

(** Guarantees that the next message sent will begin a new websocket frame.

    This is achieved by waiting until the internal [Send_window.bytes_queued] = 0, which
    means that we will send a separate message. See comment under [send_window] in
    src/websocket_io_handlers.mli for a further explanation.
*)
val flushed : t -> unit Deferred.t
