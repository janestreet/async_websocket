## Release v0.16.0

- Added a [transport] function to get an RPC transport for the websocket.
  Previous, users of the library had to construct a transport from the pipes
  given by [pipes], but now that concern has been abstracted away.
- Added the `Frame_reader`, `Frame`, `Iobuf_writer`, and `Content_reassembler`
  modules to enable reading and writing websocket frames without allocation.
  The implementation of the main interface for the library has also been
  switched to use these modules, but still does allocation, since the glue code
  uses of async pipes.
- Added `monitor_pongs` as a keepalive mechanism for websocket connections.
