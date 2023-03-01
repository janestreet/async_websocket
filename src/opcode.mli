open! Core

type t =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
  | Ctrl of int
  | Nonctrl of int
[@@deriving sexp_of, equal, quickcheck]

val of_int : int -> t
val to_int : t -> int

type kind =
  | Control
  | Non_control

val to_kind : t -> kind
