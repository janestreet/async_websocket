open! Core

type t =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
  (* RFC 6455: Opcodes 0x3-0x7 are reserved for further non-control frames yet to be
     defined. Opcodes 0xB-0xF are reserved for further control frames yet to be defined.
  *)
  | Ctrl of (int[@quickcheck.generator Int.gen_incl 0xB 0xF])
  | Nonctrl of (int[@quickcheck.generator Int.gen_incl 0x3 0x7])
[@@deriving sexp_of, equal, quickcheck]

let of_int i =
  match i land 0xf with
  | 0x0 -> Continuation
  | 0x1 -> Text
  | 0x2 -> Binary
  | 0x8 -> Close
  | 0x9 -> Ping
  | 0xA -> Pong
  | i when i > 2 && i < 8 -> Nonctrl i
  | i -> Ctrl i
;;

let to_int = function
  | Continuation -> 0x0
  | Text -> 0x1
  | Binary -> 0x2
  | Close -> 0x8
  | Ping -> 0x9
  | Pong -> 0xA
  | Ctrl i | Nonctrl i -> i
;;

type kind =
  | Control
  | Non_control

let to_kind = function
  | Close | Ping | Pong | Ctrl _ -> Control
  | Continuation | Text | Binary | Nonctrl _ -> Non_control
;;
