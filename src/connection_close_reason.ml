open! Core

(* https://tools.ietf.org/html/rfc6455#section-7.4 *)

type t =
  | Normal_closure
  | Endpoint_going_away
  | Protocol_error
  | Cannot_accept_data
  | Reserved_0
  | No_status_code
  | Closed_abnormally
  | Invalid_message_sent
  | Policy_violation
  | Message_too_large
  | Invalid_handshake
  | Unexpected_condition
  | Tls_handshake_failure
  | Unknown of int
[@@deriving sexp_of, equal, quickcheck]

let to_int = function
  | Normal_closure -> 1000
  | Endpoint_going_away -> 1001
  | Protocol_error -> 1002
  | Cannot_accept_data -> 1003
  | Reserved_0 -> 1004
  | No_status_code -> 1005
  | Closed_abnormally -> 1006
  | Invalid_message_sent -> 1007
  | Policy_violation -> 1008
  | Message_too_large -> 1009
  | Invalid_handshake -> 1010
  | Unexpected_condition -> 1011
  | Tls_handshake_failure -> 1015
  | Unknown code -> code
;;

let of_int = function
  | 1000 -> Normal_closure
  | 1001 -> Endpoint_going_away
  | 1002 -> Protocol_error
  | 1003 -> Cannot_accept_data
  | 1004 -> Reserved_0
  | 1005 -> No_status_code
  | 1006 -> Closed_abnormally
  | 1007 -> Invalid_message_sent
  | 1008 -> Policy_violation
  | 1009 -> Message_too_large
  | 1010 -> Invalid_handshake
  | 1011 -> Unexpected_condition
  | 1015 -> Tls_handshake_failure
  | code -> Unknown code
;;
