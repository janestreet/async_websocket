open Core
open Async

type t =
  { opcode : Opcode.t
  ; final : bool
  ; content : string
  }
[@@deriving sexp_of, quickcheck, equal]

module Error = struct
  type t =
    { code : Connection_close_reason.t
    ; message : string
    }
  [@@deriving sexp_of]
end

(* Extensions aren't implemented *)
let create ~opcode ?(final = true) content = { opcode; final; content }

(* See rfc6455 - 5.5.1
   The Close frame MAY contain a body (the "Application data" portion of
   the frame) that indicates a reason for closing, such as an endpoint
   shutting down, an endpoint having received a frame too large, or an
   endpoint having received a frame that does not conform to the format
   expected by the endpoint.  If there is a body, the first two bytes of
   the body MUST be a 2-byte unsigned integer (in network byte order)
   representing a status code with value /code/ defined in Section 7.4.
   Following the 2-byte integer, the body MAY contain UTF-8-encoded data
   with value /reason/, the interpretation of which is not defined by
   this specification.  This data is not necessarily human readable but
   may be useful for debugging or passing information relevant to the
   script that opened the connection.  As the data is not guaranteed to
   be human readable, clients MUST NOT show it to end users. *)
let create_close ~code ?final content =
  let len = String.length content in
  let content' = Bytes.create (len + 2) in
  Bytes.From_string.blit ~src:content ~src_pos:0 ~dst:content' ~dst_pos:2 ~len;
  Binary_packing.pack_unsigned_16_big_endian ~buf:content' ~pos:0 code;
  create ~opcode:Close ?final (Bytes.to_string content')
;;

let set_bit v idx b = if b then v lor (1 lsl idx) else v land lnot (1 lsl idx)
let xor_chars a b = Char.of_int_exn (Char.to_int a lxor Char.to_int b)

let xor_iobuf_with_mask iobuf mask ~offset =
  for i = 0 to Iobuf.length iobuf - 1 do
    let c =
      Char.of_int_exn
        (Char.to_int (Bytes.get mask ((offset + i) mod 4))
         lxor Char.to_int (Iobuf.Peek.char iobuf ~pos:i))
    in
    Iobuf.Poke.char ~pos:i iobuf c
  done
;;

let write_int16 writer n =
  let buf = Bytes.create 2 in
  Binary_packing.pack_unsigned_16_big_endian ~buf ~pos:0 n;
  Writer.write_bytes ~pos:0 ~len:2 writer buf
;;

let write_int64 writer n =
  let buf = Bytes.create 8 in
  Binary_packing.pack_signed_64_big_endian ~buf ~pos:0 n;
  Writer.write_bytes ~pos:0 ~len:8 writer buf
;;

let classify_len = function
  | n when n < 126 -> `In_header
  | n when n < 1 lsl 16 -> `Two_byte_extended
  | _ -> `Eight_byte_extended
;;

let make_hdr ~final ~opcode ~masked ~len =
  let payload_length =
    match classify_len len with
    | `In_header -> len
    | `Two_byte_extended -> 126
    | `Eight_byte_extended -> 127
  in
  let hdr = 0 in
  let hdr = set_bit hdr 15 final in
  let hdr = hdr lor (opcode lsl 8) in
  let hdr = set_bit hdr 7 masked in
  let hdr = hdr lor payload_length in
  hdr
;;

let write_frame =
  let mask = Bytes.create 4 in
  let regenerate_random_mask () =
    Bytes.set mask 0 (Char.of_int_exn (Random.int 128));
    Bytes.set mask 1 (Char.of_int_exn (Random.int 128));
    Bytes.set mask 2 (Char.of_int_exn (Random.int 128));
    Bytes.set mask 3 (Char.of_int_exn (Random.int 128))
  in
  let xor_with_mask msg =
    for i = 0 to Bytes.length msg - 1 do
      let mask_char = Bytes.get mask (i mod 4) in
      let message_char = Bytes.get msg i in
      Bytes.set msg i (xor_chars mask_char message_char)
    done
  in
  fun ~masked writer frame ->
    if Writer.is_closed writer
    then ()
    else (
      let content = Bytes.of_string frame.content in
      let len = Bytes.length content in
      let opcode = Opcode.to_int frame.opcode in
      let hdr = make_hdr ~final:frame.final ~opcode ~masked ~len in
      let buf = Bytes.create 2 in
      Binary_packing.pack_unsigned_16_big_endian ~buf ~pos:0 hdr;
      Writer.write_bytes ~len:2 ~pos:0 writer buf;
      (match classify_len len with
       | `In_header -> ()
       | `Two_byte_extended -> write_int16 writer len
       | `Eight_byte_extended -> write_int64 writer (Int64.of_int len));
      if masked
      then (
        regenerate_random_mask ();
        Writer.write_bytes ~pos:0 ~len:4 writer mask;
        xor_with_mask content);
      Writer.write_bytes ~pos:0 ~len writer content)
;;

let frame_bytes ~content_len ~masked =
  let hdr = 2 in
  let extra_payload_length_bytes =
    match classify_len content_len with
    | `In_header -> 0
    | `Two_byte_extended -> 2
    | `Eight_byte_extended -> 8
  in
  let mask = if masked then 4 else 0 in
  hdr + mask + extra_payload_length_bytes + content_len
;;

let max_content_bytes ~max_frame_bytes ~masked =
  let hdr = 2 in
  let mask = if masked then 4 else 0 in
  let max_content_plus_length = max_frame_bytes - hdr - mask in
  if max_content_plus_length - 2 < 126
  then Int.min max_content_plus_length (126 - 1)
  else if max_content_plus_length - 8 < 1 lsl 16
  then Int.min (max_content_plus_length - 2) ((1 lsl 16) - 1)
  else max_content_plus_length - 8
;;

module Iobuf_writer = struct
  type t =
    { mask : Bytes.t
    ; content_window : (read_write, Iobuf.seek) Iobuf.t
    ; mutable output_iobuf : (read_write, Iobuf.seek) Iobuf.t
    ; role : [ `Client of Base.Random.State.t | `Server ]
    }

  let create ~role =
    { mask = Bytes.create 4
    ; content_window = Iobuf.create ~len:0
    ; output_iobuf = Iobuf.create ~len:0
    ; role
    }
  ;;

  let regenerate_random_mask t ~random_state =
    Bytes.set t.mask 0 (Char.of_int_exn (Random.State.int random_state 128));
    Bytes.set t.mask 1 (Char.of_int_exn (Random.State.int random_state 128));
    Bytes.set t.mask 2 (Char.of_int_exn (Random.State.int random_state 128));
    Bytes.set t.mask 3 (Char.of_int_exn (Random.State.int random_state 128))
  ;;

  let required_frame_bytes t ~content_len =
    frame_bytes
      ~content_len
      ~masked:
        (match t.role with
         | `Client _ -> true
         | `Server -> false)
  ;;

  let max_content_bytes t ~max_frame_bytes =
    max_content_bytes
      ~max_frame_bytes
      ~masked:
        (match t.role with
         | `Client _ -> true
         | `Server -> false)
  ;;

  let start_write t frame_buffer ~(opcode : Opcode.t) ~(final : bool) ~(content_len : int)
    =
    t.output_iobuf <- frame_buffer;
    let opcode = Opcode.to_int opcode in
    (* "A client MUST mask all frames that it sends to the server."
       "A server MUST NOT mask any frames that it sends to the client." *)
    let hdr =
      make_hdr
        ~final
        ~opcode
        ~masked:
          (match t.role with
           (* "A client MUST mask all frames that it sends to the server."
              "A server MUST NOT mask any frames that it sends to the client." *)
           | `Server -> false
           | `Client _ -> true)
        ~len:content_len
    in
    Iobuf.Fill.int16_be_trunc t.output_iobuf hdr;
    (match classify_len content_len with
     | `In_header -> ()
     | `Two_byte_extended -> Iobuf.Fill.uint16_be_trunc t.output_iobuf content_len
     | `Eight_byte_extended -> Iobuf.Fill.uint64_be_trunc t.output_iobuf content_len);
    (match t.role with
     | `Server -> ()
     | `Client random_state ->
       regenerate_random_mask t ~random_state;
       Iobuf.Fill.byteso t.output_iobuf t.mask);
    Iobuf.resize t.output_iobuf ~len:content_len;
    Iobuf.Expert.set_bounds_and_buffer ~src:t.output_iobuf ~dst:t.content_window;
    t.content_window
  ;;

  let finish_write_exn t =
    assert (Iobuf.length t.content_window = 0);
    (match t.role with
     | `Server -> ()
     | `Client _ -> xor_iobuf_with_mask t.output_iobuf t.mask ~offset:0);
    Iobuf.advance t.output_iobuf (Iobuf.length t.output_iobuf)
  ;;
end

module Frame_reader = struct
  module Read_result = struct
    type t =
      | No_frame
      | Incomplete_frame
      | Consumed_one_frame
      | Cannot_parse_uint64_length
    [@@deriving sexp]
  end

  let bit_is_set idx v = (v lsr idx) land 1 = 1
  let int_value shift len v = (v lsr shift) land ((1 lsl len) - 1)

  module Header_part2 : sig
    type t

    val of_int8 : int -> t

    module Payload_length : sig
      type t

      module Meaning : sig
        type 'a t =
          | Length : int t
          | Extended_payload_len_uint16 : unit t
          | Extended_payload_len_uint64 : unit t

        type packed = T : 'a t -> packed [@@unboxed]
      end

      val meaning : t -> Meaning.packed
      val to_int : t -> int Meaning.t -> int
    end

    val payload_length : t -> Payload_length.t
    val masked : t -> bool
  end = struct
    type t = int

    let of_int8 t = t

    module Payload_length = struct
      type t = int

      module Meaning = struct
        type 'a t =
          | Length : int t
          | Extended_payload_len_uint16 : unit t
          | Extended_payload_len_uint64 : unit t

        type packed = T : 'a t -> packed [@@unboxed]
      end

      let extended_payload_len_uint16 = -1
      let extended_payload_len_uint64 = -2

      let meaning : t -> Meaning.packed = function
        | x when x = extended_payload_len_uint16 -> T Extended_payload_len_uint16
        | x when x = extended_payload_len_uint64 -> T Extended_payload_len_uint64
        | x when x < 0 ->
          (* This will never be reached because [t] is constructed with only 7 bits, and
             the possible negative cases are explicitely handled above. *)
          assert false
        | _ -> T Length
      ;;

      let to_int : t -> int Meaning.t -> int = fun t Length -> t
    end

    let payload_length t =
      match int_value 0 7 t with
      | 126 -> Payload_length.extended_payload_len_uint16
      | 127 -> Payload_length.extended_payload_len_uint64
      | i when i >= 0 && i < 126 -> i
      | _ ->
        (* This will never be reached because the matched value is constructed
           with only 7 bits. *)
        assert false
    ;;

    let%expect_test "assert won't be reached" =
      Quickcheck.test Int.quickcheck_generator ~f:(fun value ->
        let payload_length = value |> of_int8 |> payload_length in
        ignore (Payload_length.meaning payload_length : Payload_length.Meaning.packed));
      return ()
    ;;

    let masked t = bit_is_set 7 t
  end

  module Payload_length_with_extension : sig
    type t

    module Meaning : sig
      type 'a t = private
        | Length : int t
        | Incomplete_frame : unit t
        | Cannot_parse_uint64_length : unit t

      type packed = T : 'a t -> packed [@@unboxed]
    end

    val consume : header_part2:Header_part2.t -> iobuf:(read, Iobuf.seek) Iobuf.t -> t
    val meaning : t -> Meaning.packed
    val to_int : t -> int Meaning.t -> int
  end = struct
    type t = int

    module Meaning = struct
      type 'a t =
        | Length : int t
        | Incomplete_frame : unit t
        | Cannot_parse_uint64_length : unit t

      type packed = T : 'a t -> packed [@@unboxed]
    end

    let incomplete_frame = -1
    let cannot_parse_uint64_length = -2

    let meaning : t -> Meaning.packed = function
      | x when x = cannot_parse_uint64_length -> T Cannot_parse_uint64_length
      | x when x = incomplete_frame -> T Incomplete_frame
      | x when x < 0 ->
        (* This will never be reached because [t] is constructed with only 7 bits, and
           the possible negative cases are explicitely handled above. *)
        assert false
      | _ -> T Length
    ;;

    let to_int : t -> int Meaning.t -> int = fun t Length -> t

    let consume ~header_part2 ~iobuf =
      let payload_length = Header_part2.payload_length header_part2 in
      match Header_part2.Payload_length.meaning payload_length with
      | T Extended_payload_len_uint16 ->
        if Iobuf.length iobuf >= 2
        then Iobuf.Unsafe.Consume.uint16_be iobuf
        else incomplete_frame
      | T Extended_payload_len_uint64 ->
        if Iobuf.length iobuf >= 8
        then (
          try Iobuf.Unsafe.Consume.uint64_be_exn iobuf with
          | _ -> cannot_parse_uint64_length)
        else incomplete_frame
      | T (Length as meaning) -> Header_part2.Payload_length.to_int payload_length meaning
    ;;
  end

  module Mask : sig
    val consume
      :  header_part2:Header_part2.t
      -> mask:Bytes.t
      -> (read, Iobuf.seek) Iobuf.t
      -> [ `Mask | `Incomplete_frame | `No_mask_needed ]
  end = struct
    let consume ~header_part2 ~mask iobuf =
      match Header_part2.masked header_part2 with
      | false -> `No_mask_needed
      | true ->
        if Iobuf.length iobuf < 4
        then `Incomplete_frame
        else (
          Iobuf.Consume.To_bytes.blit ~src:iobuf ~dst:mask ~dst_pos:0 ~len:4;
          `Mask)
    ;;
  end

  type t =
    { frame_handler :
        (read, Iobuf.seek) Iobuf.t
        -> Opcode.t
        -> bool
        -> [ `Content_was_masked | `Content_was_not_masked ]
        -> unit
    ; mask : Bytes.t
    }

  let create ~frame_handler =
    let frame_handler iobuf opcode final masked =
      frame_handler ~opcode ~final ~content:(Iobuf.no_seek iobuf) ~masked
    in
    { frame_handler; mask = Bytes.create 4 }
  ;;

  let expected_frame_bytes (iobuf : (read, Iobuf.seek) Iobuf.t) : int option =
    match Iobuf.length iobuf with
    | 0 -> None
    | len when len < 2 -> None
    | _ ->
      let lo = Iobuf.Lo_bound.window iobuf in
      let _header_part1 = Iobuf.Unsafe.Consume.int8 iobuf in
      let header_part2 = iobuf |> Iobuf.Unsafe.Consume.int8 |> Header_part2.of_int8 in
      let payload_length =
        Payload_length_with_extension.consume ~header_part2 ~iobuf:(Iobuf.read_only iobuf)
      in
      Iobuf.Lo_bound.restore lo iobuf;
      (match Payload_length_with_extension.meaning payload_length with
       | T Cannot_parse_uint64_length | T Incomplete_frame -> None
       | T (Length as meaning) ->
         Some
           (frame_bytes
              ~content_len:(Payload_length_with_extension.to_int payload_length meaning)
              ~masked:(Header_part2.masked header_part2)))
  ;;

  let maybe_consume_payload_length iobuf =
    match Iobuf.length iobuf with
    | 0 -> `No_frame_header
    | len when len < 2 -> `Incomplete_frame_header
    | _ ->
      let lo = Iobuf.Lo_bound.window iobuf in
      let header_part1 = Iobuf.Unsafe.Consume.int8 iobuf in
      let header_part2 = iobuf |> Iobuf.Unsafe.Consume.int8 |> Header_part2.of_int8 in
      let payload_length =
        Payload_length_with_extension.consume
          ~header_part2
          ~iobuf:(Iobuf.read_only_local iobuf)
      in
      (match Payload_length_with_extension.meaning payload_length with
       | T Cannot_parse_uint64_length ->
         Iobuf.Lo_bound.restore lo iobuf;
         `Cannot_parse_uint64_length
       | T Incomplete_frame ->
         Iobuf.Lo_bound.restore lo iobuf;
         `Incomplete_frame_header
       | T (Length as meaning) ->
         let payload_length =
           Payload_length_with_extension.to_int payload_length meaning
         in
         `Consumed_payload_length (header_part1, header_part2, payload_length))
  ;;

  let get_payload_length iobuf =
    match maybe_consume_payload_length iobuf with
    | `Cannot_parse_uint64_length -> `Cannot_parse_uint64_length
    | `Incomplete_frame_header -> `Incomplete_frame_header
    | `No_frame_header -> `No_frame_header
    | `Consumed_payload_length (_, _, payload_length) -> `Payload_length payload_length
  ;;

  let maybe_consume_header iobuf ~payload_availability ~mask ~f =
    let lo = Iobuf.Lo_bound.window iobuf in
    match maybe_consume_payload_length iobuf with
    | `No_frame_header -> `No_frame_header
    | `Incomplete_frame_header -> `Incomplete_frame_header
    | `Cannot_parse_uint64_length -> `Cannot_parse_uint64_length
    | `Consumed_payload_length (header_part1, header_part2, payload_length) ->
      (match Mask.consume ~header_part2 ~mask (Iobuf.read_only_local iobuf) with
       | `Incomplete_frame ->
         Iobuf.Lo_bound.restore lo iobuf;
         `Incomplete_frame_header
       | (`No_mask_needed | `Mask) as masked ->
         let okay_to_consume =
           match payload_availability with
           | `Entire_payload_must_be_available -> Iobuf.length iobuf >= payload_length
           | `Incomplete_payload_ok -> true
         in
         (match okay_to_consume with
          | false ->
            Iobuf.Lo_bound.restore lo iobuf;
            `Incomplete_frame
          | true ->
            f
              ~masked
              ~final:(bit_is_set 7 header_part1)
              ~opcode:(Opcode.of_int (int_value 0 4 header_part1))
              ~payload_length;
            `Consumed_header))
  ;;

  let consume_frame t (iobuf : (read_write, Iobuf.seek) Iobuf.t) : Read_result.t =
    match
      maybe_consume_header
        iobuf
        ~payload_availability:`Entire_payload_must_be_available
        ~mask:t.mask
        ~f:(fun ~masked ~final ~opcode ~payload_length ->
          let process_frame_content iobuf =
            (* Unmask the frame inline. This should be okay now that we know for sure
               we can process the frame. *)
            let masked =
              match masked with
              | `No_mask_needed -> `Content_was_not_masked
              | `Mask ->
                xor_iobuf_with_mask iobuf t.mask ~offset:0;
                `Content_was_masked
            in
            Iobuf.protect_window_bounds_and_buffer_3
              (Iobuf.read_only iobuf)
              ~f:t.frame_handler
              opcode
              final
              masked
          in
          let iobuf_length = Iobuf.length iobuf in
          Iobuf.unsafe_resize iobuf ~len:payload_length;
          process_frame_content (Iobuf.no_seek iobuf);
          Iobuf.unsafe_resize iobuf ~len:iobuf_length;
          Iobuf.unsafe_advance iobuf payload_length)
    with
    | `Cannot_parse_uint64_length -> Cannot_parse_uint64_length
    | `Consumed_header -> Consumed_one_frame
    | `Incomplete_frame | `Incomplete_frame_header -> Incomplete_frame
    | `No_frame_header -> No_frame
  ;;

  let rec consume_all_available_frames t iobuf =
    match consume_frame t iobuf with
    | Cannot_parse_uint64_length -> `Cannot_parse_uint64_length
    | No_frame -> `Consumed_as_much_as_possible
    | Incomplete_frame -> `Consumed_until_incomplete_frame
    | Consumed_one_frame -> consume_all_available_frames t iobuf
  ;;

  module Expert = struct
    type partial_frame_handler =
      opcode:Opcode.t
      -> final:bool
      -> total_frame_payload_len:int
      -> payload_pos:int
      -> payload_fragment:(read, Iobuf.seek) Iobuf.t
      -> masked:[ `Payload_was_masked | `Payload_was_not_masked ]
      -> unit

    type t =
      { partial_frame_handler : partial_frame_handler
      ; mutable payload_length : int
      ; mutable remaining_payload_to_consume : int
      ; mutable unmasked_length : int
      ; mask : Bytes.t
      ; mutable masked : [ `No_mask_needed | `Mask ]
      ; mutable opcode : Opcode.t
      ; mutable final : bool
      ; output_iobuf : (read_write, Iobuf.seek) Iobuf.t
      }

    let create ~partial_frame_handler =
      { partial_frame_handler
      ; payload_length = 0
      ; remaining_payload_to_consume = 0
      ; unmasked_length = 0
      ; mask = Bytes.create 4
      ; masked = `No_mask_needed
      ; opcode = Opcode.of_int 0
      ; final = false
      ; output_iobuf = Iobuf.create ~len:0
      }
    ;;

    let consume_payload_even_if_incomplete
      t
      iobuf
      ~remaining_payload_length
      ~(f : (read_write, Iobuf.seek) Iobuf.t -> unit)
      =
      let iobuf_length = Iobuf.length iobuf in
      let possible_consume_length = min iobuf_length remaining_payload_length in
      if possible_consume_length > 0
         || (possible_consume_length = 0 && remaining_payload_length = 0)
      then (
        Iobuf.set_bounds_and_buffer ~src:iobuf ~dst:t.output_iobuf;
        Iobuf.resize t.output_iobuf ~len:possible_consume_length;
        match f t.output_iobuf with
        | () ->
          let consumed = possible_consume_length - Iobuf.length t.output_iobuf in
          Iobuf.unsafe_advance iobuf consumed;
          t.remaining_payload_to_consume <- t.remaining_payload_to_consume - consumed
        | exception exn ->
          let bt = Stdlib.Printexc.get_raw_backtrace () in
          (match
             let consumed = possible_consume_length - Iobuf.length t.output_iobuf in
             Iobuf.unsafe_advance iobuf consumed;
             t.remaining_payload_to_consume <- t.remaining_payload_to_consume - consumed
           with
           | () -> Exn.raise_with_original_backtrace exn bt
           | exception final_exn ->
             Exn.raise_with_original_backtrace (Exn.Finally (exn, final_exn)) bt))
    ;;

    let consume_frame_even_if_incomplete_payload
      t
      (iobuf : (read_write, Iobuf.seek) Iobuf.t)
      : Read_result.t
      =
      let follow_up_action =
        match t.remaining_payload_to_consume <> 0 with
        | true -> `Consume_remaining_payload
        | false ->
          (match
             maybe_consume_header
               iobuf
               ~payload_availability:`Incomplete_payload_ok
               ~mask:t.mask
               ~f:(fun ~masked ~final ~opcode ~payload_length ->
                 t.masked <- masked;
                 t.final <- final;
                 t.opcode <- opcode;
                 t.payload_length <- payload_length;
                 t.remaining_payload_to_consume <- payload_length;
                 t.unmasked_length <- 0)
           with
           | `No_frame_header -> `Return_no_frame
           | `Incomplete_frame_header | `Incomplete_frame -> `Return_incomplete_frame
           | `Cannot_parse_uint64_length -> `Return_cannot_parse_uint64_length
           | `Consumed_header -> `Consume_remaining_payload)
      in
      match follow_up_action with
      | `Return_no_frame -> No_frame
      | `Return_incomplete_frame -> Incomplete_frame
      | `Return_cannot_parse_uint64_length -> Cannot_parse_uint64_length
      | `Consume_remaining_payload ->
        let process_payload iobuf =
          (* Unmask the frame inline. This should be okay now that we know for sure
             we can process the frame. *)
          let payload_pos = t.payload_length - t.remaining_payload_to_consume in
          let masked =
            match t.masked with
            | `No_mask_needed -> `Payload_was_not_masked
            | `Mask ->
              (match t.unmasked_length < payload_pos + Iobuf.length iobuf with
               | true ->
                 let lo = Iobuf.Lo_bound.window iobuf in
                 let hi = Iobuf.Hi_bound.window iobuf in
                 let needs_unmask_len =
                   payload_pos + Iobuf.length iobuf - t.unmasked_length
                 in
                 Iobuf.unsafe_advance iobuf (t.unmasked_length - payload_pos);
                 Iobuf.unsafe_resize iobuf ~len:needs_unmask_len;
                 xor_iobuf_with_mask iobuf t.mask ~offset:t.unmasked_length;
                 Iobuf.Lo_bound.restore lo iobuf;
                 Iobuf.Hi_bound.restore hi iobuf;
                 t.unmasked_length <- payload_pos + Iobuf.length iobuf
               | false -> ());
              `Payload_was_masked
          in
          t.partial_frame_handler
            ~opcode:t.opcode
            ~final:t.final
            ~total_frame_payload_len:t.payload_length
            ~payload_pos
            ~payload_fragment:(Iobuf.read_only iobuf)
            ~masked
        in
        consume_payload_even_if_incomplete
          t
          iobuf
          ~remaining_payload_length:t.remaining_payload_to_consume
          ~f:process_payload;
        (match t.remaining_payload_to_consume = 0 with
         | false -> Incomplete_frame
         | true -> Consumed_one_frame)
    ;;

    let maybe_consume_header = maybe_consume_header
    let get_payload_length = get_payload_length
  end
end
