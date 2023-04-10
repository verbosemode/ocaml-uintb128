type t = Bytes.t

let int_of_hex_char c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'a' .. 'f' -> Char.code c - 87
  | 'A' .. 'F' -> Char.code c - 55
  | _ -> invalid_arg "char is not a valid hex digit"

exception Overflow

let zero () = Bytes.make 16 '\x00'
let min_int () = zero ()
let max_int () = Bytes.make 16 '\xff'
let equal = Bytes.equal
let compare = Bytes.compare

let fold_left f a b =
  let a' = ref a in
  for i = 0 to 15 do
    let x' = Bytes.get_uint8 b i in
    a' := f !a' x'
  done;
  !a'

let iteri_right2 f x y =
  for i = 15 downto 0 do
    let x' = Bytes.get_uint8 x i in
    let y' = Bytes.get_uint8 y i in
    f i x' y'
  done

let of_hexstring_exn s =
  let l = String.length s in
  if l != 32 then invalid_arg "not 32 chars long"
  else
    let b = zero () in
    let bi = ref 15 in
    let i = ref (l - 1) in
    while !i >= 0 do
      let x = int_of_hex_char (String.get s !i) in
      let y = int_of_hex_char (String.get s (!i - 1)) in
      Bytes.set_uint8 b !bi ((y lsl 4) + x);
      i := !i - 2;
      bi := !bi - 1
    done;
    b

let of_hexstring s = try Some (of_hexstring_exn s) with Invalid_argument _ -> None

let to_hexstring b =
  let l = ref [] in
  for i = 15 downto 0 do
    l := Printf.sprintf "%.2x" (Bytes.get_uint8 b i) :: !l
  done;
  String.concat "" !l

let pp ppf b = Format.fprintf ppf "%s" (to_hexstring b)

let of_int64 (a, b) =
  let b' = zero () in
  Bytes.set_int64_be b' 0 a;
  Bytes.set_int64_be b' 8 b;
  b'

let to_int64 b = (Bytes.get_int64_be b 0, Bytes.get_int64_be b 8)

let of_int32 (a, b, c, d) =
  let b' = zero () in
  Bytes.set_int32_be b' 0 a;
  Bytes.set_int32_be b' 4 b;
  Bytes.set_int32_be b' 8 c;
  Bytes.set_int32_be b' 12 d;
  b'

let to_int32 b =
  ( Bytes.get_int32_be b 0,
    Bytes.get_int32_be b 4,
    Bytes.get_int32_be b 8,
    Bytes.get_int32_be b 12 )

let of_int16 (a, b, c, d, e, f, g, h) =
  let b' = zero () in
  Bytes.set_uint16_be b' 0 a;
  Bytes.set_uint16_be b' 2 b;
  Bytes.set_uint16_be b' 4 c;
  Bytes.set_uint16_be b' 6 d;
  Bytes.set_uint16_be b' 8 e;
  Bytes.set_uint16_be b' 10 f;
  Bytes.set_uint16_be b' 12 g;
  Bytes.set_uint16_be b' 14 h;
  b'

let to_int16 b =
  ( Bytes.get_uint16_be b 0,
    Bytes.get_uint16_be b 2,
    Bytes.get_uint16_be b 4,
    Bytes.get_uint16_be b 6,
    Bytes.get_uint16_be b 8,
    Bytes.get_uint16_be b 10,
    Bytes.get_uint16_be b 12,
    Bytes.get_uint16_be b 14 )

let add_exn x y =
  let b = zero () in
  let carry = ref 0 in
  iteri_right2
    (fun i x' y' ->
      let sum = x' + y' + !carry in
      if sum >= 256 then (
        carry := 1;
        Bytes.set_uint8 b i (sum - 256))
      else (
        carry := 0;
        Bytes.set_uint8 b i sum))
    x y;
  if !carry <> 0 then raise Overflow else b

let add x y = try Some (add_exn x y) with Overflow -> None

let sub_exn x y =
  if Bytes.compare x y = -1 then raise Overflow
  else
    let b = zero () in
    let carry = ref 0 in
    iteri_right2
      (fun i x' y' ->
        if x' < y' then (
          Bytes.set_uint8 b i (256 + x' - y' - !carry);
          carry := 1)
        else (
          Bytes.set_uint8 b i (x' - y' - !carry);
          carry := 0))
      x y;
    if !carry <> 0 then raise Overflow else b

let sub x y =
  try Some (sub_exn x y) with Overflow -> None | Invalid_argument _ -> None

let logand x y =
  let b = zero () in
  iteri_right2 (fun i x y -> Bytes.set_uint8 b i (x land y)) x y;
  b

let logor x y =
  let b = zero () in
  iteri_right2 (fun i x y -> Bytes.set_uint8 b i (x lor y)) x y;
  b

let logxor x y =
  let b = zero () in
  iteri_right2 (fun i x y -> Bytes.set_uint8 b i (x lxor y)) x y;
  b

let lognot x =
  let b = zero () in
  Bytes.iteri (fun i _ -> Bytes.set_uint8 b i (lnot (Bytes.get_uint8 x i))) x;
  b

module Byte = struct
  (* Extract the [n] least significant bits from [i] *)
  let get_lsbits n i =
    if n <= 0 || n > 8 then invalid_arg "out of bounds";
    i land ((1 lsl n) - 1)

  (* Extract the [n] most significant bits from [i] *)
  let get_msbits n i =
    if n <= 0 || n > 8 then invalid_arg "out of bounds";
    (i land (255 lsl (8 - n))) lsr (8 - n)

  (* Set value [x] in [i]'s [n] most significant bits *)
  let set_msbits n x i =
    if n < 0 || n > 8 then raise (Invalid_argument "n must be >= 0 && <= 8")
    else if n = 0 then i
    else if n = 8 then x
    else (x lsl (8 - n)) lor i

  (* set bits are represented as true *)
  let fold_left f a i =
    let bitmask = ref 0b1000_0000 in
    let a' = ref a in
    for _ = 0 to 7 do
      a' := f !a' (i land !bitmask > 0);
      bitmask := !bitmask lsr 1
    done;
    !a'
end

let shift_right x n =
  match n with
  | 0 -> x
  | 128 -> zero ()
  | n when n > 0 && n < 128 ->
      let b = zero () in
      let shift_bytes, shift_bits = (n / 8, n mod 8) in
      (if shift_bits = 0 then Bytes.blit x 0 b shift_bytes (16 - shift_bytes)
      else
        let carry = ref 0 in
        for i = 0 to 15 - shift_bytes do
          let x' = Bytes.get_uint8 x i in
          let new_carry = Byte.get_lsbits shift_bits x' in
          let shifted_value = x' lsr shift_bits in
          let new_value = Byte.set_msbits shift_bits !carry shifted_value in
          Bytes.set_uint8 b (i + shift_bytes) new_value;
          carry := new_carry
        done);
      b
  | _ -> raise (Invalid_argument "n must be >= 0 && <= 128")

let shift_left x n =
  match n with
  | 0 -> x
  | 128 -> zero ()
  | n when n > 0 && n < 128 ->
      let b = zero () in
      let shift_bytes, shift_bits = (n / 8, n mod 8) in
      (if shift_bits = 0 then Bytes.blit x shift_bytes b 0 (16 - shift_bytes)
      else
        let carry = ref 0 in
        for i = 15 downto 0 + shift_bytes do
          let x' = Bytes.get_uint8 x i in
          let new_carry = Byte.get_msbits shift_bits x' in
          let shifted_value = x' lsl shift_bits in
          let new_value = shifted_value lor !carry in
          Bytes.set_uint8 b (i - shift_bytes) new_value;
          carry := new_carry
        done);
      b
  | _ -> raise (Invalid_argument "n must be >= 0 && <= 128")
