type t = Bytes.t

exception Overflow

let zero () = Bytes.make 16 '\x00'
let min_int () = zero ()
let max_int () = Bytes.make 16 '\xff'
let equal = Bytes.equal
let compare = Bytes.compare

let is_hex_char = function
  | 'a' .. 'f' | '0' .. '9' | 'A' .. 'Z' -> true
  | _ -> false

let int_of_hex_char c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'a' .. 'f' -> Char.code c - 87
  | 'A' .. 'F' -> Char.code c - 55
  | _ -> invalid_arg "char is not a valid hex digit"

let fold_left f a x =
  let a' = ref a in
  for i = 0 to 15 do
    let x' = Bytes.get_uint8 x i in
    a' := f !a' x'
  done;
  !a'

let iteri_right2 f x y =
  for i = 15 downto 0 do
    let x' = Bytes.get_uint8 x i in
    let y' = Bytes.get_uint8 y i in
    f i x' y'
  done

(* TODO allow strings shorter than 32 characters. add 0 from left for padding. *)
let of_string_exn s =
  if String.length s <> 32 && String.for_all is_hex_char s then
    invalid_arg "not 32 chars long or invalid hex chars"
  else
    let b = zero () in
    let i = ref 31 in
    let j = ref 15 in
    while !i >= 0 do
      let x = int_of_hex_char (String.get s !i) in
      let y = int_of_hex_char (String.get s (!i - 1)) in
      Bytes.set_uint8 b !j ((y lsl 4) + x);
      i := !i - 2;
      j := !j - 1
    done;
    b

let v = of_string_exn
let of_string s = try Some (of_string_exn s) with Invalid_argument _ -> None

(* TODO
   - use Bytes.copy for thread-safety??
   - optionally ommit leading zeroes by using for upto
*)
let to_string b =
  let l = ref [] in
  for i = 15 downto 0 do
    l := Printf.sprintf "%.2x" (Bytes.get_uint8 b i) :: !l
  done;
  String.concat "" !l

let pp ppf t = Format.fprintf ppf "uintb128 = %s" (to_string t)

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
  if Bytes.compare x y = -1 then invalid_arg "y is larger than x"
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

(* Extract the [n] least significant bits from [x] *)
let get_lsbits n x =
  if n <= 0 || n > 8 then invalid_arg "out of bounds";
  x land ((1 lsl n) - 1)

(* Extract the [n] most significant bits from [x] *)
let get_msbits n x =
  if n <= 0 || n > 8 then invalid_arg "out of bounds";
  (x land (255 lsl (8 - n))) lsr (8 - n)

let set_bit i x =
  assert (i >= 0 && i <= 7);
  x lor (1 lsl i)

let is_bit_set i x =
  assert (i >= 0 && i <= 7);
  x land (1 lsl i) <> 0

(* Set value [x] in [y]'s [n] MSB bits *)
let set_msbits n x y =
  if n < 0 || n > 8 then raise (Invalid_argument "n must be >= 0 && <= 8")
  else if n = 0 then y
  else if n = 8 then x
  else (x lsl (8 - n)) lor y

(* Returns a tuple of how many bytes and how many subsequent
   bits after that need to be shifted.

   Shift by 19 bits results in (2, 3): Shift by 2 bytes, then by 3 bits
*)
let get_bitshift_counts n =
  assert (n >= 0 && n <= 128);
  if n = 0 then (0, 0) else (n / 8, n mod 8)

let shift_right n x =
  match n with
  | 0 -> x
  | 128 -> zero ()
  | n when n > 0 && n < 128 ->
      let b = zero () in
      let shift_bytes, shift_bits = get_bitshift_counts n in
      (if shift_bits = 0 then
       for i = 0 to 15 - shift_bytes do
         let x' = Bytes.get_uint8 x i in
         Bytes.set_uint8 b (i + shift_bytes) x'
       done
      else
        let carry = ref 0 in
        for i = 0 to 15 - shift_bytes do
          let x' = Bytes.get_uint8 x i in
          let new_carry = get_lsbits shift_bits x' in
          let shifted_value = x' lsr shift_bits in
          let new_value = set_msbits shift_bits !carry shifted_value in
          Bytes.set_uint8 b (i + shift_bytes) new_value;
          carry := new_carry
        done);
      b
  | _ -> raise (Invalid_argument "n must be >= 0 && <= 128")

let shift_left n x =
  match n with
  | 0 -> x
  | 128 -> zero ()
  | n when n > 0 && n < 128 ->
      let b = zero () in
      let shift_bytes, shift_bits = get_bitshift_counts n in
      (if shift_bits = 0 then
       for i = 15 downto 0 + shift_bytes do
         let x' = Bytes.get_uint8 x i in
         Bytes.set_uint8 b (i - shift_bytes) x'
       done
      else
        let carry = ref 0 in
        for i = 15 downto 0 + shift_bytes do
          let x' = Bytes.get_uint8 x i in
          let new_carry = get_msbits shift_bits x' in
          let shifted_value = x' lsl shift_bits in
          let new_value = shifted_value lor !carry in
          Bytes.set_uint8 b (i - shift_bytes) new_value;
          carry := new_carry
        done);
      b
  | _ -> raise (Invalid_argument "n must be >= 0 && <= 128")

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
