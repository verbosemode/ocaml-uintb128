let uintb128 = Alcotest.testable Uintb128.pp Uintb128.equal

let test_add_one_to_zero () =
  let d1 = Uintb128.zero () in
  let d2 = Uintb128.v "00000000000000000000000000000001" in
  Alcotest.(check uintb128)
    "adding one to zero is one" d2 (Uintb128.add_exn d1 d2)

let test_addition_carry () =
  let d1 = Uintb128.v "000000000000000000ff000000000000" in
  let d2 = Uintb128.v "00000000000000000001000000000000" in
  let d3 = Uintb128.v "00000000000000000100000000000000" in
  Alcotest.(check uintb128)
    "test addition carry over" d3 (Uintb128.add_exn d1 d2)

let test_add_one_to_maxint_overflows () =
  let d1 = Uintb128.max_int () in
  let d2 = Uintb128.v "00000000000000000000000000000001" in
  Alcotest.(check (option uintb128))
    "adding one to max_int overflows" None (Uintb128.add d1 d2)

let test_subtract_one_from_one () =
  let d1 = Uintb128.v "00000000000000000000000000000001" in
  let d2 = Uintb128.v "00000000000000000000000000000001" in
  let d3 = Uintb128.zero () in
  Alcotest.(check uintb128)
    "subtracting one from one is zero" d3 (Uintb128.sub_exn d1 d2)

let test_subtract_carry () =
  let d1 = Uintb128.v "00000000000000000000000000000300" in
  let d2 = Uintb128.v "0000000000000000000000000000002a" in
  let d3 = Uintb128.v "000000000000000000000000000002d6" in
  Alcotest.(check uintb128)
    "test subtraction carry over" d3 (Uintb128.sub_exn d1 d2)

let test_subtract_one_from_minint_overflows () =
  let d1 = Uintb128.min_int () in
  let d2 = Uintb128.v "00000000000000000000000000000001" in
  Alcotest.(check (option uintb128))
    "subtracting one from min_int overflows" None (Uintb128.sub d1 d2)

let test_of_to_string () =
  let s = "ff000000000000004200000000000001" in
  Alcotest.(check string)
    "input of of_string is equal to output of to_string" s
    (Uintb128.of_string_exn s |> Uintb128.to_string)

let test_lognot () =
  let d1 = Uintb128.v "00000000000000000000000000000001" in
  let d2 = Uintb128.v "fffffffffffffffffffffffffffffffe" in
  Alcotest.(check uintb128) "lognot inverts bits" d2 (Uintb128.lognot d1)

(* TODO DRY Use a List with tuples for test parameters *)
let test_shift_right () =
  let d1 = Uintb128.v "00000000000000000000000000000100" in
  let d2 = Uintb128.v "00000000000000000000000000000001" in
  Alcotest.(check uintb128) "shift right by 8" d2 (Uintb128.shift_right 8 d1);
  let d1 = Uintb128.v "f0000000000000000000000000000000" in
  let d2 = Uintb128.v "78000000000000000000000000000000" in
  Alcotest.(check uintb128) "shift right by 1" d2 (Uintb128.shift_right 1 d1);
  let d1 = Uintb128.v "f0000000000000000000000000000000" in
  let d2 = Uintb128.v "3c000000000000000000000000000000" in
  Alcotest.(check uintb128) "shift right by 2" d2 (Uintb128.shift_right 2 d1);
  let d1 = Uintb128.v "f0000000000000000000000000000000" in
  let d2 = Uintb128.v "00780000000000000000000000000000" in
  Alcotest.(check uintb128) "shift right by 9" d2 (Uintb128.shift_right 9 d1);
  let d1 = Uintb128.v "01000000000000000000000000000000" in
  let d2 = Uintb128.v "00000000000000000100000000000000" in
  Alcotest.(check uintb128) "shift right by 64" d2 (Uintb128.shift_right 64 d1);
  let d1 = Uintb128.v "80000000000000000000000000000000" in
  let d2 = Uintb128.v "00000000000000000000000000000001" in
  Alcotest.(check uintb128)
    "shift right by 127" d2
    (Uintb128.shift_right 127 d1)

let test_get_lsbits () =
  Alcotest.(check int) "get 8 lsb" 0xff (Uintb128.get_lsbits 8 0xff);
  Alcotest.(check int) "get 4 lsb" 0x0f (Uintb128.get_lsbits 4 0xff);
  Alcotest.(check int) "get 4 lsb from 0x00" 0x00 (Uintb128.get_lsbits 4 0x00);
  Alcotest.(check int) "get 5 lsb" 0x10 (Uintb128.get_lsbits 5 0x10)

let () =
  let open Alcotest in
  run "uintb128"
    [
      ( "addition",
        [
          test_case "simple addition" `Quick test_add_one_to_zero;
          test_case "test carry over" `Quick test_addition_carry;
          test_case "adding one to max_int overflows" `Quick
            test_add_one_to_maxint_overflows;
        ] );
      ( "subtraction",
        [
          test_case "simple subtraction" `Quick test_subtract_one_from_one;
          test_case "test carry over" `Quick test_subtract_carry;
          test_case "subtracting one from min_int overflows" `Quick
            test_subtract_one_from_minint_overflows;
        ] );
      ( "string-converstion",
        [ test_case "to_string of_string conversion" `Quick test_of_to_string ]
      );
      ( "bitwise logical operations",
        [
          test_case "lognot inverts bits" `Quick test_lognot;
          test_case "logical shift right" `Quick test_shift_right;
        ] );
      ( "auxiliary functions",
        [ test_case "get least significant bits" `Quick test_get_lsbits ] );
    ]
