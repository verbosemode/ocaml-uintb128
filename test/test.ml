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

let test_shift_right () =
  (* (bit shift count, input, expected output) *)
  let test_rshifts = [
    (1, "f0000000000000000000000000000000", "78000000000000000000000000000000");
    (2, "f0000000000000000000000000000000", "3c000000000000000000000000000000");
    (2, "0000000000000000000000000000ffff", "00000000000000000000000000003fff");
    (2, "000000000000000000000000000ffff0", "0000000000000000000000000003fffc");
    (8, "00000000000000000000000000000100", "00000000000000000000000000000001");
    (9, "f0000000000000000000000000000000", "00780000000000000000000000000000");
    (64, "01000000000000000000000000000000", "00000000000000000100000000000000");
    (127, "80000000000000000000000000000000", "00000000000000000000000000000001");
    (128, "ffff0000000000000000000000000000", "00000000000000000000000000000000");
  ]
  in
  let open Uintb128 in
  List.iter (fun (bits, input_value, expected_output) ->
    Alcotest.(check uintb128) (Printf.sprintf "shift right by %i" bits)
    (of_string_exn expected_output)
    (shift_right bits (of_string_exn input_value))
  ) test_rshifts

let test_shift_left () =
  (* (bit shift count, input, expected output) *)
  let test_lshifts = [
    (1, "f0000000000000000000000000000000", "e0000000000000000000000000000000");
    (1, "0000000000000000000000000000000f", "0000000000000000000000000000001e");
    (1, "00000000000000000000000000000001", "00000000000000000000000000000002");
    (2, "f0000000000000000000000000000000", "c0000000000000000000000000000000");
    (8, "00000000000000000000000000000100", "00000000000000000000000000010000");
    (9, "f0000000000000000000000000000000", "00000000000000000000000000000000");
    (64, "00000000000000000000000000000001", "00000000000000010000000000000000");
    (127, "00000000000000000000000000000001", "80000000000000000000000000000000");
    (128, "00000000000000000000000000000001", "00000000000000000000000000000000");
  ]
  in
  let open Uintb128 in
  List.iter (fun (bits, input_value, expected_output) ->
    Alcotest.(check uintb128) (Printf.sprintf "shift left by %i" bits)
    (of_string_exn expected_output)
    (shift_left bits (of_string_exn input_value))
  ) test_lshifts


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
          test_case "logical shift left" `Quick test_shift_left;
        ] );
      ( "auxiliary functions",
        [ test_case "get least significant bits" `Quick test_get_lsbits ] );
    ]
