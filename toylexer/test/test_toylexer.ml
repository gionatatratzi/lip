open OUnit2
open Lexer

(* Define mock tokens based on lexer.mll token output *)
type token =
  | ATOK of string
  | BTOK of string
  | CTOK of string
  | DTOK of string
  | ETOK of string

(* Helper function to convert token to string for comparison *)
let string_of_token = function
  | ATOK s -> "ATOK(" ^ s ^ ")"
  | BTOK s -> "BTOK(" ^ s ^ ")"
  | CTOK s -> "CTOK(" ^ s ^ ")"
  | DTOK s -> "DTOK(" ^ s ^ ")"
  | ETOK s -> "ETOK(" ^ s ^ ")"

(* Test ATOK token *)
let test_atok _ =
  let result = Lexer.lex "Abc123" in
  assert_equal (ATOK "Abc123") result ~printer:string_of_token

(* Test BTOK token *)
let test_btok _ =
  let result = Lexer.lex "aeiou" in
  assert_equal (BTOK "aeiou") result ~printer:string_of_token

(* Test CTOK token *)
let test_ctok _ =
  let result = Lexer.lex "bcdfg" in
  assert_equal (CTOK "bcdfg") result ~printer:string_of_token

(* Test DTOK token *)
let test_dtok _ =
  let result = Lexer.lex "-3.14" in
  assert_equal (DTOK "-3.14") result ~printer:string_of_token

(* Test ETOK token *)
let test_etok _ =
  let result = Lexer.lex "0x1A3F" in
  assert_equal (ETOK "0x1A3F") result ~printer:string_of_token

(* Define the test suite *)
let suite =
  "Lexer tests" >::: [
    "test_atok" >:: test_atok;
    "test_btok" >:: test_btok;
    "test_ctok" >:: test_ctok;
    "test_dtok" >:: test_dtok;
    "test_etok" >:: test_etok;
  ]

(* Run the test suite *)
let () = run_test_tt_main suite


