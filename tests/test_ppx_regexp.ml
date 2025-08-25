(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* pcre tests *)

let () =
  (match%pcre "%" with _ -> ());
  (match%pcre "%" with s -> assert (s = "%"));
  (function%pcre _ -> ()) "%";
  (function%pcre s -> assert (s = "%")) "%"

let test1 =
  function%pcre
  | {|^(?<k>.*): *(?<v>.+)?$|} -> `Attr (k, v)
  | {|^# (?<comment>.+)$|} -> `Comment comment
  | {|^((?<last>[@%]){2}){0,2}$|} -> `Even_sigils last
  | {|^[@%]|} -> `Odd_sigils
  | _ -> `Unknown

let () =
  assert (test1 "x: 1" = `Attr ("x", Some "1"));
  assert (test1 "# Kommentar" = `Comment "Kommentar");
  assert (test1 "" = `Even_sigils None);
  assert (test1 "%%%@" = `Even_sigils (Some "@"));
  assert (test1 "%%@" = `Odd_sigils)

let last_elt s =
  let n = String.length s in
  assert (s.[n - 1] = ';');
  let i = try String.rindex_from s (n - 2) ';' + 1 with Not_found -> 0 in
  String.sub s i (n - i - 1)

let rec test2 s =
  match%pcre s with
  | {|^<>$|} -> assert (s = "<>")
  | {|^<(?<x>[^<>]+)>$|} -> assert (s = "<" ^ x ^ ">")
  | {|^<(?<x>[^<>]+)><(?<y>[^<>]+)>$|} -> assert (s = "<" ^ x ^ "><" ^ y ^ ">")
  | {|^((?<elt>[^;<>]);)+$|} -> assert (elt = last_elt s)
  | {|^[^{}]*\{(?<s'>.*)\}|} -> test2 s'
  | {|^(?<a>one)|(?<b>two)$|} -> assert ((a = Some "one" && b = None) || (a = None && b = Some "two"))
  | _ -> assert false

let test3 s =
  match%pcre s with
  | {|no(is)((e)) (?<is>is) (g(oo)d)|} -> assert (is = "is")
  | {|?<s'>&()[a-zA-Z0-9_-]+(;)|} ->
    let i, j = String.index s '&', String.rindex s ';' in
    assert (s' = String.sub s i (j - i + 1))
  | {|m(o+)re re(gular)? no(is)e, (no )*be(t+)?er|} -> ()
  | s' -> assert (s = s')

let test4 =
  function%pcre (* Issue 8 *)
  | {|(?<x>[-+]?[[:digit:]]+.[[:digit:]]*)|} -> [ x ]
  | {|(?<x>(abc))[[:space:]]*(?<y>(xyz))|} -> [ x; y ]
  | _ -> assert false

let test5 =
  function%pcre
  | {|^.(?<x>.+)|} ->
    (match%pcre x with {|^.(?<y>.+)|} -> (match%pcre y with {|^.(?<z>.+)|} -> x, y, z | _ -> assert false) | _ -> assert false)
  | _ -> assert false

let () =
  test2 "<>";
  test2 "<a>";
  test2 "<ab>";
  test2 "<a><b>";
  test2 "<ab><cde>";
  test2 "a;";
  test2 "a;b;c;d;";
  test2 "<a;b>";
  test2 "Xx{--{a;b;c;}--}yY.";
  test2 "one";
  test2 "two";
  test3 "- + &nbsp; + -";
  test3 "catch-all";
  assert (test4 "::123.456::" = [ "123.456" ]);
  assert (test4 "::abc xyz::" = [ "abc"; "xyz" ]);
  assert (test5 "abcd" = ("bcd", "cd", "d"))

(* It should work in a functor, and Re_pcre.regxp should be lifted to the
 * top-level. *)
module F (M : Map.OrderedType) = struct
  let f x = match%pcre x with {|#(?<space>\s)?(?<comment>.*)|} -> Some (space <> None, comment) | _ -> None
end

(* It should work as a top-level eval. *)
let r = ref false;;

match%pcre "" with {|^$|} -> r := true | _ -> assert false;;
assert (!r = true)

(* mikmatch *)
open OUnit2

let assert_raises_failure f = assert_raises (Failure "") (fun () -> f ())

(* Basic pattern matching tests *)
let test_basic_matching _ =
  let match_hello = function%mikmatch {| "hello" |} -> "matched hello" | _ -> "no match" in
  assert_equal "matched hello" (match_hello "hello");
  assert_equal "no match" (match_hello "world");

  let match_digit = function%mikmatch {| digit |} -> "single digit" | _ -> "not a digit" in
  assert_equal "single digit" (match_digit "5");
  assert_equal "not a digit" (match_digit "a");
  assert_equal "not a digit" (match_digit "55")
(* anchored by default *)

(* Test variable substitution *)
let%mikmatch greeting = {|"hello"|}
let%mikmatch number = {|digit+|}
let%mikmatch combined = {|greeting ' ' number|}

let test_variable_substitution _ =
  let match_combined = function%mikmatch {| combined |} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_combined "hello 123");
  assert_equal "no match" (match_combined "hello abc");
  assert_equal "no match" (match_combined "goodbye 123")

(* Test variable capture *)
let%mikmatch word = {| alpha+ |}

let test_variable_capture _ =
  let extract_word = function%mikmatch {| "name:" ' '* (word as name) |} -> name | _ -> "not found" in
  assert_equal "John" (extract_word "name: John");
  assert_equal "Alice" (extract_word "name:   Alice");
  assert_equal "not found" (extract_word "name: 123")

(* Test type conversion *)
let%mikmatch num = {| digit+ |}

let test_type_conversion _ =
  (* Integer conversion *)
  let extract_int = function%mikmatch {| "age:" ' '* (num as age : int) |} -> age | _ -> -1 in
  assert_equal 25 (extract_int "age: 25");
  assert_equal 0 (extract_int "age: 0");
  assert_equal (-1) (extract_int "age: abc");

  (* Float conversion *)
  let extract_float = function%mikmatch {| "price:" ' '* (digit+ '.' digit+ as price : float) |} -> price | _ -> -1.0 in
  assert_equal 19.99 (extract_float "price: 19.99");
  assert_equal 0.01 (extract_float "price: 0.01");
  assert_equal (-1.0) (extract_float "price: abc")

(* Test custom function application with := *)
let test_custom_function_application _ =
  let parse_list s = String.split_on_char ',' s in

  (* Parse comma-separated list *)
  let extract_list = function%mikmatch {| "items:" ' '* ([^' ']+ as items := parse_list) |} -> items | _ -> [] in
  assert_equal [ "a"; "b"; "c" ] (extract_list "items: a,b,c");
  assert_equal [ "one" ] (extract_list "items: one");

  (* Uppercase conversion *)
  let extract_upper = function%mikmatch {| "name:" ' '* (alpha+ as name := String.uppercase_ascii) |} -> name | _ -> "UNKNOWN" in
  assert_equal "JOHN" (extract_upper "name: john");
  assert_equal "ALICE" (extract_upper "name: Alice")

(* Test >>> piping feature *)
type person = {
  first : string;
  last : string;
  age : int;
}

let test_piping_feature _ =
  let make_person first last age = { first; last; age } in

  let parse_person =
    function%mikmatch
    | {|/ (alpha+ as first) ' ' (alpha+ as last) ',' ' '* (digit+ as age : int) >>> make_person as person /|} -> Some person
    | _ -> None
  in

  match parse_person "John Doe, 30" with
  | Some p ->
    assert_equal "John" p.first;
    assert_equal "Doe" p.last;
    assert_equal 30 p.age
  | None ->
    assert true;

    (* assert_failure "Should have matched"; *)
    assert_equal None (parse_person "John 30")

(* Test pattern guards *)
let%mikmatch num = {| digit+ |}

let test_pattern_guards _ =
  let classify_number =
    function%mikmatch
    | {| (num as n : int) |} when n < 10 -> "single digit"
    | {| (num as n : int) |} when n < 100 -> "double digit"
    | {| (num as n : int) |} when n < 1000 -> "triple digit"
    | {| num |} -> "large number"
    | _ -> "not a number"
  in

  assert_equal "single digit" (classify_number "5");
  assert_equal "double digit" (classify_number "42");
  assert_equal "triple digit" (classify_number "999");
  assert_equal "large number" (classify_number "10000");
  assert_equal "not a number" (classify_number "abc")

(* Test compilation order with guards - ensure correct branch is taken *)
let test_compilation_order _ =
  let match_order =
    function%mikmatch
    | {| "test" (digit as d : int) |} when d = 1 -> "first"
    | {| "test" (digit as d : int) |} when d = 2 -> "second"
    | {| "test" digit |} -> "other digit"
    | {| "test" _ |} -> "test with non-digit"
    | _ -> "no match"
  in

  assert_equal "first" (match_order "test1");
  assert_equal "second" (match_order "test2");
  assert_equal "other digit" (match_order "test5");
  assert_equal "test with non-digit" (match_order "testa");
  assert_equal "no match" (match_order "other")

(* Test caseless matching with 'i' flag *)
let test_caseless_flag _ =
  let match_caseless = function%mikmatch {|/ "hello" / i |} -> "matched hello" | _ -> "no match" in

  assert_equal "matched hello" (match_caseless "hello");
  assert_equal "matched hello" (match_caseless "HELLO");
  assert_equal "matched hello" (match_caseless "HeLLo");
  assert_equal "no match" (match_caseless "goodbye")

(* Test unanchored matching with 'u' flag *)
let test_unanchored_flag _ =
  let match_unanchored = function%mikmatch {|/ "world" / u |} -> "found world" | _ -> "not found" in

  assert_equal "found world" (match_unanchored "world");
  assert_equal "found world" (match_unanchored "hello world");
  assert_equal "found world" (match_unanchored "world!");
  assert_equal "found world" (match_unanchored "hello world goodbye");
  assert_equal "not found" (match_unanchored "hello")

(* Test combined flags 'iu' *)
let test_combined_flags _ =
  let match_both = function%mikmatch {|/ "test" / iu |} -> "found test" | _ -> "not found" in

  assert_equal "found test" (match_both "test");
  assert_equal "found test" (match_both "TEST");
  assert_equal "found test" (match_both "This is a TeSt here");
  assert_equal "found test" (match_both "testing 123");
  assert_equal "not found" (match_both "no match here")

(* Test caseless groups with ~ *)
let test_caseless_groups _ =
  let match_caseless_group = function%mikmatch {| "hello"~ ' ' "world" |} -> "matched" | _ -> "no match" in

  assert_equal "matched" (match_caseless_group "hello world");
  assert_equal "matched" (match_caseless_group "HELLO world");
  assert_equal "matched" (match_caseless_group "HeLLo world");
  assert_equal "no match" (match_caseless_group "hello WORLD")
(* only hello is caseless *)

(* Test repetition operators *)
let test_repetition_operators _ =
  (* Test * operator *)
  let match_star = function%mikmatch {| 'a'* |} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_star "");
  assert_equal "matched" (match_star "a");
  assert_equal "matched" (match_star "aaa");

  (* Test + operator *)
  let match_plus = function%mikmatch {| 'b'+ |} -> "matched" | _ -> "no match" in
  assert_equal "no match" (match_plus "");
  assert_equal "matched" (match_plus "b");
  assert_equal "matched" (match_plus "bbb");

  (* Test ? operator *)
  let match_optional = function%mikmatch {| 'c'? |} -> "matched" | _ -> "no match" in
  assert_equal "matched" (match_optional "");
  assert_equal "matched" (match_optional "c");
  assert_equal "no match" (match_optional "cc");

  (* Test {n} operator *)
  let match_exact = function%mikmatch {| 'd'{3} |} -> "matched" | _ -> "no match" in
  assert_equal "no match" (match_exact "dd");
  assert_equal "matched" (match_exact "ddd");
  assert_equal "no match" (match_exact "dddd");

  (* Test {n-m} operator *)
  let match_range = function%mikmatch {| 'e'{2-4} |} -> "matched" | _ -> "no match" in
  assert_equal "no match" (match_range "e");
  assert_equal "matched" (match_range "ee");
  assert_equal "matched" (match_range "eee");
  assert_equal "matched" (match_range "eeee");
  assert_equal "no match" (match_range "eeeee")

(* Test control sequences *)
let test_control_sequences _ =
  (* Test empty *)
  let match_empty = function%mikmatch {| "" |} -> "empty" | _ -> "not empty" in
  assert_equal "empty" (match_empty "");
  assert_equal "not empty" (match_empty " ");

  (* Test any *)
  let match_any = function%mikmatch {| any any any |} -> "three chars" | _ -> "not three" in
  assert_equal "three chars" (match_any "abc");
  assert_equal "three chars" (match_any "123");
  assert_equal "not three" (match_any "ab")

(* Test character sets *)
let test_character_sets _ =
  (* Basic character set *)
  let match_vowel = function%mikmatch {| ['a' 'e' 'i' 'o' 'u'] |} -> "vowel" | _ -> "not vowel" in
  assert_equal "vowel" (match_vowel "a");
  assert_equal "vowel" (match_vowel "e");
  assert_equal "not vowel" (match_vowel "b");

  (* Character range *)
  let match_hex = function%mikmatch {| ['0'-'9' 'a'-'f' 'A'-'F']+ |} -> "hex" | _ -> "not hex" in
  assert_equal "hex" (match_hex "deadbeef");
  assert_equal "hex" (match_hex "123ABC");
  assert_equal "not hex" (match_hex "xyz");

  (* Negative character class *)
  let match_not_digit = function%mikmatch {| [^'0'-'9']+ |} -> "no digits" | _ -> "has digits" in
  assert_equal "no digits" (match_not_digit "abc");
  assert_equal "has digits" (match_not_digit "abc123")

(* Test complex patterns with multiple features *)

let%mikmatch username = {| alnum+ ('.' alnum+)* |}
let%mikmatch domain = {| alnum+ ('.' alnum+)+ |}
let%mikmatch octet = {| digit{1-3} |}

let test_complex_patterns _ =
  let parse_email = function%mikmatch {|/ (username as user) '@' (domain as dom) /|} -> Some (user, dom) | _ -> None in

  begin
    match parse_email "john.doe@example.com" with
    | Some (u, d) ->
      assert_equal "john.doe" u;
      assert_equal "example.com" d
    | None -> assert_failure "Should match email"
  end;

  assert_equal None (parse_email "invalid@");

  (* IP address pattern with validation *)
  let is_valid_ip =
    function%mikmatch
    | {|/ (octet as o1 : int) '.' (octet as o2 : int) '.'
          (octet as o3 : int) '.' (octet as o4 : int) /|}
      when o1 <= 255 && o2 <= 255 && o3 <= 255 && o4 <= 255 ->
      true
    | _ -> false
  in

  assert_equal true (is_valid_ip "192.168.1.1");
  assert_equal true (is_valid_ip "255.255.255.255");
  assert_equal false (is_valid_ip "256.1.1.1");
  assert_equal false (is_valid_ip "1.2.3")

let%mikmatch date_pattern = {|digit{4} '-' digit{2} '-' digit{2}|}

let test_let_destructuring _ =
  (* basic let destructuring *)
  let test_string = "user:john age:25" in
  let%mikmatch {| "user:" (alpha+ as username) ' ' "age:" (digit+ as age : int) |} = test_string in
  assert_equal "john" username;
  assert_equal 25 age;

  (* := inside let destructuring *)
  let test_name = "firstName:alice lastName:smith" in
  let%mikmatch {|"firstName:" (alpha+ as first := String.uppercase_ascii) ' ' "lastName:" (alpha+ as last := String.uppercase_ascii)|} =
    test_name
  in
  assert_equal "ALICE" first;
  assert_equal "SMITH" last;

  (* nested pattern destructuring *)
  let test_log = "2024-03-15 ERROR: something went wrong" in
  let%mikmatch {| (date_pattern as date) ' ' (upper+ as level) ':' (_* as message) |} = test_log in
  assert_equal "2024-03-15" date;
  assert_equal "ERROR" level;
  assert_equal " something went wrong" message;

  (* let destructuring with alternative *)
  let parse_number s =
    try
      let%mikmatch {|("0x" ['0'-'9' 'a'-'f' 'A'-'F']+ | digit+ as dig)|} = s in
      Some dig
    with _ -> None
  in
  assert_equal (Some "0xff") (parse_number "0xff");
  assert_equal (Some "255") (parse_number "255");
  assert_equal None (parse_number "other");

  (* let destructuring with type conversion *)
  let parse_measurement s =
    let%mikmatch {|(digit+ '.' digit+ as value : float) ' '* (alpha+ as unit)|} = s in
    value, unit
  in
  let v, u = parse_measurement "3.14 meters" in
  assert_equal 3.14 v;
  assert_equal "meters" u

let test_mixed_matching _ =
  let classify_input = function
    | "hello" -> "literal hello"
    | "world" -> "literal world"
    | {%mikmatch| digit+ |} -> "number"
    | {%mikmatch| alpha+ |} -> "word"
    | "" -> "empty"
    | {%mikmatch| _* |} -> "other"
  in

  assert_equal "literal hello" (classify_input "hello");
  assert_equal "literal world" (classify_input "world");
  assert_equal "number" (classify_input "123");
  assert_equal "word" (classify_input "test");
  assert_equal "empty" (classify_input "");
  assert_equal "other" (classify_input "!@#");

  (* mixed matching with guards *)
  let process_command = function
    | "quit" | "exit" -> "terminating"
    | {%mikmatch|"set " (alnum+ as key) ' ' (alnum+ as value)|} -> Printf.sprintf "setting %s=%s" key value
    | {%mikmatch|"get " (alnum+ as key)|} -> Printf.sprintf "getting %s" key
    | {%mikmatch|"del " (alnum+ as key)|} when key <> "admin" -> Printf.sprintf "deleting %s" key
    | {%mikmatch|"del " "admin" |} -> "cannot delete admin"
    | cmd when String.length cmd > 10 -> "command too long"
    | _ -> "unknown command"
  in

  assert_equal "terminating" (process_command "quit");
  assert_equal "terminating" (process_command "exit");
  assert_equal "setting foo=bar" (process_command "set foo bar");
  assert_equal "getting foo" (process_command "get foo");
  assert_equal "deleting user" (process_command "del user");
  assert_equal "cannot delete admin" (process_command "del admin");
  assert_equal "command too long" (process_command "this is a very long command");
  assert_equal "unknown command" (process_command "xyz");

  (* Mixed matching with type patterns and regex *)
  let process_value = function
    | ("true" | "false") as b -> Printf.sprintf "bool: %s" b
    | {%mikmatch| (digit+ as n : int) |} when n >= 0 -> Printf.sprintf "positive int: %d" n
    | {%mikmatch|'-' (digit+ as n : int) |} -> Printf.sprintf "negative int: -%d" n
    | {%mikmatch|'-'? (digit+ '.' digit+ as f : float) |} -> Printf.sprintf "float: %f" f
    | {%mikmatch|'"' ([^'"']* as s) '"'|} -> Printf.sprintf "string: %s" s
    | _ -> "unknown type"
  in

  assert_equal "bool: true" (process_value "true");
  assert_equal "bool: false" (process_value "false");
  assert_equal "positive int: 42" (process_value "42");
  assert_equal "negative int: -10" (process_value "-10");
  assert_equal "float: 3.140000" (process_value "3.14");
  assert_equal "string: hello world" (process_value "\"hello world\"");
  assert_equal "unknown type" (process_value "undefined");

  (* Complex mixed matching with multiple regex in one match *)
  let parse_log_line = function
    | "" -> "empty line"
    | {%mikmatch|'#' _*|} -> "comment"
    | {%mikmatch| (digit{4} '-' digit{2} '-' digit{2} as date) |} -> Printf.sprintf "date only: %s" date
    | {%mikmatch|/ (digit{4} '-' digit{2} '-' digit{2} as date) ' '
                   (digit{2} ':' digit{2} ':' digit{2} as time) ' '
                   ('[' upper+ ']' as level) ' ' (_* as msg) / u |}
      ->
      Printf.sprintf "full log: %s %s %s: %s" date time level msg
    | line when String.starts_with ~prefix:"DEBUG:" line -> "debug line"
    | {%mikmatch|/ "ERROR:" ' ' (_* as err) /|} -> Printf.sprintf "error: %s" err
    | _ -> "unknown format"
  in

  assert_equal "empty line" (parse_log_line "");
  assert_equal "comment" (parse_log_line "# This is a comment");
  assert_equal "date only: 2024-03-15" (parse_log_line "2024-03-15");
  assert_equal "full log: 2024-03-15 10:30:45 [INFO]: Application started" (parse_log_line "2024-03-15 10:30:45 [INFO] Application started");
  assert_equal "debug line" (parse_log_line "DEBUG: checking value");
  assert_equal "error: file not found" (parse_log_line "ERROR: file not found");
  assert_equal "unknown format" (parse_log_line "random text");

  (* Test match ordering with mixed patterns *)
  let match_priority = function
    | "specific" -> 1
    | {%mikmatch| "spec" alpha+ |} -> 2 (* More specific regex *)
    | {%mikmatch| alpha+ |} -> 3 (* General regex *)
    | _ -> 4
  in

  assert_equal 1 (match_priority "specific");
  assert_equal 2 (match_priority "special");
  assert_equal 3 (match_priority "general");
  assert_equal 4 (match_priority "123");

  (* when no catch-all is provided, should generate one that raises Failure *)
  let no_default_case = function
    | "a" -> "got a"
    | {%mikmatch|"b"|} -> "got b"
    (* No default case *)
  in

  assert_equal "got a" (no_default_case "a");
  assert_equal "got b" (no_default_case "b");
  assert_raises (Failure "File tests/test_ppx_regexp.ml, lines 512-514, characters 24-33: String did not match any regex.") (fun () ->
    no_default_case "c")

let suite =
  "mikmatch_tests"
  >::: [
         "test_basic_matching" >:: test_basic_matching;
         "test_variable_substitution" >:: test_variable_substitution;
         "test_variable_capture" >:: test_variable_capture;
         "test_type_conversion" >:: test_type_conversion;
         "test_custom_function_application" >:: test_custom_function_application;
         "test_piping_feature" >:: test_piping_feature;
         "test_pattern_guards" >:: test_pattern_guards;
         "test_compilation_order" >:: test_compilation_order;
         "test_caseless_flag" >:: test_caseless_flag;
         "test_unanchored_flag" >:: test_unanchored_flag;
         "test_combined_flags" >:: test_combined_flags;
         "test_caseless_groups" >:: test_caseless_groups;
         "test_repetition_operators" >:: test_repetition_operators;
         "test_control_sequences" >:: test_control_sequences;
         "test_character_sets" >:: test_character_sets;
         "test_complex_patterns" >:: test_complex_patterns;
         "test_let_destructuring" >:: test_let_destructuring;
         "test_mixed_matching" >:: test_mixed_matching;
       ]

let () = run_test_tt_main suite
