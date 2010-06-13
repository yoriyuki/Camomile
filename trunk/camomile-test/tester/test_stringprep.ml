(* Copyright 2010 Pierre Chambart *)

open Blender
open CamomileLibrary.Default.Camomile
module M = StringPrep.Make(UTF8);;
open M;;

let char_of_string s =
  UChar.chr_of_uint (int_of_string ( "0x"^s ))

let type_pat = Str.regexp "[ \\t]*\\([EDBP]\\);"
let profile_pat = Str.regexp "[ \\t]*\\(node\\|name\\|res\\|sasl\\|trace\\|iscsi\\|mib\\);"
let val_pat = Str.regexp "[ \\t]*\\([0-9a-fA-F]+\\)"
let sep_pat = Str.regexp "[ \\t]*;"

type test =
  | E of ( UTF8.t * UTF8.t )
  | D of ( UTF8.t * UTF8.t )
  | B of UTF8.t
  | P of UTF8.t

let text_of_uchars l =
  let b = UTF8.Buf.create 0 in
  List.iter
    (fun x -> UTF8.Buf.add_char b x)
    l;
  UTF8.Buf.contents b

let parse_single s p =
  let rec f p =
    if Str.string_match sep_pat s p
    then []
    else
      if Str.string_match val_pat s p
      then
	let c = char_of_string (Str.matched_group 1 s) in
	c::(f (Str.match_end ()))
      else failwith "parse error"
  in
  text_of_uchars (f p)

let parse_couple s p =
  let s1 = parse_single s p in
  let p = Str.match_end () in
  let s2 = parse_single s p in
  s1,s2

let parse_type s p =
  if Str.string_match type_pat s p
  then
    ( match Str.matched_group 1 s with
	| "E" -> E (parse_couple s (Str.match_end ()))
	| "D" -> D (parse_couple s (Str.match_end ()))
	| "B" -> B (parse_single s (Str.match_end ()))
	| "P" -> P (parse_single s (Str.match_end ()))
	| _ -> failwith "wrong test type")
  else failwith "parse test error"

let profile_of_string = function
  | "node" -> `Nodeprep
  | "name" -> `Nameprep
  | "res" -> `Resourceprep
  | "sasl" -> `Saslprep
  | "trace" -> `Trace
  | "iscsi" -> `Iscsi
  | "mib" -> `Mib
  | _ -> failwith "profile parse error"

let parse_line s =
  if Str.string_match profile_pat s 0
  then Some
    (
      let profile = profile_of_string (Str.matched_group 1 s) in
      let p = Str.match_end () in
      let t = parse_type s p in
      profile,t
    )
  else None

let check_line n s =
  match parse_line s with
    | None -> 0
    | Some (profile,t) ->
      ( match t with
	| E (s1,s2) ->
	  expect_equal ~msg:(lazy ("line: " ^ (string_of_int n))) (stringprep profile s1) (stringprep profile s2)
	| D (s1,s2) ->
	  expect_true ~msg:(lazy ("line: " ^ (string_of_int n))) ((stringprep profile s1) <> (stringprep profile s2))
	| B s ->
	  expect_true ~msg:(lazy ("line: " ^ (string_of_int n))) ( try ignore (stringprep profile s);false with | Bad_bidi -> true | _ -> false )
	| P s -> 
	  expect_true ~msg:(lazy ("line: " ^ (string_of_int n))) ( try ignore (stringprep profile s);false with | Prohibited _ -> true | _ -> false ));
      1

exception Ok of string

let check_file f =
  let c = open_in f in
  let rec check line n =
    try
      raise (Ok (input_line c))
    with
      | End_of_file -> n
      | Ok s ->
	  let n = n + (check_line line s) in
	  check (line+1) n
  in
  let _ = check 1 0 in
  close_in c

let testfile = "data/stringprep"

let _ =
  test
    ~desc:"stringprep"
    ~body:(fun () ->
      expect_pass
	~body:(fun () -> check_file testfile ) )

