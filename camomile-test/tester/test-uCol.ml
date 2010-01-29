(* $Id: test-uCol.ml,v 1.13 2006/08/13 21:23:08 yori Exp $ *)
(* Copyright 2002,2003,2004,2005,2006 Yamagata Yoriyuki *)

open CamomileLibrary.Main.Camomile
open UPervasives
open Blender
open Printf

let rec lex_compare_aux i t1 t2 =
  if i >= UText.length t1 then
    if i >= UText.length t2 then 0 else ~-1
  else if i >= UText.length t2 then 1 else
  match Pervasives.compare (UText.get t1 i) (UText.get t2 i) with
    0 -> lex_compare_aux (i + 1) t1 t2
  | sgn -> sgn

let lex_compare t1 t2 = lex_compare_aux 0 t1 t2
  
let blank = Str.regexp "[ \t]+"
let line_pat = Str.regexp "\\([^;]+\\);.*$"
let comment_pat = Str.regexp "^#.*"

let uchar_of_code code = 
  uchar_of_int (int_of_string ("0x"^code))

let us_of_cs cs = List.map uchar_of_code cs

let parse_line line =
  if Str.string_match line_pat line 0 then
    let s = Str.matched_group 1 line in
    let cs = Str.split blank s in
    let us = us_of_cs cs in
    UText.init (List.length us) (fun i -> List.nth us i)
  else invalid_arg (sprintf "Malformed_line %s:" line)

let print_char u = sprintf "%04X " (int_of_uchar u)

let print_text t =
  let buf = Buffer.create (5 * UText.length t) in
  UText.iter (fun u -> Buffer.add_string buf (print_char u)) t;
  Buffer.contents buf

let sgn_of i =
  if i < 0 then -1 else
  if i = 0 then 0 else
  if i > 0 then 1 else assert false

module Ucomp = UCol.Make ((UText : UnicodeString.Type with type t = UText.t))

let uca ~desc variable c =
  let prev = ref (UText.init 0 (fun _ -> uchar_of_int 0)) in
  let prev_key = ref (Ucomp.sort_key ~variable !prev) in
  let prev_line = ref "" in
  try while true do
    let line = input_line c in
    if Str.string_match comment_pat line 0 then () else
    let t = parse_line line in
    let t_key = Ucomp.sort_key ~variable t in
    let sgn = compare !prev_key t_key in
    let sgn1 = Ucomp.compare ~variable !prev t in
    let sgn2 = Ucomp.compare_with_key ~variable !prev_key t in
    let sgn3 = ~- (Ucomp.compare_with_key ~variable t_key !prev) in
    test ~desc ~body:(fun () -> expect_pass (fun () ->
      expect_true
	~msg:(lazy (sprintf 
		      "the previous line is greater than the current:\n\
		      value: %i\n\
		      previous line:%s\n\
		      %s \n\
		      key %s\n\
		      current lins:%s\n\
		      %s \n\
		      key %s\n"
		      sgn 
		      !prev_line 
		      (print_text !prev) 
		      (String.escaped !prev_key)
		      line 
		      (print_text t) 
		      (String.escaped t_key)))
	(sgn <= 0);
      if sgn = 0 then
	expect_true
	  ~msg:(lazy (sprintf
			"the previous line and the current are equal but\
			code point order is not correct.\n\
			previous line:%s\n\
			%s \n\
			key %s\n\
			current lins:%s\n\
			%s \n\
			key %s\n"
			!prev_line 
			(print_text !prev) 
			(String.escaped !prev_key)
			line 
			(print_text t) 
			(String.escaped t_key)))
	  (lex_compare !prev t <= 0);
      expect_true
	~msg:(lazy (sprintf
		      "comparison by compare is different from \
		      comparison by keys.\n\
		      value by compare: %i\n\
		      value by sort key: %i\n\
		      previous line:%s\n\
		      %s \n\
		      key %s\n\
		      current lins:%s\n\
		      %s \n\
		      key %s\n"
		      sgn1 
		      sgn 
		      !prev_line 
		      (print_text !prev) 
		      (String.escaped !prev_key)
		      line 
		      (print_text t) 
		      (String.escaped t_key)))
	(sgn > 0 && sgn1 > 0 || sgn = 0 && sgn1 = 0 || sgn < 0 && sgn1 < 0);
      expect_true
	~msg:(lazy (sprintf
		      "comparison by compare_with_key prev_key current \
		      is different from comparison by keys.\n\
		      value by compare_with_key prev_key current: %i\n\
		      value by sort key: %i\n\
		      previous line:%s\n\
		      %s \n\
		      key %s\n\
		      current lins:%s\n\
		      %s \n\
		      key %s\n"
		      sgn2 
		      sgn 
		      !prev_line 
		      (print_text !prev) 
		      (String.escaped !prev_key)
		      line 
		      (print_text t) 
		      (String.escaped t_key)))
	(sgn > 0 && sgn2 > 0 || sgn = 0 && sgn2 = 0 || sgn < 0 && sgn2 < 0);
      expect_true
	~msg:(lazy (sprintf
		      "comparison by compare_with_key current_key prev \
		      is different from comparison by keys.\n\
		      value by compare_with_key current_key prev: %i\n\
		      value by sort key: %i\n\
		      previous line:%s\n\
		      %s \n\
		      key %s\n\
		      current lins:%s\n\
		      %s \n\
		      key %s\n"
		      sgn3 
		      sgn 
		      !prev_line 
		      (print_text !prev) 
		      (String.escaped !prev_key)
		      line 
		      (print_text t) 
		      (String.escaped t_key)))
	(sgn > 0 && sgn3 > 0 || sgn = 0 && sgn3 = 0 || sgn < 0 && sgn3 < 0)));
    prev := t;
    prev_key := t_key;
    prev_line := line
  done with End_of_file -> ()

let _ = read_file 
    (input_filename "unidata/CollationTest_SHIFTED.txt")
    (uca ~desc:"Shifted" `Shifted)

let _ = read_file 
    (input_filename "unidata/CollationTest_NON_IGNORABLE.txt")
    (uca ~desc:"Non ignorable" `Non_ignorable)

module UTF8Comp = UCol.Make (UTF8)

let print_text_utf8 t =
  let buf = Buffer.create (5 * UTF8.length t) in
  UTF8.iter (fun u -> Buffer.add_string buf (print_char u)) t;
  Buffer.contents buf

let locale_test ~desc ?variable ~locale c =
  let prev = ref "" in
  let prev_key = ref (UTF8Comp.sort_key ?variable ~locale "") in
  try while true do
    let line = input_line c in
    if Str.string_match comment_pat line 0 then () else
    let key = UTF8Comp.sort_key ?variable ~locale line in
    let sgn = sgn_of (UTF8Comp.compare ?variable ~locale !prev line) in
    let sgn1 = sgn_of (Pervasives.compare !prev_key key) in
    let sgn2 = sgn_of 
	(UTF8Comp.compare_with_key ?variable ~locale !prev_key line) in
    let sgn3 = - sgn_of 
	(UTF8Comp.compare_with_key ?variable ~locale key !prev) in
    test ~desc ~body:(fun () -> expect_pass (fun () ->
      expect_true
	~msg:(lazy (sprintf 
		      "the previous key is greater than the current:\n\
		      value: %i\n\
		      previous: %s \n\
		      code : %s \n\
		      key %s\n\
		      current: %s \n\
		      code : %s \n\
		      key %s\n"
		      sgn 
		      !prev 
		      (print_text_utf8 !prev) 
		      (String.escaped !prev_key)
		      line 
		      (print_text_utf8 line) 
		      (String.escaped key)))
	(sgn1 <= 0);
      expect_true
	~msg:(lazy (sprintf 
		      "The comparison results differ\n\
		      value: %i\n\
		      previous: %s \n\
		      code : %s \n\
		      key %s\n\
		      current: %s \n\
		      code : %s \n\
		      key %s\n\
		      previous - current comparison : %d\n\
		      previous key - current key comparison : %d\n\
		      previous key - current comparison : %d\n\
		      previous - current key comparison : %d\n"
		      sgn 
		      !prev 
		      (print_text_utf8 !prev) 
		      (String.escaped !prev_key)
		      line 
		      (print_text_utf8 line) 
		      (String.escaped key)
		      sgn
		      sgn1
		      sgn2
		      sgn3))
	(sgn = sgn1 && sgn1 = sgn2 && sgn2 = sgn3)));
    prev := line;
    prev_key := key
  done with End_of_file -> ()

let _ = read_file
    (input_filename "data/fr_CA")
    (locale_test 
       ~desc:"Canadian French" 
       ~variable:`Shift_Trimmed
       ~locale:"fr_CA")

let _ = read_file
    (input_filename "data/th18057")
    (locale_test 
       ~desc:"Thai" 
       ~variable:`Non_ignorable
       ~locale:"th_TH")


let test_list ~desc ?variable ~locale list =
  let rec loop prev prev_key = function
      [] -> ()
    | t :: rest ->
    let key = UTF8Comp.sort_key ?variable ~locale t in
    let sgn = sgn_of (UTF8Comp.compare ?variable ~locale prev t) in
    let sgn1 = sgn_of (Pervasives.compare prev_key key) in
    let sgn2 = sgn_of
	(UTF8Comp.compare_with_key ?variable ~locale prev_key t) in
    let sgn3 = - sgn_of
	(UTF8Comp.compare_with_key ?variable ~locale key prev) in
    test ~desc ~body:(fun () -> expect_pass (fun () ->
      expect_true
	~msg:(lazy (sprintf 
		      "the previous key is greater than the current:\n\
		      value: %i\n\
		      previous: %s \n\
		      code : %s \n\
		      key %s\n\
		      current: %s \n\
		      code : %s \n\
		      key %s\n"
		      sgn 
		      prev 
		      (print_text_utf8 prev) 
		      (String.escaped prev_key)
		      t
		      (print_text_utf8 t) 
		      (String.escaped key)))
	(sgn1 < 0);
      if sgn1 = 0 then
	expect_true
	  ~msg:(lazy (sprintf
			"the previous line and the current are equal but\
			code point order is not correct.\n\
			previous line:%s\n\
			%s \n\
			key %s\n\
			current lins:%s\n\
			%s \n\
			key %s\n"
			prev
			(print_text_utf8 prev) 
			(String.escaped prev_key)
			t 
			(print_text_utf8 t) 
			(String.escaped key)))
	  (Pervasives.compare prev t <= 0);
      expect_true
	~msg:(lazy (sprintf 
		      "The comparison results differ\n\
		      value: %i\n\
		      previous: %s \n\
		      code : %s \n\
		      key %s\n\
		      current: %s \n\
		      code : %s \n\
		      key %s\n
		      previous - current comparison : %d\n
		      previous key - current key comparison : %d\n
		      previous key - current comparison : %d\n
		      previous - current key comparison : %d\n"
		      sgn 
		      prev 
		      (print_text_utf8 prev) 
		      (String.escaped prev_key)
		      t
		      (print_text_utf8 t) 
		      (String.escaped key)
		      sgn
		      sgn1
		      sgn2
		      sgn3))
	(sgn = sgn1 && sgn1 = sgn2 && sgn2 = sgn3)));
    loop t key rest in
  loop "" (UTF8Comp.sort_key ?variable ~locale "") list


(* Test for Scandinavian languages*)

let () =
  test_list
    ~desc:"German: ä<b"
    ~locale:"de"
    ["a";  "ä"; "b"; "z"]


let () =
  test_list
    ~desc:"Finish: b<ä"
    ~locale:"fi_FI"
    ["a"; "b"; "z"; "ä"]

let () =
  test_list
    ~desc:"German: Ä<B"
    ~locale:"de"
    ["A";  "Ä"; "B"; "Z"]

let () =
  test_list
    ~desc:"Finish: B<Ä"
    ~locale:"fi_FI"
    ["A"; "B"; "Z"; "Ä"]


let jisx4061_test1 =
  ["シャーレ";
   "シャイ";
   "シヤィ";
   "シャレ";
   "ちょこ";
   "ちよこ";
   "チョコレート";
   "てーた";
   "テータ";
   "テェタ";
   "てえた";
   "でーた"; 
   "データ";
   "デェタ";
   "でえた";
   "てーたー";
   "テータァ";
   "テェター";
   "てぇたぁ";
   "てえたー";
   "でーたー";
   "データァ";
   "でェたァ";
   "デぇタぁ";
   "デエタア";
   "ひゆ";
   "びゅあ";
   "ぴゅあ";
   "びゅあー";
   "ビュアー";
   "ぴゅあー";
   "ピュアー";
   "ヒュウ";
   "ヒユウ";
   "ピュウア";
   "びゅーあー"; 
   "ビューアー";
   "ビュウアー";
   "ひゅん";
   "ぴゅん";
   "ふーり";
   "フーリ";
   "ふぅり";
   "ふゥり";
   "ふゥリ";
   "フウリ";
   "ぶーり";
   "ブーリ";
   "ぶぅり";
   "ブゥり";
   "ぷうり";
   "プウリ";
   "ふーりー";
   "フゥリー";
   "ふゥりィ";
   "フぅりぃ";
   "フウリー";
   "ふうりぃ";
   "ブウリイ";
   "ぷーりー";
   "ぷゥりイ";
   "ぷうりー";
   "プウリイ";
   "フヽ";
   "ふゞ";
   "ぶゝ";
   "ぶふ";
   "ぶフ";
   "ブふ";
   "ブフ";
   "ぶゞ";
   "ぶぷ";
   "ブぷ";
   "ぷゝ";
   "プヽ";
   "ぷふ";]

let jisx4061_test2 =
  ["はゝ"; 
   "はは"; 
   "はハ"; 
   "ハハ"; 
   "はゞ"; 
   "ハヾ"; 
   "はば"; 
   "ハバ"; 
   "はぱ"; 
   "ハぱ"; 
   "ハパ"; 
   "ばゝ"; 
   "バヽ"; 
   "ばは"; 
   "バハ"; 
   "ばゞ"; 
   "バヾ"; 
   "ばば"; 
   "バば"; 
   "ババ"; 
   "ばぱ"; 
   "バパ"; 
   "ぱゝ"; 
   "パヽ"; 
   "ぱは"; 
   "パハ"; 
   "ぱば"; 
   "ぱバ"; 
   "パバ"; 
   "ぱぱ"; 
   "パパ";]

let () = test_list ~desc:"JISX 4061 test1" ~locale:"ja" jisx4061_test1
let () = test_list ~desc:"JISX 4061 test2" ~locale:"ja" jisx4061_test2

let () = 
  test_list 
    ~desc:"test_1 y < x" 
    ~locale:"test_1"
    ["aaaaXbbbb"; "aaaaybbbb"; "aaaaxbbbb"; "aaaaZbbbb"; "aaaazbbbb"]

let () =
  test_list
    ~desc:"test_1 i <<< い << ii = i*"
    ~locale:"test_1"
    ["bbbbiccc"; "bbbbいccc"; "bbbbiiccc"; "bbbbi*dcc"]

let () =
  test_list
    ~desc:"test_1 &[before 3]I <<< イ"
    ~locale:"test_1"
    ["aaaahbbbb"; "aaaaイbbbb"; "aaaaIbbbb"]

let () =
  test_list
    ~desc:"test_1 &[before 3]UE <<< ウェ <<< UE <<< ue"
    ~locale:"test_1"
    ["aaaaudbbbb";
     "aaaaウェbbbb"; 
     "aaaaUEbbbb"; 
     "aaaauebbbb";
     "aaaaUFbbbb"]
 
let () =
  test_list
    ~desc:"test_1 &[before 3]V <<< ヴェ/ェ = V <<< v"
    ~locale:"test_1"
     ["aaaaVェabbb";
      "aaaaヴェbbbb";
      "aaaaVェcbbb";
      "aaaavェcbbb"]

let () =
  test_list
    ~desc:"test_1 &b <<< b|*"
    ~locale:"test_1"
    ["aaaabbbb";
     "aaaab*b*";
     "aaaabcbc"]

let () =
  test_list
    ~desc:"test_1 &皇帝 < emperor"
    ~locale:"test_1"
    ["anon_皇帝";
     "anon_emperor"]
