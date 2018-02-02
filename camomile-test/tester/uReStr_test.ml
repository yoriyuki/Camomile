(* Tests for UReStr *)
open CamomileLibraryTest.Camomile
open Blender
open Printf

module URegexp = UReStr.Make (UTF8)

let regexp_match_str r t i =

  let n =
    let p = Str.regexp_string "\\(" in
    let rec count i c =
      match try Some (Str.search_forward p r i) with Not_found -> None with
	Some i -> count (i + 1) (c + 1)
      | None -> c in
    count i 0 in

  let g = Array.make (n + 1) None in

  if Str.string_match (Str.regexp r) t i then

    let j = Str.match_end () in
    g.(0) <- Some (URegexp.SubText.refer t i j);

    for i = 1 to n do
      g.(i) <-
	try 
	  let j1 = Str.group_beginning i in
	  let j2 = Str.group_end i in
	  Some (URegexp.SubText.refer t j1 j2)
	with Not_found ->
	  None
    done;

    Some g

  else
    None
	
let string_of_sub s =
  let s0 = URegexp.SubText.excerpt s in
  let _, i, _ = URegexp.SubText.context s in
  sprintf "(%d, \"%s\")" i s0

let string_of_subs g =
  let b = Buffer.create 0 in
  Buffer.add_string b "[|";
  for i = 0 to Array.length g - 1 do
    Buffer.add_string b 
      (match g.(i) with
	Some s -> string_of_sub s
      | None -> " ");
    Buffer.add_string b "; "
  done;
  Buffer.add_string b "|]";
  Buffer.contents b

let printer = function
    Some g -> string_of_subs g
  | None -> "No match"

let eq_match ?(pos=0) r t =
  let r' = UReStr.regexp r in
  expect_equal_app
    ~msg:(lazy t)
    ~printer
    (fun () -> regexp_match_str r t pos) ()
    (fun () -> URegexp.regexp_match (URegexp.compile r') t pos) ()

let eq_matched_string ?(pos=0) ?(sem=`Longest) r t t0 =
  let r = UReStr.regexp r in
  let g = URegexp.regexp_match ~sem (URegexp.compile r) t pos in
  match g with
    None -> fail ("No match: " ^ t)
  | Some g -> match g.(0) with None -> assert false | Some s ->
      let t = URegexp.SubText.excerpt s in
      expect_equal ~msg:(lazy t)~printer:(fun x -> x) t0 t

let expect_pass desc body =
  test ~desc ~body:(fun () -> expect_pass ~body)

let expect_match r t =
  let r = UReStr.regexp r in
  let r = URegexp.compile r in
  expect_true ~msg:(lazy t) (URegexp.string_match r t 0)

let expect_not_match r t =
  let r = UReStr.regexp r in
  let r = URegexp.compile r in
  expect_true ~msg:(lazy t) (not (URegexp.string_match r t 0))

let _ =

(* Literal Match *)
expect_pass "literal" (fun () ->
  eq_match  "a"                 "a";
  eq_match  "a"                 "b";
);

(* Basic Operations *)

expect_pass "alt" (fun () ->
  eq_match  "a\\|b"              "a";
  eq_match  "a\\|b"              "b";
  eq_match  "a\\|b"              "c";
);

expect_pass "seq" (fun () ->
  eq_match  "ab"                "ab";
  eq_match  "ab"                "ac";
);

expect_pass "epsilon" (fun () ->
  eq_match  ""                  "";
  eq_match  ""                  "a";
);

expect_pass "rep" (fun () ->
  eq_match  "a*"                "";
  eq_match  "a*"                "a";
  eq_match  "a*"                "aa";
  eq_match  "a*"                "b";
);

expect_pass "rep1" (fun () ->
  eq_match  "a+"                "a";
  eq_match  "a+"                "aa";
  eq_match  "a+"                "";
  eq_match  "a+"                "b";
);

expect_pass "opt" (fun () ->
  eq_match  "a?"                "";
  eq_match  "a?"                "a";
);

expect_pass "repn" (fun () ->
  eq_matched_string "a\\{3\\}"  "aaa" "aaa";
  eq_matched_string "a\\{3,\\}" "aaaa" "aaaa";
  eq_matched_string "a\\{3,5\\}"  "aaaaaaabb" "aaaaa";
  eq_matched_string "a\\{3,5\\}"  "aaaaab" "aaaaa";
);

(* String, line, word *)

expect_pass "bol" (fun () ->
  eq_match  "^a"                "ab";
  eq_match  "^a"                "b\na";
  eq_match  "^a"                "ba";
);

expect_pass "eol" (fun () ->
  eq_match  "a$"                "ba";
  eq_match  "a$"                "a\nb";
  eq_match  "a$"                "ba\n";
  eq_match  "a$"                "ab";
);

(* expect_pass "bow" (fun () -> *)
(*   eq_match  "\<a"               "a"; *)
(*   eq_match  "\
<a"               "bb aa"; *)
(*   eq_match  "\<a"               "ba ba"; *)
(* ); *)

(* expect_pass "eow" (fun () -> *)
(*   eq_match  "\>a"               "a"; *)
(*   eq_match  "\>a"               "bb aa"; *)
(*   eq_match  "\>a"               "ab ab"; *)
(* ); *)

expect_pass "bos" (fun () ->
  eq_match  "\\`a"               "ab";
  eq_match  "\\`a"               "b\na";
  eq_match  "\\`a"               "ba";
);

expect_pass "eos" (fun () ->
  eq_match  "a\\'"               "ba";
  eq_match  "a\\'"               "a\nb";
  eq_match  "a\\'"               "ba\n";
  eq_match  "a\\'"               "ab";
);

(* expect_pass "start" (fun () -> *)
(*   eq_match ~pos:1 "\=a"         "xab"; *)
(*   eq_match ~pos:1 "\=a"         "xb\na"; *)
(*   eq_match ~pos:1 "\=a"         "xba"; *)
(* ); *)

(* expect_pass "not_boundary" (fun () -> *)
(*   eq_match "\Bb\B"              "abc"; *)
(*   eq_match "\Ba"                "abc"; *)
(*   eq_match "c\B"                "abc"; *)
(* ); *)

(* Match semantics *)

expect_pass "match semantics" (fun () ->
  eq_match "\\(a\\|b\\)*b"         "aabaab";
  eq_matched_string ~sem:`Shortest "\\(a\\|b\\)*b" "aabaab" "aab";
  eq_matched_string ~sem:`First "\\(a\\|b\\)*b" "aabaab" "aab";
  eq_match "aa\\|aaa"            "aaaa";
  eq_match "aaa\\|aa"            "aaaa";
);

(* Group (or submatch) *)

expect_pass "group" (fun () ->
  eq_match "\\(a\\)\\(a\\)?\\(b\\)"   "ab";
);

(* Character set *)

expect_pass "rg" (fun () ->
  eq_match "[0-9]+"             "0123456789";
  eq_match "[0-9]+"             "a";
  eq_match "[0-9a-z]+"          "a";
);

expect_pass "compl" (fun () ->
  eq_match "[^0-9a-z]+"         "0";
  eq_match "[^0-9a-z]+"         "a";
  eq_match "[^0-9a-z]+"         "A:Z+";
);

(* Case modifiers *)

(* expect_pass "no_case" (fun () -> *)
(*   eq_match ~case:false "abc"    "abc"; *)
(*   eq_match ~case:false "abc"    "ABC"; *)

expect_pass "pathological set" (fun () ->
  eq_match "[]]+" "]]]]]kk";
  eq_match "[]a]+" "]a]]ab";
  eq_match "[^^]+" "^abcd^ef";
  eq_match "[^^]+" "abcd^ef";
  eq_match "[a-z-]" "abcd-efgh";
  eq_match "[]-^-]" "]]]---^^^[[[" 
);

expect_pass "escape" (fun () ->
  eq_matched_string "[\\u0000-u\\00ff]+" "abcdef漢字" "abcdef"
);

expect_pass "non ascii" (fun () ->
  eq_match "夕供御" "夕供御まゐるをり";
  eq_matched_string "[あ-ん]+" "ひらがなAAAA" "ひらがな";
  expect_match ".....[{Zs}]+.......[{Zs}]+....."
    "ふるいけや かわずとびこむ みずのおと"
);

expect_pass "property" (fun () ->
  eq_matched_string "[{Lu}]+" "AAAAAaaaaa" "AAAAA";
  eq_matched_string "[{Uppercase}]+" "AAAAA漢字" "AAAAA";
  eq_matched_string "[{Hiragana}]+" "ひらがな漢字" "ひらがな";
  eq_matched_string "[{Any}]+" "AAAAA漢字" "AAAAA漢字"
);

expect_pass "set notation" (fun () ->
  eq_matched_string "[{[0-9]}]+" "12345AAAA" "12345";
  eq_matched_string "[{[0-9] & [0-5]}]+" "123456789AAAA" "12345";
  eq_matched_string "[{[0-9] : [0-5]}]+" "123456789AAAA" "12345";
  eq_matched_string "[{[0-9] | [A-Z]}]+" "12345AAAA漢字" "12345AAAA";
  eq_matched_string "[{[A-Z] - [A-C]}]+" "DDDDCBA" "DDDD";
  eq_matched_string "[{[A-Zz] - [A-C] | [e-z] & [a-k]}]+" "DkzAa" "Dkz";
);

  let phone = 
    ".+[{Zs}][0-9０-９]\\{2,3\\}[{Zs}]+" ^
    "\\((\\|（\\)[0-9０-９]\\{3,4\\}\\()\\|）\\)[{Zs}]*\\(-\\|ー\\)[{Zs}]*" ^
    "[0-9０-９]\\{4\\}$"
  in

expect_pass "phonebook" (fun () ->
  expect_match phone
    "遠藤周作 03 (3744)-7633";
  expect_match phone
    "遠藤周作 ０３ （３７４４）ー７６３３";
  expect_match phone
    "宮崎 駿 076 (943) -2264";
  expect_not_match phone
    "03 (3744)-7633 遠藤周作";
  expect_not_match phone
    "遠藤周作0 (3744)-7633";
  expect_not_match phone
    "ジブリ美術館 03 (3744)-";
);
  
let contain r t =
  let r = URegexp.compile (UReStr.regexp r) in
     match URegexp.search_forward ~sem:`First r t 0 with
     Some _ -> true
     | None -> false in

let expect_contain r t =
  expect_true ~msg:(lazy r) (contain r t) in

let expect_not_contain r t =
  expect_true ~msg:(lazy r) (not (contain r t)) in

let text =
  "くれ竹のひとよに春の立つ霞、けさしも待ちいで顔に花を折り
    匂ひをあらそいてなみゐたれば、我もひとなみなみにさしいでたり。
    つぼみ紅梅にやあらむ七つに、紅のうちき、もよぎのうはぎ、
    赤色の唐衣などにしてやらん。梅からくさを浮き織りたる二つ
    小袖に、からかきの梅をぬひて侍りしをぞ着たり。" in

expect_pass "search" (fun () ->
  expect_contain "うはぎ" text;
  expect_contain "侍りし" text;
  expect_contain "[{Hiragana}]\\{7,\\}" text;
  expect_not_contain "如法" text;
  expect_not_contain "[{Ideographic}]\\{3,\\}" text;
);

(* Bugs reported by KAWAKAMI Shigenobu *)

expect_pass "dot" (fun () ->
  expect_contain "Sr\\." "Sra. y Sr. Zapatero - 1";
  expect_contain "Sr[.]" "Sra. y Sr. Zapatero - 2";
  expect_contain "Sr\\u002e" "Sra. y Sr. Zapatero - 3";
);

expect_pass "tag" (fun () ->
  eq_matched_string "^</?a>" "<a>abcdefg-1</a>" "<a>";
  eq_matched_string "</?a>" "</a>-2" "</a>";
  eq_matched_string "^<\\(/?\\)a>" "<a>abcdefg-3</a>" "<a>";
  eq_matched_string "^<\\(/\\)?a>" "<a>abcdefg-4</a>" "<a>";
);
