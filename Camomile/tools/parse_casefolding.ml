(* $Id: parse_casefolding.ml,v 1.8 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

open Unidata

let folds = ref UMap.empty

let int_of_code code = int_of_string ("0x"^code)
let uchar_of_code code = UChar.chr_of_uint (int_of_code code)

let us_of_codes codes = List.map uchar_of_code codes

let scolon_pat = Str.regexp ";"
let blank_pat = Str.regexp "[ \t]+"

let comment_pat = Str.regexp "\\(^#.*\\)\\|\\([ \t]*$\\)"
let entry_pat = 
  Str.regexp "\\([^;]*\\); \\([^;]*\\); \\([^;]*\\);.*"

let loaddata () = 
  let count = ref 0 in
  try while true do
    let line = read_line () in
    incr count;
    if Str.string_match comment_pat line 0 then () else
    if Str.string_match entry_pat line 0 then
      let u = uchar_of_code (Str.matched_group 1 line) in
      let status = Str.matched_group 2 line in
      let mapping = Str.matched_group 3 line in
      if status = "C" || status = "F" then
	let mapping = us_of_codes (Str.split blank_pat mapping) in
        folds := UMap.add u mapping !folds
      else ()
    else failwith (Printf.sprintf "Malformed entry in the line %d" !count)
  done with End_of_file -> ()

module Tbl = UCharTbl.Make (struct
  type t = UChar.t list
  let equal = (=)
  let hash = Hashtbl.hash
end)

let  _ =
  let dir = ref "" in
  Arg.parse [] (fun s -> dir := s) "Parse the CaseFolding file";
  loaddata ();
  let tbl = Tbl.of_map [] !folds in
  Database.write !dir "mar" output_value "case_folding" tbl
