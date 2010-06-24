(* $Id: parse_specialcasing.ml,v 1.10 2006/08/13 17:24:43 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

module Unidata = Unidata.Make(Camomileconfig)
module UCharInfo = UCharInfo.Make(Camomileconfig)

let scases = ref UMap.empty

let int_of_code code = int_of_string ("0x"^code)
let uchar_of_code code = UChar.chr_of_uint (int_of_code code)

let us_of_codes codes = List.map uchar_of_code codes

let not_pat = Str.regexp "not_\\(.*\\)"
let locale_pat = Str.regexp "\\(..\\)\\(_\\(..\\)\\)?\\(_\\(.+\\)\\)?"

let rec parse_condition condition =
  let s = String.lowercase condition in
  match s with
    "final_sigma" -> `FinalSigma
  | "after_soft_dotted" -> `AfterSoftDotted
  | "more_above" -> `MoreAbove
  | "before_dot" -> `BeforeDot
  | _ ->
      if Str.string_match not_pat s 0 then
	`Not (parse_condition (Str.matched_group 1 s))
      else if Str.string_match locale_pat s 0 then `Locale s
      else invalid_arg "Not a condition"

let scolon_pat = Str.regexp ";"
let blank_pat = Str.regexp "[ \t]+"

let put_record code lower title upper conditions =
  let u = uchar_of_code code in
  let record =
    {UCharInfo.lower = us_of_codes (Str.split blank_pat lower);
     UCharInfo.title = us_of_codes (Str.split blank_pat title);
     UCharInfo.upper = us_of_codes (Str.split blank_pat upper);
     UCharInfo.condition = List.map parse_condition conditions}
  in 
  let entry = try UMap.find u !scases with Not_found -> [] in
  scases := UMap.add u (record :: entry) !scases

let comment_pat = Str.regexp "\\(^#.*\\)\\|\\([ \t]*$\\)"
let no_context_pat = 
  Str.regexp "\\([^;]*\\);\\([^;]*\\);\\([^;]*\\);\\([^;]*\\);?[ \t]*#.*"

let with_context_pat =
  Str.regexp "\\([^;]*\\);\\([^;]*\\);\\([^;]*\\);\\([^;]*\\);\\([^;]*\\);?[ \t]*#.*"

let loaddata () = 
  let count = ref 0 in
  try while true do
    let line = read_line () in
    incr count;
    if Str.string_match comment_pat line 0 then () else
    if Str.string_match no_context_pat line 0 then
      let code = Str.matched_group 1 line in
      let lower = Str.matched_group 2 line in
      let title = Str.matched_group 3 line in
      let upper = Str.matched_group 4 line in
      put_record code lower title upper []
    else if Str.string_match with_context_pat line 0 then
      let code = Str.matched_group 1 line in
      let lower = Str.matched_group 2 line in
      let title = Str.matched_group 3 line in
      let upper = Str.matched_group 4 line in
      let conditions = Str.matched_group 5 line in
      put_record code lower title upper (Str.split blank_pat conditions)
    else failwith (Printf.sprintf "Malformed entry in the line %d" !count)
  done with End_of_file -> ()

module CasingTbl = UCharTbl.Make (struct
  type t = UCharInfo.special_casing_property list
  let equal = (=)
  let hash = Hashtbl.hash
end)

let  _ =
  let dir = ref "" in
  Arg.parse [] (fun s -> dir := s) "Parse the SpecialCasing file";
  loaddata ();
  let tbl = CasingTbl.of_map [] !scases in
  let file = Filename.concat !dir "special_casing.mar" in
  let c = open_out_bin file in
  output_value c tbl; close_out c;
