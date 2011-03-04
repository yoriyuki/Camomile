(* Copyright 2010 Pierre Chambart *)

(* remove comments *)
let range_pat = 
  Str.regexp "\\([0-9A-Fa-f]+\\)\\.\\.\\([0-9A-Fa-f]+\\)[ \\t]*;[ \\t]*\\([0-9]+\\)\\.\\([0-9]+\\)"
let num_pat = 
  Str.regexp "\\([0-9A-Za-z]+\\)+[ \\t]*;[ \\t]*\\([0-9]+\\)\\.\\([0-9]+\\)"

let char_of_string s =
  UChar.chr_of_uint (int_of_string ("0x"^s))

let undefined_version = '\xFE'

let make_version_char major minor =
  Char.chr (major * 16 + minor)

let parse_line map s =
  if Str.string_match range_pat s 0
  then
    let u1 = char_of_string (Str.matched_group 1 s) in
    let u2 = char_of_string (Str.matched_group 2 s) in
    let major = int_of_string (Str.matched_group 3 s) in
    let minor = int_of_string (Str.matched_group 4 s) in
    UMap.add_range u1 u2 (make_version_char major minor) map
  else
    if Str.string_match num_pat s 0
    then
    let u = char_of_string (Str.matched_group 1 s) in
    let major = int_of_string (Str.matched_group 2 s) in
    let minor = int_of_string (Str.matched_group 3 s) in
    UMap.add u (make_version_char major minor) map
    else map

exception Ok of string

let parse () =
  let rec parse map =
    try
      raise (Ok (read_line ()))
    with
      | End_of_file -> map
      | Ok s ->
	  let map = parse_line map s in
	  parse map
  in
  parse UMap.empty

let main () =
  Arg.parse [] 
    (fun dir -> 
       let map = parse () in
       Database.write dir "mar" output_value "age" 
	 (UCharTbl.Char.of_map undefined_version map))
    "Parse DerivedAge.txt"

let _ = main ()
