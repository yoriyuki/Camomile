(* $Id: iana.ml,v 1.1 2003/07/31 12:23:22 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki *)

#load "bigarray.cma";;
#load "camomile.cma";;
#load "str.cma";;

let name_pat = Str.regexp "Name:[ \t]+\\([^ \t]+\\)"
let alias_pat = Str.regexp "Alias:[ \t]+\\([^ \t]+\\)"

let codegen name iana_names =
  match name with
    None -> ()
  | Some name ->
      List.iter (fun alias ->
	Printf.printf "let () = alias \"IANA/%s\" \"%s\"\n" alias name)
	iana_names

let () = 
  let iana_names = ref [] in
  let name = ref None in
  try while true do
    let line = read_line () in
    if Str.string_match name_pat line 0 then begin
      codegen !name !iana_names;
      let s = Str.matched_group 1 line in
      iana_names := [s];
      name := (try 
	let enc = CharEncoding.of_name s in
	Some (CharEncoding.name_of enc)
      with Not_found ->
	try
	  let s = String.uppercase s in
	  let enc = CharEncoding.of_name s in
	  Some (CharEncoding.name_of enc)
	with Not_found ->
	  None);
      Printf.printf "(* %s *)\n" line;
    end else if Str.string_match alias_pat line 0 then
      let alias = Str.matched_group 1 line in
      if alias = "None" then () else begin
	iana_names := alias :: !iana_names;
	if !name <> None then () else
	name := (try 
	  let enc = CharEncoding.of_name alias in
	  Some (CharEncoding.name_of enc)
	with Not_found -> 
	  try
	    let s = String.uppercase alias in
	    let enc = CharEncoding.of_name alias in
	    Some (CharEncoding.name_of enc)
	  with Not_found ->
	    None);
	Printf.printf "(* %s *)\n" line
      end
    else ()
  done with End_of_file -> ()
