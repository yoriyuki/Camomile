(* $Id: parse_uniset.ml,v 1.10 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki. distributed with LGPL *)

let tbl_rw = ref USet.empty

(* remove comments *)
let range_pat = Str.regexp "\\([0-9A-Fa-f]+\\)\\.\\.\\([0-9A-Fa-f]+\\)"
let num_pat = Str.regexp "[0-9A-Za-z]+"

(* let get_line () =
  let s = read_line () in
  if Str.string_match line_pat s 0 then Str.matched_group 1 s else s *)

let prev_entry = ref 0

let read_data () =
  try while true do
    let s = read_line () in
    if Str.string_match range_pat s 0 then
      let u1 = UChar.chr_of_uint (int_of_string ("0x"^(Str.matched_group 1 s))) in
      let u2 = UChar.chr_of_uint (int_of_string ("0x"^(Str.matched_group 2 s))) in
      tbl_rw := USet.add_range u1 u2 !tbl_rw
    else if Str.string_match num_pat s 0 then
      let u = UChar.chr_of_uint (int_of_string ("0x"^(Str.matched_string s))) in
      tbl_rw := USet.add u !tbl_rw
    else ()
  done with End_of_file -> ()

let main () =
  read_data ();
  let dir, name =
    let dir = ref None in
    let name = ref None in
    Arg.parse [] (fun s ->
      match !dir with
	None -> dir := Some s;
      | Some _ -> 
	  if !name = None then name := Some s else
	  raise (Arg.Bad "Too many arguments"))
      "Parse unicode lists";
    match !dir, !name with
      (Some dir, Some name) -> dir, name
    | _ -> raise (Arg.Bad "Some arguments are missing.") in
  let c = open_out_bin (Filename.concat dir (name ^ "_set.mar")) in
  output_value c !tbl_rw;
  close_out c;
  let c = open_out_bin (Filename.concat dir (name ^ ".mar")) in
  let tbl = UCharTbl.Bool.of_set !tbl_rw in
  output_value c tbl;
  close_out c 
  
let _ = main ()
