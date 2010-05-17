(* $Id: parse_scripts.ml,v 1.6 2006/08/13 17:23:13 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

module Unidata = Unidata.Make(Camomileconfig)
open Unidata

let tbl_rw = 
  let max_uchar = UChar.chr_of_uint 0x7fffffff in
  let null = UChar.chr_of_uint 0 in
  let n = num_of_script `Common in
  ref (UMap.add_range null max_uchar n UMap.empty)

(* remove comments *)
let range_pat = 
  Str.regexp "\\([0-9A-Fa-f]+\\)\\.\\.\\([0-9A-Fa-f]+\\)[ \\t]*;[ \\t]*\\([^ \\t]+\\)"
let num_pat = 
  Str.regexp "\\([0-9A-Za-z]\\)+[ \\t]*;[ \\t]*\\([^ \\t]+\\)"

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
      let name = Str.matched_group 3 s in
      let script = script_of_name name in
      let num = num_of_script script in
      tbl_rw := UMap.add_range u1 u2 num !tbl_rw
    else if Str.string_match num_pat s 0 then
      let n = int_of_string ("0x"^(Str.matched_group 1 s)) in
      let name = Str.matched_group 2 s in
      let script = script_of_name name in
      let num = num_of_script script in      
      tbl_rw := UMap.add (UChar.chr_of_uint n) num !tbl_rw
    else ()
  done with End_of_file -> ()

let main () =
  begin
    read_data ();
    Arg.parse [] 
      (fun dir -> 
	let c = open_out_bin (Filename.concat dir "scripts_map.mar") in
	let tbl = UMap.map script_of_num !tbl_rw in
	output_value c tbl;
	close_out c;
	let c = open_out_bin (Filename.concat dir "scripts.mar") in
	let tbl = UCharTbl.Bits.of_map (num_of_script `Common) !tbl_rw in
	output_value c tbl;
	close_out c) 
      "Parse Scripts.txt"
  end
    
let _ = main ()
