(** Parse the script data of Unicode *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. *)
(*           2011 National Institute of Advanced Industrial *)
(*                Science and Technology *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yori@users.sourceforge.net *)

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
  Str.regexp "\\([0-9A-Za-z]+\\)+[ \\t]*;[ \\t]*\\([^ \\t]+\\)"

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
	let write name value = Database.write dir "mar" output_value name value in
	write "scripts_map" (UMap.map script_of_num !tbl_rw);
	write "scripts" (UCharTbl.Bits.of_map (num_of_script `Common) !tbl_rw))
      "Parse Scripts.txt"
  end
    
let _ = main ()
