(** Parser for Unicode sets *)

(* Copyright 2002 Yamagata Yoriyuki. distributed with LGPL *)
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
(* yoriyuki.yamagata@aist.go.jp *)

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
  let write name value = Database.write dir "mar" output_value name value in
  write (name ^ "_set") !tbl_rw;
  let tbl = UCharTbl.Bool.of_set !tbl_rw in
  write name tbl
  
let _ = main ()
