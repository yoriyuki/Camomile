(** Parser for the case folding table *)

(* Copyright (C) 2002 Yamagata Yoriyuki *)
(*               2011 National Institute of Advanced Industrial  *)
(*                    Science and Technology *)

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
