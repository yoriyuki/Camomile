(** Parse version data of Unicode charactor *)

(* Copyright (C) 2010 Pierre Chambart *)
(*               2011 Yoriyuki Yamagata *)

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

let parse ic =
  let rec parse map =
    try
      raise (Ok (input_line ic))
    with
    | End_of_file -> close_in ic; map
    | Ok s ->
      let map = parse_line map s in
      parse map
  in
  parse UMap.empty

let () =
  match Sys.argv with
  | [|_; dir; input_fname|] ->
    let map = parse (open_in input_fname) in
    Database.write dir "mar" output_value "age"
      (UCharTbl.Char.of_map undefined_version map)
  | _ -> failwith "invalid command line"
