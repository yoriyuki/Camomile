(** Parser for the default keys of Unicode Collation Algorithm *)

(* Copyright (C) 2002, 2011 Yamagata Yoriyuki *)

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


module Int = struct type t = int let compare = (-) end
module IntMap = Map.Make (Int)

let scolon_pat = Str.regexp ";"
let blank_pat = Str.regexp "[ \t]+"
let delim_pat = Str.regexp "[ \t]*\\["

let comment_pat = Str.regexp "\\(^#.*\\)\\|\\([ \t]*$\\)"
let version_pat = Str.regexp "@version\\(.*\\)"
let entry_pat = Str.regexp
    "\\([^;]+\\);[ \t]*\\([^#]+\\)\\(#.*\\)?$"
let elements_pat = Str.regexp
    "\\([\\.\\*]\\)\\([0-9A-F]+\\)\\.\\([0-9A-F]+\\)\\.\\([0-9A-F]+\\)\\.\\([0-9A-F]+\\)]"

let int_of_code code =
  try int_of_string ("0x"^code) with _ -> failwith ("int_of_code: " ^ code)

let uchar_of_code code = UChar.chr_of_uint (int_of_code code)

let ref_lastvariable_weight = ref 0

let element_of s =
  if Str.string_match elements_pat s 0 then
    let (w1, _, _) as w =
      (int_of_code (Str.matched_group 2 s),
       int_of_code (Str.matched_group 3 s),
       int_of_code (Str.matched_group 4 s))
    in
    if Str.matched_group 1 s <> "." && w1 > !ref_lastvariable_weight
    then ref_lastvariable_weight := w1 else ();
    w
  else
    failwith ("Broken element: " ^ s)

let compose_weight a b = a land 0x3ffff lsl 15 lor (b land 0x7fff)

let ref_implicit_weights = ref
    (IntMap.add (compose_weight 0xfb40 0x8000) [0xfb40; 0x8000]
    IntMap.empty)

let rec handle_implicit_weight = function
    [] -> []
  | (a, w2, w3) :: (b, 0x0000, 0x0000) :: rest
    when a >= 0xfb40 && b >= 0x8000 ->
      let w = compose_weight a b in
      ref_implicit_weights := IntMap.add w [a; b] !ref_implicit_weights;
      (w, w2, w3) :: handle_implicit_weight rest
  | (w1, w2, w3) :: rest ->
      assert (w1 <> 0xffff);
      if w1 >= 0xfb40 then
	let w = w1 land 0x3ffff lsl 15 in
	ref_implicit_weights := IntMap.add w [w1] !ref_implicit_weights;
	(w, w2, w3) :: handle_implicit_weight rest
      else
	(w1, w2, w3) :: handle_implicit_weight rest

let swap_case = function
    0x0002 -> 0x0008
  | 0x0003 -> 0x0009
  | 0x0004 -> 0x000A
  | 0x0005 -> 0x000B
  | 0x0006 -> 0x000C
  | 0x0008 -> 0x0002
  | 0x0009 -> 0x0003
  | 0x000A -> 0x0004
  | 0x000B -> 0x0005
  | 0x000C -> 0x0006
  | 0x001C -> 0x001D
  | 0x001D -> 0x001C
  | x -> x

let swap_case_weight = function
    [w1; w2; w3] -> [w1; w2; swap_case w3]
  | _ -> assert false

let weights1_tbl =
  let s = AbsCe.EltMap.empty in
  let s = AbsCe.EltMap.add `FirstImplicit [compose_weight 0xfb40 0x8000] s in
  let s = AbsCe.EltMap.add `FirstTrailing [compose_weight 0xfc00 0x8000] s in
  s

let map_triple f (x1, x2, x3) = (f x1, f x2, f x3)

let map2_triple f (x1, x2, x3) (y1, y2, y3) =
  (f x1 y1, f x2 y2, f x3 y3)

let ws1_of = List.map (fun (w1, _, _) -> w1)
let ws2_of = List.map (fun (_, w2, _) -> w2)
let ws3_of = List.map (fun (_, _, w3) -> w3)

let directory, input_fname =
  match Sys.argv with
  | [|_; dir; fn|] -> (dir, fn)
  | _ -> failwith "invalid command line"

let weight1_tbl, weight2_tbl, weight3_lowercasefirst_tbl =
  let weight_tbls = ref (weights1_tbl, AbsCe.EltMap.empty, AbsCe.EltMap.empty) in
  let ic = open_in input_fname in
  try while true do
    let line = input_line ic in
    if Str.string_match comment_pat line 0 then () else
    if Str.string_match version_pat line 0 then () else
    if Str.string_match entry_pat line 0 then
      let s1 = Str.matched_group 1 line in
      let s2 = Str.matched_group 2 line in
      let us = List.map uchar_of_code (Str.split blank_pat s1) in
      let es = List.map element_of (Str.split delim_pat s2) in
      let es = handle_implicit_weight es in
      let ws = (ws1_of es, ws2_of es, ws3_of es) in
      weight_tbls := map2_triple (AbsCe.EltMap.add (`Seq us)) ws !weight_tbls;
    else
      failwith ("Broken_line: " ^ line)
  done; assert false with End_of_file ->
    close_in ic;
    !weight_tbls

let weight3_uppercasefirst_tbl =
  AbsCe.EltMap.map (List.map swap_case) weight3_lowercasefirst_tbl

let weight1_tbl =
  AbsCe.EltMap.add `LastVariable [!ref_lastvariable_weight] weight1_tbl

let weight1_tbl = IntMap.fold (fun w ws tbl ->
  AbsCe.EltMap.add (`ImplicitWeight ws) [w] tbl) !ref_implicit_weights weight1_tbl

let aceset_info =
  { AbsCe
    .lowercase_first_tbl =
      AbsCe.import (weight1_tbl, weight2_tbl, weight3_lowercasefirst_tbl)
  ; uppercase_first_tbl =
      AbsCe.import (weight1_tbl, weight2_tbl, weight3_uppercasefirst_tbl)
  }

let uca_defaults =
  AbsCe.cetbl_of (AbsCe.create_ace_info aceset_info.lowercase_first_tbl)

let  _ =
  let write name value = Database.write directory "mar" output_value name value in
  write "allkeys" uca_defaults;
  write "acset" aceset_info
