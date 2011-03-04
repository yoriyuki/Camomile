(* $Id: parse_allkeys.ml,v 1.17 2006/08/20 08:25:59 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

open Toolslib
open Unidata
open AbsCe

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
  let s = EltMap.empty in
  let s = EltMap.add `FirstImplicit [compose_weight 0xfb40 0x8000] s in
  let s = EltMap.add `FirstTrailing [compose_weight 0xfc00 0x8000] s in
  s

let map_triple f (x1, x2, x3) = (f x1, f x2, f x3)

let map2_triple f (x1, x2, x3) (y1, y2, y3) =
  (f x1 y1, f x2 y2, f x3 y3)

let ws1_of = List.map (fun (w1, _, _) -> w1)
let ws2_of = List.map (fun (_, w2, _) -> w2)
let ws3_of = List.map (fun (_, _, w3) -> w3)

let weight1_tbl, weight2_tbl, weight3_lowercasefirst_tbl =
  let weight_tbls = ref (weights1_tbl, EltMap.empty, EltMap.empty) in
  try while true do
    let line = read_line () in
    if Str.string_match comment_pat line 0 then () else
    if Str.string_match version_pat line 0 then () else
    if Str.string_match entry_pat line 0 then
      let s1 = Str.matched_group 1 line in
      let s2 = Str.matched_group 2 line in
      let us = List.map uchar_of_code (Str.split blank_pat s1) in
      let es = List.map element_of (Str.split delim_pat s2) in
      let es = handle_implicit_weight es in
      let ws = (ws1_of es, ws2_of es, ws3_of es) in
      weight_tbls := map2_triple (EltMap.add (`Seq us)) ws !weight_tbls;
    else
      failwith ("Broken_line: " ^ line)
  done; assert false with End_of_file ->
    !weight_tbls

let weight3_uppercasefirst_tbl = 
  EltMap.map (List.map swap_case) weight3_lowercasefirst_tbl

let weight1_tbl = 
  EltMap.add `LastVariable [!ref_lastvariable_weight] weight1_tbl

let weight1_tbl = IntMap.fold (fun w ws tbl ->
  EltMap.add (`ImplicitWeight ws) [w] tbl) !ref_implicit_weights weight1_tbl

let aceset_info =
  {lowercase_first_tbl = 
   import (weight1_tbl, weight2_tbl, weight3_lowercasefirst_tbl);
   uppercase_first_tbl =
   import (weight1_tbl, weight2_tbl, weight3_uppercasefirst_tbl)}

let uca_defaults = cetbl_of (create_ace_info aceset_info.lowercase_first_tbl)

let directory =
  let dir = ref "" in
  Arg.parse [] (fun s -> dir := s) "Parse the allkeys.txt";
  !dir

let  _ =
  let write name value = Database.write directory "mar" output_value name value in
  write "allkeys" uca_defaults;
  write "acset" aceset_info
