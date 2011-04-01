(* Copyright (C) 2003 Yamagata Yoriyuki *)
(*               2011 National Institute of Advanced Industrial *)
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


type t = string

let rec parse_locale_aux i s =
  let i' =
    try String.index_from s i '_'
    with Not_found -> String.length s
  in
  String.sub s i (i' - i) ::
  if i' >= String.length s then [] else
  if i' = String.length s - 1 then [""] else
  parse_locale_aux (i' + 1) s

let parse_locale = parse_locale_aux 0

let isoc_locale_of ~locale ~enc =
  match parse_locale locale with
    [lg] -> lg ^ "." ^ enc
  | [lg; nt] -> lg ^ "_" ^ nt ^ "." ^ enc
  | [lg; nt; md] ->
      lg ^ "_" ^ nt ^ "." ^ enc ^ "@" ^ md
  | _ -> invalid_arg ("locale: " ^ locale)
  
let rec cut_last = function
    [] -> assert false
  | [x] -> []
  | x :: rest -> x :: cut_last rest

let read root suffix reader locale =
  let locale_path = parse_locale locale in
  let rec search locale_path =
    let basename = 
      if locale_path = [] then "root" else
      String.concat "_" locale_path
    in
    try Database.read root suffix reader basename with
      Not_found ->
	if locale_path = [] then raise Not_found else
	search (cut_last locale_path)
  in
  search locale_path

let rec list_contain l1 l2 =
  match l1, l2 with
    [], _ -> true
  | _, [] -> false
  | x1 :: rest1, x2 :: rest2 ->
      if x1 = x2 then list_contain rest1 rest2 else false

let contain loc1 loc2 =
  let l1 = parse_locale loc1 in
  let l2 = parse_locale loc2 in
  list_contain l1 l2
