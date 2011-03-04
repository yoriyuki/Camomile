(* $Id: locale.ml,v 1.11 2004/08/29 04:48:21 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki *)

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
