(* Copyright (C) 2002, 2003, 2004 Yamagata Yoriyuki *)

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

 
module type Type =
  sig
    type text
    val lowercase : ?locale:string -> text -> text
    val uppercase : ?locale:string -> text -> text
    val titlecase : ?locale:string -> text -> text
    val casefolding : text -> text
    val compare_caseless : text -> text -> int
  end

module Make (Config : ConfigInt.Type) (Text : UnicodeString.Type) =  struct
  module Unidata = Unidata.Make(Config)
  open Unidata
  module UCharInfo = UCharInfo.Make(Config)
  open UCharInfo

let uppercase_tbl = load_property_tbl `Uppercase

let is_uppercase u = UCharTbl.Bool.get uppercase_tbl u 

let lowercase_tbl = load_property_tbl `Lowercase
let is_lowercase u = UCharTbl.Bool.get lowercase_tbl u 

let conditional_casing_tbl = load_conditional_casing_tbl ()
let conditional_casing u = UCharTbl.get conditional_casing_tbl u

let casefolding_tbl = load_casefolding_tbl ()
let casefolding_char u = UCharTbl.get casefolding_tbl u

let is_null u = UChar.uint_code u = 0

let to_lower1_tbl = load_to_lower1_tbl ()

let to_lower1 u = 
  let u' = UCharTbl.get to_lower1_tbl u in
  if is_null u' then u else u'

let to_upper1_tbl = load_to_upper1_tbl ()
let to_upper1 u = 
  let u' = UCharTbl.get to_upper1_tbl u in
  if is_null u' then u else u'

let to_title1_tbl = load_to_title1_tbl ()
let to_title1 u = 
  let u' = UCharTbl.get to_title1_tbl u in
  if is_null u' then u else u'

let is_case_ignorable u =
  let n = UChar.uint_code u in
  if n = 0x0027 || n = 0x00ad || n = 0x2016 then true else
  match (general_category u) with
    `Mn -> true
  | `Me -> true
  | `Cf -> true
  | `Lm -> true
  | `Sk -> true
  | _ -> false

(* Fix me: "normalization clause" of UTR#25 is ommited. *)
let is_cased u = 
  is_uppercase u || is_lowercase u || general_category u = `Lt 

    type text = Text.t

    let is_final_sigma t i =
      let rec see_backward t i =
	if Text.out_of_range t i then false else
	let u = Text.look t i in
	if is_case_ignorable u then see_backward t (Text.prev t i) else
	is_cased u
      in
      let rec see_forward t i =
	if Text.out_of_range t i then false else
	let u = Text.look t i in
	if is_case_ignorable u then see_forward t (Text.next t i) else
	is_cased u
      in
      see_backward t (Text.prev t i) && 
      not (see_forward t (Text.next t i))

    let is_more_above t i =
      let rec search i =
	if Text.out_of_range t i then false else
	let u = Text.look t i in
	let c = combined_class u in
	if c = 0 then false else
	if c = 230 then true else
	search (Text.next t i)
      in
      search (Text.next t i)

    let soft_dotted_tbl = UCharInfo.load_property_tbl `Soft_Dotted
    let is_soft_dotted u = UCharTbl.Bool.get soft_dotted_tbl u

    let is_after_soft_dotted t i =
      let rec search i =
	if Text.out_of_range t i then false else
	let u = Text.look t i in
	let c = combined_class u in
	if c = 0 then is_soft_dotted u else
	if c = 230 then false else
	search (Text.prev t i)
      in
      search (Text.prev t i)

    let is_before_dot t i =
	let rec search i =
	  if Text.out_of_range t i then false else
	  let u = Text.look t i in
	  if UChar.int_of u = 0x0307 then true else
	  let c = combined_class u in
	  if c = 0 || c = 230 then false else
	  search (Text.next t i)
	in
	search (Text.next t i)

    let rec match_condition ?locale t i condition =
      match condition with
	`Locale loc -> 
	  (match locale with
	    None -> false
	  | Some loc' -> Locale.contain loc loc')
      | `FinalSigma -> is_final_sigma t i
      | `MoreAbove -> is_more_above t i
      | `AfterSoftDotted -> is_after_soft_dotted t i
      | `BeforeDot -> is_before_dot t i
      | `Not cond -> not (match_condition ?locale t i cond)

    let rec is_matched_casing_property ?locale t i prop =
      List.for_all (match_condition ?locale t i) prop.condition

    let lowercase ?locale t =
      let buf = Text.Buf.create 0 in
      let rec loop i =
	if Text.out_of_range t i then Text.Buf.contents buf else
	let u = Text.look t i in
	(match conditional_casing u with
	  [] -> Text.Buf.add_char buf (to_lower1 u)
	| conds  ->
	    try
	      let p = is_matched_casing_property ?locale t i in
	      let c = List.find p conds in
	      List.iter (Text.Buf.add_char buf) c.lower
	    with 
	      Not_found -> Text.Buf.add_char buf (to_lower1 u));
	loop (Text.next t i)
      in
      loop (Text.nth t 0)

    let uppercase ?locale t =
      let buf = Text.Buf.create 0 in
      let rec loop i =
	if Text.out_of_range t i then Text.Buf.contents buf else
	let u = Text.look t i in
	(match conditional_casing u with
	  [] -> Text.Buf.add_char buf (to_upper1 u)
	| conds  ->
	    try
	      let p = is_matched_casing_property ?locale t i in
	      let c = List.find p conds in
	      List.iter (Text.Buf.add_char buf) c.upper
	    with 
	      Not_found -> Text.Buf.add_char buf (to_upper1 u));
	loop (Text.next t i)
      in
      loop (Text.nth t 0)

    let titlecase ?locale t =
      let buf = Text.Buf.create 0 in
      let rec loop is_head i =
	if Text.out_of_range t i then Text.Buf.contents buf else
	let u = Text.look t i in
	(match conditional_casing u with
	  [] -> 
	    let u' = 
	      if is_head then to_title1 u else
	      to_lower1 u
	    in
	    Text.Buf.add_char buf u'
	| conds  ->
	    try
	      let p = is_matched_casing_property ?locale t i in
	      let c = List.find p conds in
	      let us = if is_head then c.title else c.lower in
	      List.iter (Text.Buf.add_char buf) us
	    with 
	      Not_found ->
		let u' = 
		  if is_head then to_title1 u else
		  to_lower1 u
		in
		Text.Buf.add_char buf u');
	let is_head =
	  if is_case_ignorable u then is_head else
	  not (is_cased u)
	in
	loop is_head (Text.next t i)
      in
      loop true (Text.nth t 0)
	
    let casefolding t =
      let buf = Text.Buf.create 0 in
	Text.iter 
	  (fun u -> 
	     let us = casefolding_char u in
	       List.iter (Text.Buf.add_char buf) us)
	  t;
	Text.Buf.contents buf

    let compare_caseless t1 t2 =
      let rec loop i1 i2 us1 = function
	  [] ->
	    if Text.out_of_range t2 i2 then
	      if us1 = [] && Text.out_of_range t1 i1 then 0 else 1
	    else
	      let u = Text.look t2 i2 in
		loop i1 (Text.next t2 i2) us1 (casefolding_char u)
	| (u :: r) as us2 ->
	    match us1 with
		[] ->
		  if Text.out_of_range t1 i1 then -1 else
		    let u = Text.look t1 i1 in
		      loop (Text.next t1 i1) i2 (casefolding_char u) us2
	      | u' :: r' ->
		  let sgn = UChar.compare u' u in
		    if sgn = 0 then loop i1 i2 r' r else sgn 
      in loop (Text.first t1) (Text.first t2) [] []
	      
  end
