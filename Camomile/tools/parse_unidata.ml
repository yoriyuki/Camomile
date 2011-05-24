(** Parser of Unicode Database *)
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


module Unidata = Unidata.Make(Camomileconfig)
open Unidata

(* It seems that the default value of combined class is 0 *)
type combined_class = int

let num_of_combined_class cc = cc

let combined_class_of_num i = i

let null = UChar.chr_of_uint 0
let max_uchar = UChar.chr_of_uint 0x7fffffff

let cat_tbl = ref (UMap.add_range null max_uchar 0 UMap.empty)

let combcl_tbl = ref UMap.empty

let decomp_tbl : decomposition_info UMap.t ref = ref UMap.empty

let to_lower1 = ref UMap.empty
let to_title1 = ref UMap.empty
let to_upper1 = ref UMap.empty

let scolon_pat = Str.regexp ";"
let blank_pat = Str.regexp "[ \t]+"
let mark_pat = Str.regexp "<.*>"

let int_of_code code = int_of_string ("0x"^code)
let uchar_of_code code = UChar.chr_of_uint (int_of_code code)
let option_uchar_of_code code =
  if code = "" then None else Some (uchar_of_code code)

let read_unidata () =
  try while true do
    let s = read_line () in
    let tokens = Str.split_delim scolon_pat s in
    match tokens with
      [code; name; catname; comb_cl_str; bidi_str; decomp_str;
       dec_digit_str; digit_str; num_str; mirrored_str; old_name; comment; 
       upper_str; lower_str; title_str] ->
	 let i0 =  int_of_code code in
	 if i0 >= 0xf0000 && i0 <= 0xffffd then () else
	 if i0 >= 0x100000 && i0 <= 0x10fffd then () else
	 let i1 =			(*continous region*)
	   if i0 = 0x3400 then 0x4db4	    (*CJK Ideographic Extension A*)
	   else if i0 = 0x4e00 then 0x9fa4  (*CJK Ideographic*)
	   else if i0 = 0xac00 then 0xd7a3  (*Hangul Syllable*)
	   else if i0 = 0xd800 then 0xdbfe  (*High Surrogate*)
	   else if i0 = 0xdc00 then 0xdefe  (*Low Surrogate*)
	   else if i0 = 0xe000 then 0xf8fe  (*Private Zone*)
	   else if i0 = 0x20000 then 0x2a6d5(*CJK Ideographic Extension B*)
	   else i0
	 in
	 let cat_num = num_of_cat (cat_of_name catname) in
	 let comb_cl = int_of_string comb_cl_str in
	 let decomp = 
	   let char_str = Str.split blank_pat decomp_str in
	   if char_str = [] then 
	     if 0xac00 <= i0 && i0 <= 0xd7a3 then `HangulSyllable 
	     else `Canonform
	   else if Str.string_match mark_pat (List.hd char_str) 0 then
	     let us = 
	       List.map (fun s -> 
		 UChar.chr_of_uint (int_of_string ("0x"^s))) (List.tl char_str)
	     in
	     match Str.matched_string (List.hd char_str) with
	       "<font>" -> `Composite (`Font, us)
	     | "<noBreak>" -> `Composite (`NoBreak, us)
	     | "<initial>" -> `Composite (`Initial, us)
	     | "<medial>" -> `Composite (`Medial, us)
	     | "<final>" -> `Composite (`Final, us)
	     | "<isolated>" -> `Composite (`Isolated, us)
	     | "<circle>" -> `Composite (`Circle, us)
	     | "<super>" -> `Composite (`Super, us)
	     | "<sub>" -> `Composite (`Sub, us)
	     | "<vertical>" -> `Composite (`Vertical, us)
	     | "<wide>" -> `Composite (`Wide, us)
	     | "<narrow>" -> `Composite (`Narrow, us)
	     | "<small>" -> `Composite (`Small, us)
	     | "<square>" -> `Composite (`Square, us)
	     | "<fraction>" -> `Composite (`Fraction, us)
	     | "<compat>" -> `Composite (`Compat, us)
	     |  _ -> failwith ("Malformed Table"^s)
	   else 
	     let us = 
	       List.map (fun s -> 
		 UChar.chr_of_uint (int_of_string ("0x"^s))) 
		 char_str
	     in 
	     `Composite(`Canon, us)
	 in
	 let upper_us = option_uchar_of_code upper_str in
	 let title_us = option_uchar_of_code title_str in
	 let lower_us = option_uchar_of_code lower_str in
	 let u0 = UChar.chr_of_uint i0 in
	 let u1 = UChar.chr_of_uint i1 in
	 if cat_num <> 0 then cat_tbl := UMap.add_range u0 u1 cat_num !cat_tbl;
	 if comb_cl <>0 then 
	   combcl_tbl := UMap.add_range u0 u1 comb_cl !combcl_tbl;
	 if decomp <> `Canonform then 
	   decomp_tbl := UMap.add_range u0 u1 decomp !decomp_tbl;
	 (match upper_us with None -> () | Some u' ->
	   to_upper1 := UMap.add_range u0 u1 u' !to_upper1);
	 (match title_us with None -> () | Some u' ->
	   to_title1 := UMap.add_range u0 u1 u' !to_title1);
	 (match lower_us with None -> () | Some u' ->
	   to_lower1 := UMap.add_range u0 u1 u' !to_lower1);
    | _ -> failwith ("Malformed Table "^s)
  done with End_of_file -> ()

let rec decompose decomp_tbl u =
  try match UMap.find u decomp_tbl with
    `Composite (`Canon, us) ->
      List.fold_right (fun u a -> (decompose decomp_tbl u) @ a) us []
  | `HangulSyllable -> Hangul.decompose u
  | _ -> [u]
  with 
    Not_found -> [u]

module CompositeTbl = 
  UCharTbl.Make (struct 
    type t = (UChar.t * UChar.t) list
    let equal = (=)
    let hash = Hashtbl.hash
  end)

module DecompTbl =
  UCharTbl.Make (struct 
    type t = Unidata.decomposition_info
    let equal = (=)
    let hash = Hashtbl.hash
  end)

module UTbl =
  UCharTbl.Make (struct 
    type t = UChar.t
    let equal = UChar.eq
    let hash u = UChar.uint_code u
  end)

let main () =
  let dir = ref "" in
  begin
    Arg.parse [] (fun s -> dir := s) "Parse the Unicode data file";
    read_unidata();
    let comp_tbl =
      let f u d tbl =
	match d with
	  `Composite (`Canon, [u1; u2]) -> 
	    let l = try UMap.find u1 tbl with Not_found -> [] in
(*	    Printf.printf "\\u%04x : [" (int_of_uchar u1);
	    List.iter (fun (u2, u) ->
	      Printf.printf "(\\u%04x, \\u%04x);"
		(int_of_uchar u2)
		(int_of_uchar u))
	      l;
	    print_string "] \n"; *)
	    UMap.add u1 ((u2, u) :: l) tbl
	| _ -> tbl
      in
      UMap.fold f !decomp_tbl UMap.empty in
    let comp_tbl_ro = CompositeTbl.of_map [] comp_tbl in
    let decomps =
      UMap.fold (fun u d decomps ->
	match d with
	  `Composite (`Canon, us) ->
	    let d = 
	      List.fold_right 
		(fun u a -> (decompose !decomp_tbl u) @ a) 
		us 
		[] 
	    in
	    UMap.add u (`Composite (`Canon, d)) decomps
	| x -> UMap.add u x decomps)
	!decomp_tbl
	UMap.empty
    in
    let cat_tbl_ro = UCharTbl.Bits.of_map 0 !cat_tbl in
    let combcl_tbl_ro = 
      UCharTbl.Char.of_map
	'\000' 
	(UMap.map Char.chr !combcl_tbl) in
    let decomp_tbl_ro = DecompTbl.of_map `Canonform decomps in
    let null = UChar.chr_of_uint 0 in
    let to_lower1_ro = UTbl.of_map null !to_lower1 in
    let to_title1_ro = UTbl.of_map null !to_title1 in
    let to_upper1_ro = UTbl.of_map null !to_upper1 in
    let write name value = Database.write !dir "mar" output_value name value in
    begin
      let gen_cat_map = UMap.map cat_of_num !cat_tbl in
      write "general_category_map" gen_cat_map;      
      write "general_category" cat_tbl_ro;
      write "combined_class_map" !combcl_tbl; 
      write "combined_class" combcl_tbl_ro;
      write "decomposition" decomp_tbl_ro;
      write "composition" comp_tbl_ro;
      write "to_lower1" to_lower1_ro;
      write "to_title1" to_title1_ro;
      write "to_upper1" to_upper1_ro;
    end
  end

let _ =  main ()
