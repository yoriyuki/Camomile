(** Unicode data *)

(* Copyright (C) 2002, 2003, 2011 Yamagata Yoriyuki *)

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


module type Type = sig
val read_data : ?datadir:string -> string -> 'a

type general_category_type = 
  [ `Lu		(* Letter, Uppercase *)
  | `Ll		(* Letter, Lowercase *)
  | `Lt		(* Letter, Titlecase *)
  | `Mn		(* Mark, Non-Spacing *)
  | `Mc		(* Mark, Spacing Combining *)
  | `Me		(* Mark, Enclosing *)
  | `Nd		(* Number, Decimal Digit *)
  | `Nl		(* Number, Letter *)
  | `No		(* Number, Other *)
  | `Zs		(* Separator, Space *)
  | `Zl		(* Separator, Line *)
  | `Zp		(* Separator, Paragraph *)
  | `Cc		(* Other, Control *)
  | `Cf		(* Other, Format *)
  | `Cs		(* Other, Surrogate *)
  | `Co		(* Other, Private Use *)
  | `Cn		(* Other, Not Assigned *)
  | `Lm		(* Letter, Modifier *)
  | `Lo		(* Letter, Other *)
  | `Pc		(* Punctuation, Connector *)
  | `Pd		(* Punctuation, Dash *)
  | `Ps		(* Punctuation, Open *)
  | `Pe		(* Punctuation, Close *)
  | `Pi		(* Punctuation, Initial quote  *)
  | `Pf		(* Punctuation, Final quote  *)
  | `Po		(* Punctuation, Other *)
  | `Sm		(* Symbol, Math *)
  | `Sc		(* Symbol, Currency *)
  | `Sk		(* Symbol, Modifier *)
  | `So ]	(* Symbol, Other *)

val cat_of_name : string -> general_category_type
val num_of_cat : general_category_type -> int
val cat_of_num : int -> general_category_type

type script_type =
  [ `Common
  | `Inherited
  | `Latin
  | `Greek
  | `Cyrillic
  | `Armenian
  | `Hebrew
  | `Arabic
  | `Syriac
  | `Thaana
  | `Devanagari
  | `Bengali
  | `Gurmukhi
  | `Gujarati
  | `Oriya
  | `Tamil
  | `Telugu
  | `Kannada
  | `Malayalam
  | `Sinhala
  | `Thai
  | `Lao
  | `Tibetan
  | `Myanmar
  | `Georgian
  | `Hangul
  | `Ethiopic
  | `Cherokee
  | `Canadian_Aboriginal
  | `Ogham
  | `Runic
  | `Khmer
  | `Mongolian
  | `Hiragana
  | `Katakana
  | `Bopomofo
  | `Han
  | `Yi
  | `Old_Italic
  | `Gothic
  | `Deseret
  | `Tagalog
  | `Hanunoo
  | `Buhid
  | `Tagbanwa ]

val script_of_name : string -> script_type
val script_of_num : int -> script_type
val num_of_script : script_type -> int

type decomposition_type =
    [ `Canon | `Font | `NoBreak | `Initial | `Medial | `Final |
    `Isolated | `Circle | `Super | `Sub | `Vertical | `Wide | `Narrow |
    `Small | `Square | `Fraction | `Compat ]

type decomposition_info =
    (* Already in the canonical form *)
    [ `Canonform
    (* `Composite (dtype, text) :
     * means the given character is decomposed into text by dtype
     * decomposition. *)
  | `HangulSyllable
  | `Composite of decomposition_type * UChar.t list ]

(* Collation *)

type ce_type = int		(*collation element*)

val primary : ce_type -> int
val secondary : ce_type -> int
val tertiary : ce_type -> int
val compose_ce : int -> int -> int -> ce_type
val complete_ignorable : ce_type

type ce_tbl = (UChar.t list * ce_type list) list UCharTbl.t

type variable_option =   
  [ `Blanked 
  | `Non_ignorable 
  | `Shifted
  | `Shift_Trimmed ]

type col_info =
    {variable_top : int;
     variable_option : variable_option;
     french_accent : bool;		
     hiraganaQ : bool;
     hiraganaQ_weight : int;
     tbl : ce_tbl}

val get_col_info : ?locale:string -> unit -> col_info

(* If the returned list contains ([u1; u2; ... ;un], [ce1; ce2; ... ;cem]),
   for the given character u, the sequence u u1 u2 ... un corresponds
   sequence of collation elements ce1 ce2 ... cem. the list is in 
   decreasing order respect to n. *)
val ce : ce_tbl -> UChar.t -> (UChar.t list * ce_type list) list

type localedata =
    {col_info : col_info option}
end

module Make (Config : ConfigInt.Type) = struct

let read_data ?datadir name =
  let datadir =
    match datadir with
      Some d -> d
    | None -> Config.datadir in
  try
    Database.read datadir "mar" input_value name
  with
    Not_found -> failwith (Printf.sprintf "Camomile: resource %S not found in database %s" (name^".mar") datadir)

type general_category_type = 
  [ `Lu		(* Letter, Uppercase *)
  | `Ll		(* Letter, Lowercase *)
  | `Lt		(* Letter, Titlecase *)
  | `Mn		(* Mark, Non-Spacing *)
  | `Mc		(* Mark, Spacing Combining *)
  | `Me		(* Mark, Enclosing *)
  | `Nd		(* Number, Decimal Digit *)
  | `Nl		(* Number, Letter *)
  | `No		(* Number, Other *)
  | `Zs		(* Separator, Space *)
  | `Zl		(* Separator, Line *)
  | `Zp		(* Separator, Paragraph *)
  | `Cc		(* Other, Control *)
  | `Cf		(* Other, Format *)
  | `Cs		(* Other, Surrogate *)
  | `Co		(* Other, Private Use *)
  | `Cn		(* Other, Not Assigned *)
  | `Lm		(* Letter, Modifier *)
  | `Lo		(* Letter, Other *)
  | `Pc		(* Punctuation, Connector *)
  | `Pd		(* Punctuation, Dash *)
  | `Ps		(* Punctuation, Open *)
  | `Pe		(* Punctuation, Close *)
  | `Pi		(* Punctuation, Initial quote  *)
  | `Pf		(* Punctuation, Final quote  *)
  | `Po		(* Punctuation, Other *)
  | `Sm		(* Symbol, Math *)
  | `Sc		(* Symbol, Currency *)
  | `Sk		(* Symbol, Modifier *)
  | `So ]	(* Symbol, Other *)


let cat_of_name name =
  match name with
    "Lu" -> `Lu | "Ll" -> `Ll | "Lt" -> `Lt
  | "Mn" -> `Mn | "Mc" -> `Mc | "Me" -> `Me 
  | "Nd" -> `Nd | "Nl" -> `Nl | "No" -> `No  
  | "Zs" -> `Zs | "Zl" -> `Zl | "Zp" -> `Zp 
  | "Cc" -> `Cc | "Cf" -> `Cf | "Cs" -> `Cs | "Co" -> `Co | "Cn" -> `Cn
  | "Lm" -> `Lm | "Lo" -> `Lo 
  | "Pc" -> `Pc | "Pd" -> `Pd | "Ps" -> `Ps | "Pe" -> `Pe 
  | "Pi" -> `Pi | "Pf" -> `Pf | "Po" -> `Po 
  | "Sm" -> `Sm | "Sc" -> `Sc | "Sk" -> `Sk | "So"-> `So
  | _ -> raise Not_found

let num_of_cat ca =
  match ca with
    `Lu -> 1 | `Ll -> 2 | `Lt -> 3 
  | `Mn -> 4 | `Mc -> 5 | `Me -> 6
  | `Nd -> 7 | `Nl -> 8 | `No -> 9
  | `Zs -> 10 | `Zl -> 11 | `Zp -> 12 
  | `Cc -> 13 | `Cf -> 14 | `Cs -> 15 | `Co -> 16 | `Cn -> 0
  | `Lm -> 17 | `Lo -> 18 
  | `Pc -> 19 | `Pd -> 20 | `Ps -> 21 | `Pe -> 22 
  | `Pi -> 23 | `Pf -> 24 | `Po -> 25
  | `Sm -> 26 | `Sc -> 27 | `Sk -> 28 | `So -> 29

let cat_of_num_tbl : general_category_type array = 
  [| `Cn ; `Lu ; `Ll ; `Lt ; `Mn ; `Mc ; `Me ; `Nd ; `Nl ; `No
  ; `Zs ; `Zl ; `Zp ; `Cc ; `Cf ; `Cs ; `Co
  ; `Lm ; `Lo ; `Pc ; `Pd ; `Ps ; `Pe ; `Pi ; `Pf ; `Po
  ; `Sm ; `Sc ; `Sk ; `So |]

let cat_of_num i = cat_of_num_tbl.(i)

type script_type =
  [ `Common
  | `Inherited
  | `Latin
  | `Greek
  | `Cyrillic
  | `Armenian
  | `Hebrew
  | `Arabic
  | `Syriac
  | `Thaana
  | `Devanagari
  | `Bengali
  | `Gurmukhi
  | `Gujarati
  | `Oriya
  | `Tamil
  | `Telugu
  | `Kannada
  | `Malayalam
  | `Sinhala
  | `Thai
  | `Lao
  | `Tibetan
  | `Myanmar
  | `Georgian
  | `Hangul
  | `Ethiopic
  | `Cherokee
  | `Canadian_Aboriginal
  | `Ogham
  | `Runic
  | `Khmer
  | `Mongolian
  | `Hiragana
  | `Katakana
  | `Bopomofo
  | `Han
  | `Yi
  | `Old_Italic
  | `Gothic
  | `Deseret
  | `Tagalog
  | `Hanunoo
  | `Buhid
  | `Tagbanwa ]

let script_of_name name =
  match String.lowercase name with
  | "common" -> `Common
  | "inherited" -> `Inherited
  | "latin" -> `Latin
  | "greek" -> `Greek
  | "cyrillic" -> `Cyrillic
  | "armenian" -> `Armenian
  | "hebrew" -> `Hebrew
  | "arabic" -> `Arabic
  | "syriac" -> `Syriac
  | "thaana" -> `Thaana
  | "devanagari" -> `Devanagari
  | "bengali" -> `Bengali
  | "gurmukhi" -> `Gurmukhi
  | "gujarati" -> `Gujarati
  | "oriya" -> `Oriya
  | "tamil" -> `Tamil
  | "telugu" -> `Telugu
  | "kannada" -> `Kannada
  | "malayalam" -> `Malayalam
  | "sinhala" -> `Sinhala
  | "thai" -> `Thai
  | "lao" -> `Lao
  | "tibetan" -> `Tibetan
  | "myanmar" -> `Myanmar
  | "georgian" -> `Georgian
  | "hangul" -> `Hangul
  | "ethiopic" -> `Ethiopic
  | "cherokee" -> `Cherokee
  | "canadian_aboriginal" -> `Canadian_Aboriginal
  | "ogham" -> `Ogham
  | "runic" -> `Runic
  | "khmer" -> `Khmer
  | "mongolian" -> `Mongolian
  | "hiragana" -> `Hiragana
  | "katakana" -> `Katakana
  | "bopomofo" -> `Bopomofo
  | "han" -> `Han
  | "yi" -> `Yi
  | "old_italic" -> `Old_Italic
  | "gothic" -> `Gothic
  | "deseret" -> `Deseret
  | "tagalog" -> `Tagalog
  | "hanunoo" -> `Hanunoo
  | "buhid" -> `Buhid
  | "tagbanwa" -> `Tagbanwa 
  | _ -> raise Not_found

let num_of_script = function
    `Common -> 0
  | `Inherited -> 1
  | `Latin -> 2
  | `Greek -> 3
  | `Cyrillic -> 4
  | `Armenian -> 5
  | `Hebrew -> 6
  | `Arabic -> 7
  | `Syriac -> 8
  | `Thaana -> 9
  | `Devanagari -> 10
  | `Bengali -> 11
  | `Gurmukhi -> 12
  | `Gujarati -> 13
  | `Oriya -> 14
  | `Tamil -> 15
  | `Telugu -> 16
  | `Kannada -> 17
  | `Malayalam -> 18
  | `Sinhala -> 19
  | `Thai -> 20
  | `Lao -> 21
  | `Tibetan -> 22
  | `Myanmar -> 23
  | `Georgian -> 24
  | `Hangul -> 25
  | `Ethiopic -> 26
  | `Cherokee -> 27
  | `Canadian_Aboriginal -> 28
  | `Ogham -> 29
  | `Runic -> 30
  | `Khmer -> 31
  | `Mongolian -> 32
  | `Hiragana -> 33
  | `Katakana -> 34
  | `Bopomofo -> 35
  | `Han -> 36
  | `Yi -> 37
  | `Old_Italic -> 38
  | `Gothic -> 39
  | `Deseret -> 40
  | `Tagalog -> 41
  | `Hanunoo -> 42
  | `Buhid -> 43
  | `Tagbanwa -> 44

let script_tbl =
  [| `Common; `Inherited; `Latin; `Greek; `Cyrillic; `Armenian; `Hebrew;
     `Arabic; `Syriac; `Thaana; `Devanagari; `Bengali; `Gurmukhi; `Gujarati;
     `Oriya; `Tamil; `Telugu; `Kannada; `Malayalam; `Sinhala; `Thai; `Lao;
     `Tibetan; `Myanmar; `Georgian; `Hangul; `Ethiopic; `Cherokee; 
     `Canadian_Aboriginal; `Ogham; `Runic; `Khmer; `Mongolian; `Hiragana;
     `Katakana; `Bopomofo; `Han; `Yi; `Old_Italic; `Gothic; `Deseret; `Tagalog;
     `Hanunoo; `Buhid; `Tagbanwa |]

let script_of_num i = script_tbl.(i)

type decomposition_type =
    [ `Canon | `Font | `NoBreak | `Initial | `Medial | `Final |
    `Isolated | `Circle | `Super | `Sub | `Vertical | `Wide | `Narrow |
    `Small | `Square | `Fraction | `Compat ]

type decomposition_info =
    (* Already in the canonical form *)
    [ `Canonform
    (* `Composite (dtype, text) :
     * means the given character is decomposed into text by dtype
     * decomposition. *)
  | `HangulSyllable
  | `Composite of decomposition_type * UChar.t list ]

type ce_type = int

let primary_mask = 0xffff lsl 15
let secondary_mask = 0xff lsl 7
let tertiary_mask = 0x7f

let primary ce = (ce land primary_mask) lsr 15
let secondary ce = (ce land secondary_mask) lsr 7
let tertiary ce = ce land tertiary_mask
let compose_ce w1 w2 w3 = (w1 lsl 15) lor (w2 lsl 7) lor w3
let complete_ignorable = 0

type ce_tbl = (UChar.t list * ce_type list) list UCharTbl.t

type variable_option =   
  [ `Blanked 
  | `Non_ignorable 
  | `Shifted
  | `Shift_Trimmed ]

type col_info =
    {variable_top : int;
     variable_option : variable_option;
     french_accent : bool;		
     hiraganaQ : bool;
     hiraganaQ_weight : int;
     tbl : ce_tbl}

let default_col_data =
  lazy (read_data "allkeys" : col_info)

type localedata =
    {col_info : col_info option}

let read_localedata c =
  let data : localedata = input_value c in
  match data.col_info with
    None -> raise Not_found
  | Some x -> x

let col_tbl = Hashtbl.create 0

let get_col_info ?locale () =
  match locale with
    None -> Lazy.force default_col_data
  | Some s ->
      try 
	let b = Hashtbl.find col_tbl s in
	match Weak.get b 0 with
	  None ->
	    Hashtbl.remove col_tbl s;
	    raise Not_found
	| Some x -> x
      with Not_found ->
	try 
	  let info = Locale.read Config.localedir "mar" read_localedata s in
	  let b = Weak.create 1 in
	  Weak.set b 0 (Some info);
	  Hashtbl.add col_tbl s b;
	  info
	with
	  Not_found -> Lazy.force default_col_data

let ce tbl u = UCharTbl.get tbl u
end
