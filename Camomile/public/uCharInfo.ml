(** Unicode character informations *)
(* Copyright (C) 2002 Yamagata Yoriyuki.*)
(*               2010 Pierre Chambart *)

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

(** Character Information *)

(** Type of Unicode general character categories.  
   Each variant specifies
 - [`Lu] : Letter, Uppercase 
 - [`Ll] : Letter, Lowercase 
 - [`Lt] : Letter, Titlecase 
 - [`Mn] : Mark, Non-Spacing 
 - [`Mc] : Mark, Spacing Combining 
 - [`Me] : Mark, Enclosing 
 - [`Nd] : Number, Decimal Digit 
 - [`Nl] : Number, Letter 
 - [`No] : Number, Other 
 - [`Zs] : Separator, Space 
 - [`Zl] : Separator, Line 
 - [`Zp] : Separator, Paragraph 
 - [`Cc] : Other, Control 
 - [`Cf] : Other, Format 
 - [`Cs] : Other, Surrogate 
 - [`Co] : Other, Private Use 
 - [`Cn] : Other, Not Assigned 
 - [`Lm] : Letter, Modifier 
 - [`Lo] : Letter, Other 
 - [`Pc] : Punctuation, Connector 
 - [`Pd] : Punctuation, Dash 
 - [`Ps] : Punctuation, Open 
 - [`Pe] : Punctuation, Close 
 - [`Pi] : Punctuation, Initial
 - [`Pf] : Punctuation, Final
 - [`Po] : Punctuation, Other 
 - [`Sm] : Symbol, Math 
 - [`Sc] : Symbol, Currency 
 - [`Sk] : Symbol, Modifier 
 - [`So] : Symbol, Other  *)
type general_category_type = 
  [ `Lu		(** Letter, Uppercase *)
  | `Ll		(** Letter, Lowercase *)
  | `Lt		(** Letter, Titlecase *)
  | `Mn		(** Mark, Non-Spacing *)
  | `Mc		(** Mark, Spacing Combining *)
  | `Me		(** Mark, Enclosing *)
  | `Nd		(** Number, Decimal Digit *)
  | `Nl		(** Number, Letter *)
  | `No		(** Number, Other *)
  | `Zs		(** Separator, Space *)
  | `Zl		(** Separator, Line *)
  | `Zp		(** Separator, Paragraph *)
  | `Cc		(** Other, Control *)
  | `Cf		(** Other, Format *)
  | `Cs		(** Other, Surrogate *)
  | `Co		(** Other, Private Use *)
  | `Cn		(** Other, Not Assigned *)
  | `Lm		(** Letter, Modifier *)
  | `Lo		(** Letter, Other *)
  | `Pc		(** Punctuation, Connector *)
  | `Pd		(** Punctuation, Dash *)
  | `Ps		(** Punctuation, Open *)
  | `Pe		(** Punctuation, Close *)
  | `Pi		(** Punctuation, Initial quote  *)
  | `Pf		(** Punctuation, Final quote  *)
  | `Po		(** Punctuation, Other *)
  | `Sm		(** Symbol, Math *)
  | `Sc		(** Symbol, Currency *)
  | `Sk		(** Symbol, Modifier *)
  | `So	        (** Symbol, Other *) ]

val general_category : UChar.t -> general_category_type
val load_general_category_map : unit -> general_category_type UMap.t

(** Type of character properties *)
type character_property_type = [

(**Derived Core Properties*)

    `Math				
  | `Alphabetic
  | `Lowercase
  | `Uppercase
  | `ID_Start
  | `ID_Continue
  | `XID_Start
  | `XID_Continue
  | `Default_Ignorable_Code_Point
  | `Grapheme_Extend
  | `Grapheme_Base

(**Extended Properties*)

  | `Bidi_Control			
  | `White_Space
  | `Hyphen
  | `Quotation_Mark
  | `Terminal_Punctuation
  | `Other_Math
  | `Hex_Digit
  | `Ascii_Hex_Digit
  | `Other_Alphabetic
  | `Ideographic
  | `Diacritic
  | `Extender
  | `Other_Lowercase
  | `Other_Uppercase
  | `Noncharacter_Code_Point
  | `Other_Grapheme_Extend
  | `Grapheme_Link
  | `IDS_Binary_Operator
  | `IDS_Trinary_Operator
  | `Radical
  | `Unified_Ideograph
  | `Other_default_Ignorable_Code_Point
  | `Deprecated
  | `Soft_Dotted
  | `Logical_Order_Exception ]

(** Load the table for the given character type. *)
val load_property_tbl : character_property_type -> UCharTbl.Bool.t

(** Load the table for the given name of the character type.
   The name can be obtained by removing ` from its name of 
   the polymorphic variant tag. *)
val load_property_tbl_by_name : string -> UCharTbl.Bool.t

(** Load the set of characters of the given character type. *)
val load_property_set : character_property_type -> USet.t

(** Load the set of characters of the given name of the character type.
   The name can be obtained by removing ` from its name of 
   the polymorphic variant tag. *)
val load_property_set_by_name : string -> USet.t

(** Type for script type *)
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

val script : UChar.t -> script_type
val load_script_map : unit -> script_type UMap.t

(** age *)
type version_type =
  [ `Nc		(** undefined code point *)
  | `v1_0
  | `v1_1
  | `v2_0
  | `v2_1
  | `v3_0
  | `v3_1
  | `v3_2 ]

(** [age c] unicode version in wich [c] was introduced *)
val age : UChar.t -> version_type
(** [older v1 v2] is [true] if [v1] is older ( or the same version )
    than [v2]. Everithing is older than [`Nc] *)
val older : version_type -> version_type -> bool

(** casing *)

val load_to_lower1_tbl : unit -> UChar.t UCharTbl.t
val load_to_upper1_tbl : unit -> UChar.t UCharTbl.t
val load_to_title1_tbl : unit -> UChar.t UCharTbl.t

type casemap_condition =
  [ `Locale of string
  | `FinalSigma
  | `AfterSoftDotted
  | `MoreAbove
  | `BeforeDot
  | `Not of casemap_condition ]

type special_casing_property =
  {lower : UChar.t list;
  title : UChar.t list;
  upper : UChar.t list;
  condition : casemap_condition list;} 

val load_conditional_casing_tbl : 
    unit -> special_casing_property list UCharTbl.t

val load_casefolding_tbl : unit -> UChar.t list UCharTbl.t

(** Combined class
   A combined class is an integer of 0 -- 255, showing how this character
   interacts to other combined characters.  *)
val combined_class : UChar.t -> int

(** Decomposition *)

(** Types of decomposition. *)
type decomposition_type =
    [ `Canon | `Font | `NoBreak | `Initial | `Medial | `Final |
    `Isolated | `Circle | `Super | `Sub | `Vertical | `Wide | `Narrow |
    `Small | `Square | `Fraction | `Compat ]

type decomposition_info =
(** Already in the canonical form *)
    [ `Canonform
(** Hangul is treated algotighmically.*)
  | `HangulSyllable
(** [`Composite (dtype, text)]
   means the given character is decomposed into text by dtype
   decomposition. *)
  | `Composite of decomposition_type * UChar.t list ]

val load_decomposition_tbl : unit -> decomposition_info UCharTbl.t

(** Canonical Composition *)

(** The return value [[(u_1, u'_1); ... (u_n, u'_1)]] means
   for the given character [u], [u u_i] forms 
   the canonical composition [u'_i].
   If u is a Hangul jamo, composition returns []. *)
val load_composition_tbl : unit -> (UChar.t * UChar.t) list UCharTbl.t

(** Whether the given composed character is used in NFC or NFKC *)
val load_composition_exclusion_tbl : unit -> UCharTbl.Bool.t

end

module Make (Config : ConfigInt.Type) = struct
include Unidata.Make(Config)

(* General category *)

let general_category_tbl : UCharTbl.Bits.t = 
  read_data "general_category"

let general_category u =
  match UCharTbl.Bits.get general_category_tbl u with
    0 ->
      let n = UChar.uint_code u in
      if n >= 0x0f0000 && n <= 0x100000 then `Co else
      if n >= 0xe00000 && n <= 0xff0000 then `Co else
      if n >= 0x60000000 && n <= 0x7f000000 then `Co else `Cn
  | x -> cat_of_num x

let load_general_category_map () = 
  read_data "general_category_map"

(* character property *)

type character_property_type =
  [ `Math				(*Derived Core Properties*)
  | `Alphabetic
  | `Lowercase
  | `Uppercase
  | `ID_Start
  | `ID_Continue
  | `XID_Start
  | `XID_Continue
  | `Default_Ignorable_Code_Point
  | `Grapheme_Extend
  | `Grapheme_Base
  | `Bidi_Control			(*Extended Properties*)
  | `White_Space
  | `Hyphen
  | `Quotation_Mark
  | `Terminal_Punctuation
  | `Other_Math
  | `Hex_Digit
  | `Ascii_Hex_Digit
  | `Other_Alphabetic
  | `Ideographic
  | `Diacritic
  | `Extender
  | `Other_Lowercase
  | `Other_Uppercase
  | `Noncharacter_Code_Point
  | `Other_Grapheme_Extend
  | `Grapheme_Link
  | `IDS_Binary_Operator
  | `IDS_Trinary_Operator
  | `Radical
  | `Unified_Ideograph
  | `Other_default_Ignorable_Code_Point
  | `Deprecated
  | `Soft_Dotted
  | `Logical_Order_Exception ]

let name_of_property p =
  match p with
    `Math -> "Math"  			
  | `Alphabetic -> "Alphabetic"
  | `Lowercase -> "Lowercase"
  | `Uppercase -> "Uppercase"
  | `ID_Start -> "ID_Start"
  | `ID_Continue -> "ID_Continue"
  | `XID_Start -> "XID_Start"
  | `XID_Continue -> "XID_Continue"
  | `Default_Ignorable_Code_Point -> "Default_Ignorable_Code_Point"
  | `Grapheme_Extend -> "Grapheme_Extend"
  | `Grapheme_Base -> "Grapheme_Base"
  | `Bidi_Control -> "Bidi_Control"			
  | `White_Space -> "White_Space"
  | `Hyphen -> "Hyphen"
  | `Quotation_Mark -> "Quotation_Mark"
  | `Terminal_Punctuation -> "Terminal_Punctuation"
  | `Other_Math -> "Other_Math"
  | `Hex_Digit -> "Hex_Digit"
  | `Ascii_Hex_Digit -> "Ascii_Hex_Digit"
  | `Other_Alphabetic -> "Other_Alphabetic"
  | `Ideographic -> "Ideographic"
  | `Diacritic -> "Diacritic"
  | `Extender -> "Extender"
  | `Other_Lowercase -> "Other_Lowercase"
  | `Other_Uppercase -> "Other_Uppercase"
  | `Noncharacter_Code_Point -> "Noncharacter_Code_Point"
  | `Other_Grapheme_Extend -> "Other_Grapheme_Extend"
  | `Grapheme_Link -> "Grapheme_Link"
  | `IDS_Binary_Operator -> "IDS_Binary_Operator"
  | `IDS_Trinary_Operator -> "IDS_Trinary_Operator"
  | `Radical -> "Radical"
  | `Unified_Ideograph -> "Unified_Ideograph"
  | `Other_default_Ignorable_Code_Point -> "Other_default_Ignorable_Code_Point"
  | `Deprecated -> "Deprecated"
  | `Soft_Dotted -> "Soft_Dotted"
  | `Logical_Order_Exception -> "Logical_Order_Exception"

let property_of_name : string -> character_property_type = function
    "Math" -> `Math
  | "Alphabetic"  -> `Alphabetic
  | "Lowercase" -> `Lowercase
  | "Uppercase" -> `Uppercase
  | "ID_Start" -> `ID_Start
  | "ID_Continue" -> `ID_Continue
  | "XID_Start" -> `XID_Start
  | "XID_Continue" -> `XID_Continue
  | "Default_Ignorable_Code_Point" -> `Default_Ignorable_Code_Point
  | "Grapheme_Extend" -> `Grapheme_Extend
  | "Grapheme_Base" -> `Grapheme_Base
  | "Bidi_Control" -> `Bidi_Control
  | "White_Space" -> `White_Space
  | "Hyphen" -> `Hyphen
  | "Quotation_Mark" -> `Quotation_Mark
  | "Terminal_Punctuation" -> `Terminal_Punctuation
  | "Other_Math" -> `Other_Math
  | "Hex_Digit" -> `Hex_Digit
  | "Ascii_Hex_Digit" -> `Ascii_Hex_Digit
  | "Other_Alphabetic" -> `Other_Alphabetic
  | "Ideographic" -> `Ideographic
  | "Diacritic" -> `Diacritic
  | "Extender" -> `Extender
  | "Other_Lowercase" -> `Other_Lowercase
  | "Other_Uppercase" -> `Other_Uppercase
  | "Noncharacter_Code_Point" -> `Noncharacter_Code_Point
  | "Other_Grapheme_Extend" -> `Other_Grapheme_Extend
  | "Grapheme_Link" -> `Grapheme_Link
  | "IDS_Binary_Operator" -> `IDS_Binary_Operator
  | "IDS_Trinary_Operator" -> `IDS_Trinary_Operator
  | "Radical" -> `Radical
  | "Unified_Ideograph" -> `Unified_Ideograph
  | "Other_default_Ignorable_Code_Point" -> `Other_default_Ignorable_Code_Point
  | "Deprecated" -> `Deprecated
  | "Soft_Dotted" -> `Soft_Dotted
  | "Logical_Order_Exception" -> `Logical_Order_Exception
  | _ -> raise Not_found

let loaded_props = Hashtbl.create 0

let load_property_tbl p = 
  try 
    let b = Hashtbl.find loaded_props p in
    match Weak.get b 0 with
      None -> 
	Hashtbl.remove loaded_props p;
	raise Not_found
    | Some x -> x
  with Not_found ->
    let tbl = read_data (name_of_property p) in
    let b = Weak.create 1 in
    Weak.set b 0 (Some tbl);
    Hashtbl.add loaded_props p b;
    tbl

let load_property_tbl_by_name s = 
   load_property_tbl (property_of_name s)

let loaded_prop_sets = Hashtbl.create 0

let load_property_set p = 
  try 
    let b = Hashtbl.find loaded_prop_sets p in
    match Weak.get b 0 with
      None -> 
	Hashtbl.remove loaded_prop_sets p;
	raise Not_found
    | Some x -> x
  with Not_found ->
    let tbl = read_data  ((name_of_property p) ^ "_set") in
    let b = Weak.create 1 in
    Weak.set b 0 (Some tbl);
    Hashtbl.add loaded_prop_sets p b;
    tbl

let load_property_set_by_name s =
  load_property_set (property_of_name s)

(* Scripts *)

let script_tbl : UCharTbl.Bits.t = read_data "scripts"

let script u = script_of_num (UCharTbl.Bits.get script_tbl u)
let load_script_map () = read_data "scripts_map"

(** age *)

type version_type =
  [ `Nc
  | `v1_0
  | `v1_1
  | `v2_0
  | `v2_1
  | `v3_0
  | `v3_1
  | `v3_2 ]

let version_of_char = function
  | '\x10' -> `v1_0
  | '\x11' -> `v1_1
  | '\x20' -> `v2_0
  | '\x21' -> `v2_1
  | '\x30' -> `v3_0
  | '\x31' -> `v3_1
  | '\x32' -> `v3_2
  | '\xfe' -> `Nc
  | i -> failwith (Printf.sprintf "version_of_char, unknown version v%x" (Char.code i))

let version_to_char = function
  | `v1_0 -> '\x10'
  | `v1_1 -> '\x11'
  | `v2_0 -> '\x20'
  | `v2_1 -> '\x21'
  | `v3_0 -> '\x30'
  | `v3_1 -> '\x31'
  | `v3_2 -> '\x32'
  | `Nc   -> '\xfe'

let age_tbl : UCharTbl.Char.t = read_data  "age"

let age u = version_of_char (UCharTbl.Char.get age_tbl u)
let older v1 v2 =
  ( version_to_char v1 ) <= ( version_to_char v2 )

(* Casing *)

let cache = Weak.create 3

let load_to_lower1_tbl () =
  match Weak.get cache 0 with
    Some t -> t
  | None ->
      let t = read_data  "to_lower1" in
      Weak.set cache 0 (Some t);
      t
      
let load_to_upper1_tbl () =
  match Weak.get cache 1 with
    Some t -> t
  | None ->
      let t = read_data  "to_upper1" in
      Weak.set cache 1 (Some t);
      t

let load_to_title1_tbl () =
  match Weak.get cache 2 with
    Some t -> t
  | None ->
      let t = read_data  "to_title1" in
      Weak.set cache 2 (Some t);
      t

type casemap_condition =
  [ `Locale of string
  | `FinalSigma
  | `AfterSoftDotted
  | `MoreAbove
  | `BeforeDot
  | `Not of casemap_condition ]

type special_casing_property =
  {lower : UChar.t list;
  title : UChar.t list;
  upper : UChar.t list;
  condition : casemap_condition list;} 

let cache = Weak.create 1

let load_conditional_casing_tbl () =
  match Weak.get cache 0 with
    Some t -> t
  | None ->
      let t = read_data  "special_casing" in
      Weak.set cache 0 (Some t);
      t

let cache = Weak.create 1

let load_casefolding_tbl () =
  match Weak.get cache 0 with
    Some t -> t
  | None ->
      let t = read_data  "case_folding" in
      Weak.set cache 0 (Some t);
      t

(* Combined class *)

let combined_class_tbl : UCharTbl.Char.t =
  read_data  "combined_class"

let combined_class u = Char.code (UCharTbl.Char.get combined_class_tbl u)

(* Decomposition *)

let cache = Weak.create 1

let load_decomposition_tbl () =
  match Weak.get cache 0 with
    Some t -> t
  | None ->
      let t = read_data  "decomposition" in
      Weak.set cache 0 (Some t);
      t

(* Composition *)

let cache = Weak.create 1

let load_composition_tbl () =
  match Weak.get cache 0 with
    Some t -> t
  | None ->
      let t = read_data  "composition" in
      Weak.set cache 0 (Some t);
      t

let cache = Weak.create 1

let load_composition_exclusion_tbl () = 
  match Weak.get cache 0 with
    Some t -> t
  | None ->
      let t = read_data  "composition_exclusion" in
      Weak.set cache 0 (Some t);
      t
end
