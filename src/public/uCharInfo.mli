(** Unicode character informations *)
(* Copyright (C) 2002, 2003 Yamagata Yoriyuki.*)
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
(* yoriyuki.y@gmail.com *)

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
    [ `Lu  (** Letter, Uppercase *)
    | `Ll  (** Letter, Lowercase *)
    | `Lt  (** Letter, Titlecase *)
    | `Mn  (** Mark, Non-Spacing *)
    | `Mc  (** Mark, Spacing Combining *)
    | `Me  (** Mark, Enclosing *)
    | `Nd  (** Number, Decimal Digit *)
    | `Nl  (** Number, Letter *)
    | `No  (** Number, Other *)
    | `Zs  (** Separator, Space *)
    | `Zl  (** Separator, Line *)
    | `Zp  (** Separator, Paragraph *)
    | `Cc  (** Other, Control *)
    | `Cf  (** Other, Format *)
    | `Cs  (** Other, Surrogate *)
    | `Co  (** Other, Private Use *)
    | `Cn  (** Other, Not Assigned *)
    | `Lm  (** Letter, Modifier *)
    | `Lo  (** Letter, Other *)
    | `Pc  (** Punctuation, Connector *)
    | `Pd  (** Punctuation, Dash *)
    | `Ps  (** Punctuation, Open *)
    | `Pe  (** Punctuation, Close *)
    | `Pi  (** Punctuation, Initial quote  *)
    | `Pf  (** Punctuation, Final quote  *)
    | `Po  (** Punctuation, Other *)
    | `Sm  (** Symbol, Math *)
    | `Sc  (** Symbol, Currency *)
    | `Sk  (** Symbol, Modifier *)
    | `So  (** Symbol, Other *) ]

  val general_category : UChar.t -> general_category_type
  val load_general_category_map : unit -> general_category_type UMap.t

  (** Type of character properties *)
  type character_property_type =
    [ `Math  (**Derived Core Properties*)
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
    | `Bidi_Control  (**Extended Properties*)
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
    [ `Nc  (** undefined code point *)
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

  type special_casing_property = {
    lower : UChar.t list;
    title : UChar.t list;
    upper : UChar.t list;
    condition : casemap_condition list;
  }

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
    [ `Canon
    | `Font
    | `NoBreak
    | `Initial
    | `Medial
    | `Final
    | `Isolated
    | `Circle
    | `Super
    | `Sub
    | `Vertical
    | `Wide
    | `Narrow
    | `Small
    | `Square
    | `Fraction
    | `Compat ]

  type decomposition_info =
    [ `Canonform  (** Already in the canonical form *)
    | `HangulSyllable  (** Hangul is treated algotighmically.*)
    | `Composite of decomposition_type * UChar.t list
      (** [`Composite (dtype, text)] means the given character is decomposed into
          text by dtype decomposition. *)
    ]

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

module Make (_ : Config.Type) : Type
