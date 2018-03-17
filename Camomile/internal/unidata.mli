(** Unicode data *)

(* Copyright (C) 2002, 2003 Yamagata Yoriyuki *)

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

module Make (Config : ConfigInt.Type) : Type
