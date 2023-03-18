(** Unicode collation algorithm *)

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
(* yoriyuki.y@gmail.com *)

(** String comparison by collation as described in UTR #10 *)

(** How variables are handled *)
type variable_option = [ `Blanked | `Non_ignorable | `Shifted | `Shift_Trimmed ]

(** Strength of comparison.  For European languages, each strength
    roughly means as
    `Primary : Ignore accents and case
    `Secondary : Ignore case but accents are counted in.
    `Tertiary : Accents and case are counted in.
    For the case of `Shifted, `Shift_Trimmed, there is the fourth strength.
    `Quaternary : Variables such as - (hyphen) are counted in. *)
type precision = [ `Primary | `Secondary | `Tertiary | `Quaternary ]

module type Type = sig
  type text
  type index

  (** For locale, see {!Locale}.
      	      If [locale] is omitted, the standard UCA order is used.
      	      If [prec] is omitted, the maximum possible strength is used.
      	      If [variable] is omitted, the default of the locale 
      	      (usually [`Shifted]) is used.
      	      The meaning of the returned value is similar to Stdlib.compare *)
  val compare :
    ?locale:string ->
    ?prec:precision ->
    ?variable:variable_option ->
    text ->
    text ->
    int

  (** Binary comparison of sort_key gives the same result as [compare]. 
      		  i.e.
      		  [compare t1 t2 = Stdlib.compare (sort_key t1) (sort_key t2)]
      		  If the same texts are repeatedly compared, 
      		  pre-computation of sort_key gives better performance. *)
  val sort_key :
    ?locale:string ->
    ?prec:precision ->
    ?variable:variable_option ->
    text ->
    string

  (** Comparison with the sort key. *)
  val compare_with_key :
    ?locale:string ->
    ?prec:precision ->
    ?variable:variable_option ->
    string ->
    text ->
    int

  val search_with_key :
    ?locale:string ->
    ?prec:precision ->
    ?variable:variable_option ->
    string ->
    text ->
    index ->
    index * index

  val search :
    ?locale:string ->
    ?prec:precision ->
    ?variable:variable_option ->
    text ->
    text ->
    index ->
    index * index
end

module Make (_ : ConfigInt.Type) (Text : UnicodeString.Type) :
  Type with type text = Text.t and type index = Text.index
