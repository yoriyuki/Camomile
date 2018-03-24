(** Collaiton Element, abstracted *)

(* Copyright (C) 2003 Yamagata Yoriyuki *)

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

open CamomileLibrary

type elt =
  [ `Seq of UChar.t list
  | `ImplicitWeight of int list
  | `CompleteIgnorable
  | `UCA_Weight of int
  | `LastVariable
  | `HiraganaQ
  | `FirstImplicit
  | `FirstTrailing ]

type ce

type ceset

module EltMap : Map.S with type key = elt

val ces_of : ceset -> UChar.t list -> ceset * ce list
val complete_ignorable : ceset -> ce
val last_variable : ceset -> ce
val first_implicit : ceset -> ce
val first_trailing : ceset -> ce
val top : ceset -> ce
val next : UCol.precision -> ce -> ceset -> ce
val prev : UCol.precision -> ce -> ceset -> ce
val add_after : UCol.precision -> ce -> ceset -> ce * ceset
val add_before : UCol.precision -> ce -> ceset -> ce * ceset
val put : elt -> ce list -> ceset -> ceset

val import : int list EltMap.t * int list EltMap.t * int list EltMap.t -> ceset

type ace_info =
  {ceset : ceset;
   variable_option : UCol.variable_option;
   french : bool;
   hiraganaQ : bool}

val create_ace_info :
  ?variable_option:UCol.variable_option ->
  ?french:bool ->
  ?hiraganaQ:bool ->
  ceset ->
  ace_info

val cetbl_of : ace_info -> Toolslib.Unidata.col_info

type aceset_info =
  {lowercase_first_tbl : ceset;
   uppercase_first_tbl : ceset}
