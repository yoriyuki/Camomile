(* Copyright (C) 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

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

(** Sub-texts, parts of original (ur-) texts.  
   The signature and semantics matches those of UStorage. *)
module type Type = sig
  type t

  val get : t -> int -> UChar.t

  val init : int -> (int -> UChar.t) -> t
  val length : t -> int

  type index
  val look : t -> index -> UChar.t
  val nth : t -> int -> index
  val first : t -> index
  val last : t -> index

  val next : t -> index -> index
  val prev : t -> index -> index
  val move : t -> index -> int -> index
  val out_of_range : t -> index -> bool
  val compare_index : t -> index -> index -> int
      
  val iter : (UChar.t -> unit) -> t -> unit
  val compare : t -> t -> int

  module Buf : sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end      

(** The type of original texts. *)
  type ur_text

(** The type of indexes of original texts. *)
  type ur_index

(** [refer t i j] returns the part of [t] from [i] until [j].
   The character pointed by [j] is not included in the result.
   If [j] is equal to [i] or located before [j], the result is
   an empty string. *)
  val refer : ur_text -> ur_index -> ur_index -> t

(** [excerpt t] copies the contents of [t] as a new ur_text. *)
  val excerpt : t -> ur_text

(** [context t] returns the tuple [(s, i, j)] such that
   [t = refer s i j]. *)
  val context : t -> ur_text * ur_index * ur_index

(** Conversion from indexes of sub-texts to ur_texts. *)
  val ur_index_of : t -> index -> ur_index
end

module Make : functor (Text : UnicodeString.Type) -> 
  (Type with type ur_text = Text.t and type ur_index = Text.index)
