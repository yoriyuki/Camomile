(** An implementation of Unicode string. *)

(* Copyright (C) 2002, 2003 Yamagata Yoriyuki. *)

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

(** An implementation of Unicode string.  
    Internally, it uses integer array.
    The semantics matches the description of UStorage. *)

(** Phantom type for distinguishing mutability *)
type mutability = [ `Mutable | `Immutable ]

type 'a text
type utext = [`Immutable] text
type ustring = [`Mutable] text
type t = utext

val utext_of_ustring : ustring -> utext
val ustring_of_utext : utext -> ustring
val get :  'a text -> int -> UChar.t

(** [set s i u] sets the [i]-th character in [s] to [u]. *)
val set :  ustring -> int -> UChar.t -> unit

type index

val look : 'a text -> index -> UChar.t
val nth : 'a text -> int -> index
val first : 'a text -> index
val last : 'a text -> index
val out_of_range : 'a text -> index -> bool
val compare_index : 'a text -> index -> index -> int
val next : 'a text -> index -> index
val prev : 'a text -> index -> index
val move : 'a text -> index -> int -> index

val length : 'a text -> int

(** Conversion from Latin-1 strings. *)
val of_string : string -> utext

val init : int -> (int -> UChar.t) -> utext
val init_ustring : int -> (int -> UChar.t) -> ustring

(** The semantics of these function are similar to 
    the equivalents of string. *)
val make : int -> UChar.t -> ustring
val copy : ustring -> ustring
val sub : 'a text -> int -> int -> 'a text
val fill : ustring -> int -> int -> UChar.t -> unit
val blit : 'a text -> int -> ustring -> int -> int -> unit
val append : 'a text -> 'b text -> 'a text
val iter : (UChar.t -> unit) -> 'a text -> unit
val compare : 'a text -> 'b text -> int

module Buf : sig
  type buf

  (** [create n] creates the buffer which initially can contain
      [n] Unicode characters. *)
  val create : int -> buf

  val contents : buf -> t
  val contents_string : buf -> ustring
  val length : buf -> int
  val clear : buf -> unit
  val reset : buf -> unit
  val add_char : buf -> UChar.t -> unit
  val add_string : buf -> 'a text -> unit
  val add_buffer : buf -> buf -> unit
end
