(** Sets of Unicode characters, implemented as sets of intervals.
    The signature is mostly same to Set.S in stdlib *)

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
(* yoriyuki.y@gmail.com *)

type t

val empty : t
val is_empty : t -> bool
val mem : UChar.t -> t -> bool
val add : UChar.t -> t -> t

(** [add_range u1 u2 s] adds the characters in the range [u1] - [u2]
    to [s].  The range is determined by the code point order. *)
val add_range : UChar.t -> UChar.t -> t -> t

val singleton : UChar.t -> t
val remove : UChar.t -> t -> t

(** [remove_range u1 u2 s] removes the characters in the range [u1] - [u2]
    from [s].  The range is determined by the code point order. *)
val remove_range : UChar.t -> UChar.t -> t -> t

val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t

(** [compl s] returns the compliment of [s]. *)
val compl : t -> t

val compare : t -> t -> int
val equal : t -> t -> bool
val subset : t -> t -> bool

(** [from u s] returns the set of elements of [s] 
    whose code points are equal or greater than [u]. *)
val from : UChar.t -> t -> t

(** [after u s] returns the set of elements of [s] 
    whose code points are greater than [u]. *)
val after : UChar.t -> t -> t

(** [until u s] returns the set of elements of [s] 
    whose code points are equal or smaller than [u]. *)
val until : UChar.t -> t -> t

(** [until u s] returns the set of elements of [s] 
    whose code points are smaller than [u]. *)
val before : UChar.t -> t -> t

val iter : (UChar.t -> unit) -> t -> unit

(** [iter_range proc s] feeds the intervals contained in [s] to
    [proc] in increasing order.  The intervals given to [proc]
    are always separated by the character not in [s]. *)
val iter_range : (UChar.t -> UChar.t -> unit) -> t -> unit

val fold : (UChar.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [fold_range f s x] is equivalent to 
    [f u_i u_(i+1) (... (f u_3 u_4 (f u_1 u_2 x)))] if [s] is consisted of
    the intervals [u1]-[u2], [u3]-[u4], ..., [u_i]-[u_(i + 1)]
    in increasing order.  The intervals given to [proc]
    are always separated by the character not in [s]. *)
val fold_range : (UChar.t -> UChar.t -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (UChar.t -> bool) -> t -> bool
val exists : (UChar.t -> bool) -> t -> bool
val filter : (UChar.t -> bool) -> t -> t
val partition : (UChar.t -> bool) -> t -> t * t
val cardinal : t -> int
val elements : t -> UChar.t list

(** The list of the intervals contained in the set.  
    The returned intervals are always separated 
    by the character not in [s]. *)
val ranges : t -> (UChar.t * UChar.t) list

val min_elt : t -> UChar.t
val max_elt : t -> UChar.t

(** Returns a element roughly in the middle of the set. 
    It is not guaranteed to return the same element for 
    the sets with the same elements *)
val choose : t -> UChar.t

val uset_of_iset : ISet.t -> t
val iset_of_uset : t -> ISet.t
