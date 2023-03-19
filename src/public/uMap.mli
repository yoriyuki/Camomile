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

(** Maps over Unicode characters. *)
type 'a t

val empty : 'a t
val is_empty : 'a t -> bool

(** [add ?eq u v m] returns the new map which is same to [m]
    except it maps [u] to some value [v'] which satisfies [eq v v'].
    If [eq] is not supplied, structural equality is used. *)
val add : ?eq:('a -> 'a -> bool) -> UChar.t -> 'a -> 'a t -> 'a t

(** [add ?eq u1 u2 v m] returns the new map which is same to [m]
    except it maps characters in the range [u1]-[u2]
    to some value [v'] which satisfies [eq v v'].
    If [eq] is not supplied, structural equality is used. *)
val add_range :
  ?eq:('a -> 'a -> bool) -> UChar.t -> UChar.t -> 'a -> 'a t -> 'a t

val find : UChar.t -> 'a t -> 'a
val remove : UChar.t -> 'a t -> 'a t

(** [remove_range u1 u2 m] removes [u1]-[u2] from the domain of [m] *)
val remove_range : UChar.t -> UChar.t -> 'a t -> 'a t

(** [from u m] restricts the domain of [m] to the characters whose
    code points are equal or greater than [u]. *)
val from : UChar.t -> 'a t -> 'a t

(** [after u m] restricts the domain of [m] to the characters whose
    code points are greater than [u]. *)
val after : UChar.t -> 'a t -> 'a t

(** [until u m] restricts the domain of [m] to the characters whose
    code points are equal or smaller than [u]. *)
val until : UChar.t -> 'a t -> 'a t

(** [before u m] restricts the domain of [m] to the characters whose
    code points are smaller than [u]. *)
val before : UChar.t -> 'a t -> 'a t

val mem : UChar.t -> 'a t -> bool
val iter : (UChar.t -> 'a -> unit) -> 'a t -> unit

(** [iter proc m] : For each contingent region [u1]-[u2]
    that is mapped to a constant [v], [proc u1 u2 v] is called.
    The order of call is determined by increasing order on [u1]. *)
val iter_range : (UChar.t -> UChar.t -> 'a -> unit) -> 'a t -> unit

(** [map ?eq f m] and [mapi ?eq f m] :  Similar to [map] and [mapi]
    in stdlib Map, but if the map [m'] is returned,  it is only guaranteed
    that [eq (find u m') (f (find u m ))] is true for [map] and
    [eq (find u m') (f u (find u m ))] is true for [mapi].  If [eq] is
    not specified, structural equality is used. *)

val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
val mapi : ?eq:('b -> 'b -> bool) -> (UChar.t -> 'a -> 'b) -> 'a t -> 'b t
val fold : (UChar.t -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a

(** [fold_range f m x] is equivalent to
    [f u_(2n) u_(2n+1) v_n (... (f u_1 u_2 v_1 x))] where all characters in
    the range [u_(2k)]-[u_(2k+1)] are mapped to [v_k] and
    [u_1] < [u_3] < ... in code point order.
    For each range [u_(2k)]-[u_(2k+1)] is separated by a character
    which is not mapped to [v_k]. *)
val fold_range : (UChar.t -> UChar.t -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a

(** Constant map.*)
val set_to_map : USet.t -> 'a -> 'a t

(** Domain. *)
val domain : 'a t -> USet.t

(** [map_to_set p m] returns the set of characters which are mapped
    to values satisfying the predicate [p] by [m]. *)
val map_to_set : ('a -> bool) -> 'a t -> USet.t

val umap_of_imap : 'a IMap.t -> 'a t
val imap_of_umap : 'a t -> 'a IMap.t
