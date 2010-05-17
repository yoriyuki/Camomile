(* $Id: uSet.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** Sets of Unicode characters, implemented as sets of intervals.
   The signature is mostly same to Set.S in stdlib *)
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
