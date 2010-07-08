(* $Id: uCharTbl.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** Fast lookup tables.  Accessible by constant time. *)
  type 'a tbl
  type 'a t = 'a tbl

  val get : 'a tbl -> UChar.t -> 'a

  module type Type = sig
    type elt
    type t = elt tbl
    val get : elt tbl -> UChar.t -> elt

(** [of_map def m] creates the table which has the same value to [m].
   The table returns [def] for the characters for which [m] is undefined. *)
    val of_map : elt -> elt UMap.t -> t
  end

(** Equality and hash are necessary for table generation. *)
  module Make : 
  functor (H : Hashtbl.HashedType) ->  (Type with type elt = H.t)

(** Tables for boolean values. *)
  module Bool : sig
    type t
    val get : t -> UChar.t -> bool
    val of_set : USet.t -> t
  end

(** Tables for small (< 256, >=0) integers *)
  module Bits : sig
    type t
    val of_map : int -> int UMap.t -> t
    val get : t -> UChar.t -> int
  end

(** Tables for integers.  If integers are not span the whole 31-bit or
63-bit values, [Bytes.t] is more space efficient than [int tbl]. *)
  module Bytes : sig
    type t
    val of_map : int -> int UMap.t -> t
    val get : t -> UChar.t -> int
  end

(** Tables for bytes. *)
  module Char : sig
    type t
    val of_map : char -> char UMap.t -> t
    val get : t -> UChar.t -> char
  end
