(* $Id: tbl31.mli,v 1.6 2003/06/18 15:11:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

type 'a tbl
type 'a t = 'a tbl

val get : 'a tbl -> int -> 'a

module type Type = sig
  type elt
  type t = elt tbl
  val get : elt tbl -> int -> elt
  val of_map : elt -> elt IMap.t -> t
end

module Make : functor (H : Hashtbl.HashedType) -> (Type with type elt = H.t)

module Bool : sig
  type t
  val of_set : ISet.t -> t
  val get : t -> int -> bool
end

module Bits : sig
  type t
  val of_map : int -> int IMap.t -> t
  val get : t -> int -> int
end

module Bytes : sig
  type t
  val of_map : int -> int IMap.t -> t
  val get : t -> int -> int
end

module Char : sig
  type t
  val of_map : char -> char IMap.t -> t
  val get : t -> int -> char
end
