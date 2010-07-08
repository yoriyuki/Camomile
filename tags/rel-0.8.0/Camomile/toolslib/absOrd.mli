(* $Id: absOrd.mli,v 1.3 2004/07/14 05:41:09 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

type point

module IntSet : sig type t = ISet.t end
module IntMap : Map.S with type key = int
module Map : Map.S with type key = point

type t
val compare : point -> point -> t -> int
val top : t -> point
val bottom : t -> point
val next : point -> t -> point
val prev : point -> t -> point
val add_top : t -> point * t
val add_bottom : t -> point * t
val add_before : point -> t -> point * t
val add_after : point -> t -> point * t
val iter : (point -> unit) -> t -> unit
val fold : (point -> 'a -> 'a) -> t -> 'a -> 'a
val import : int list -> t * point IntMap.t * int Map.t
