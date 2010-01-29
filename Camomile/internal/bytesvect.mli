(* $Id: bytesvect.mli,v 1.5 2003/04/27 17:47:59 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki *)

type t
val get : t -> int -> int
val unsafe_get : t -> int -> int
val set : t -> int -> int -> unit
val make : int -> int -> t
val copy : t -> t
val iteri : (int -> int -> unit) -> t -> unit
val length : t -> int

val set_id : t -> int -> unit
val id : t -> int
