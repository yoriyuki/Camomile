(* $Id: uCharTbl.ml,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

type 'a tbl = 'a Tbl31.tbl
type 'a t = 'a tbl

let get tbl u = Tbl31.get tbl (UChar.uint_code u)

module type Type = sig
  type elt
  type t = elt tbl
  val get : elt tbl -> UChar.t -> elt
  val of_map : elt -> elt UMap.t -> t
end

module Make (H : Hashtbl.HashedType) = struct
  module T31 = Tbl31.Make (H)

  type elt = T31.elt
  type t = H.t tbl
  let get = get
  let of_map v m = T31.of_map v (UMap.imap_of_umap m)
end

module Bool = struct
  type t = Tbl31.Bool.t
  let get tbl u = Tbl31.Bool.get tbl (UChar.uint_code u)
  let of_set s = Tbl31.Bool.of_set (USet.iset_of_uset s)
end

module Bits = struct
  type t = Tbl31.Bits.t
  let get tbl u = Tbl31.Bits.get tbl (UChar.uint_code u)
  let of_map v m = Tbl31.Bits.of_map v (UMap.imap_of_umap m)
end

module Bytes = struct
  type t = Tbl31.Bytes.t
  let get tbl u = Tbl31.Bytes.get tbl (UChar.uint_code u)
  let of_map v m = Tbl31.Bytes.of_map v (UMap.imap_of_umap m)
end

module Char = struct
  type t = Tbl31.Char.t
  let get tbl u = Tbl31.Char.get tbl (UChar.uint_code u)
  let of_map v m = Tbl31.Char.of_map v (UMap.imap_of_umap m)
end
