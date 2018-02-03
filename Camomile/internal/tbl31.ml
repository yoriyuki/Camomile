(** Tbl31 : fast table keyed by integers *)
(* Copyright (C) 2002, 2003 Yamagata Yoriyuki *)

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


(* CRC-hash, algorithm comes from addnode.c/pathalias *)
(* 31-bits CRC-polynomial, by Andrew Appel*)
let poly = 0x48000000

let crc_tbl = Array.init 128 (fun i ->
    let rec loop j sum =
      if j < 0 then sum else
      if i land (1 lsl j) <> 0 then
        loop (j - 1) (sum lxor (poly lsr j))
      else
        loop (j - 1) sum in
    loop (7 - 1) 0)

let byte3 n = n lsr 24 land 127
let byte2 n = n lsr 16 land 255
let byte1 n = n lsr 8 land 255
let byte0 n = n land 255

let (lsl) x n =
  if n >= Sys.word_size then 0 else
  if n <= ~- Sys.word_size then 0 else
  if n < 0 then x lsr (~-n) else
    x lsl n

type 'a tbl = 'a array array array array
type 'a t = 'a tbl

type 'a tagged = Tag of 'a * int

let untag (Tag (a, _)) = a
let id (Tag (_, n)) = n

let get tbl n =
  let lev = Array.unsafe_get tbl (byte3 n) in
  let lev = Array.unsafe_get lev (byte2 n) in
  let lev = Array.unsafe_get lev (byte1 n) in
  Array.unsafe_get lev (byte0 n)

(* let get tbl n =
   Printf.printf "level 3 %d" (Array.length tbl); print_newline ();
   let lev = tbl.(byte3 n) in
   Printf.printf "level 2 %d" (Array.length tbl); print_newline ();
   let lev = lev.(byte2 n) in
   Printf.printf "level 1 %d" (Array.length tbl); print_newline ();
   let lev = lev.(byte1 n) in
   Printf.printf "level 0 %d" (Array.length tbl); print_newline ();
   lev.(byte0 n) *)

module type NodeType = sig 
  type elt
  type t
  val level : int
  val make : elt -> t tagged
  val of_map : int -> elt -> elt IMap.t -> t tagged
  val of_set : int -> elt -> ISet.t -> elt -> t tagged
end

module MakeNode (Sub : NodeType) = struct
  type elt = Sub.elt
  type node = Sub.t array
  type t = node

  let level = Sub.level + 1

  module NodeHash = struct
    type t = node tagged

    let equal x y =
      let a = untag x in
      let b = untag y in
      let rec loop i =
        if i < 0 then true else
        if a.(i) == b.(i) then loop (i - 1) else
          false in
      loop  (if level = 3 then 127 else 255)

    let hash = id
  end

  module NodePool = Weak.Make (NodeHash)
  let pool = NodePool.create 256

  let crc_hash v =
    let rec loop i sum =
      if i < 0 then sum else
        let a = id v.(i) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte3 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte2 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte1 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte0 a) land 0x7f) in
        loop (i - 1) sum in
    loop (if level = 3 then 127 else 255) 0

  let hashcons a =
    let n = crc_hash a in
    let b = Array.map untag a in
    (*    prerr_int (Array.length b); prerr_newline(); *)
    let x = Tag (b, n) in
    try NodePool.find pool x with Not_found ->
      NodePool.add pool x;
      x

  let make_raw def = 
    Array.make (if level = 3 then 128 else 256) (Sub.make def)

  let make def = hashcons (make_raw def)

  let of_map n0 def m =
    let a = make_raw def in begin
      if IMap.is_empty m then () else
        let l = AvlTree.left_branch m in
        let r = AvlTree.right_branch m in
        if IMap.is_empty l && IMap.is_empty r then
          let k1, k2, v = AvlTree.root m in
          let i1 = (k1 - n0) lsr (8 * level) in
          let n1 = n0 lor (i1 lsl (8 * level)) in
          let n2 = n1 lor (1 lsl (8 * level) - 1) in
          a.(i1) <- Sub.of_map n1 def (IMap.until n2 (IMap.from n1 m));
          let i2 = (k2 - n0) lsr (8 * level) in
          if i1 <> i2 then
            let n1 = n0 lor (i2 lsl (8 * level)) in
            let n2 = n1 lor (1 lsl (8 * level) - 1) in
            a.(i2) <- Sub.of_map n1 def (IMap.until n2 (IMap.from n1 m));
            let b = Sub.make v in
            for i = i1 + 1 to i2 - 1 do a.(i) <- b done;
          else ()
        else
          for i = 0 to if level = 3 then 127 else 255 do
            let n1 = n0 lor (i lsl (8 * level)) in
            let n2 = n1 lor (1 lsl (8 * level) - 1) in
            let m' = IMap.until n2 (IMap.from n1 m) in
            if IMap.is_empty m' then () else
              a.(i) <- Sub.of_map n1 def m'
          done
    end;
    hashcons a

  let of_set n0 def s v =
    let a = make_raw def in
    for i = 0 to if level = 3 then 127 else 255 do
      let n1 = n0 lor (i lsl (8 * level)) in
      let n2 = n1 lor (1 lsl (8 * level) - 1) in
      let s' = ISet.until n2 (ISet.from n1 s) in
      if ISet.is_empty s' then () else
        a.(i) <- Sub.of_set n1 def s' v
    done;
    hashcons a
end

module MakeTbl (Lev0 : NodeType) = struct
  module Lev1 = MakeNode (Lev0)
  module Lev2 = MakeNode (Lev1)
  module Lev3 = MakeNode (Lev2)
  include Lev3

  let get = get

  let of_map def m = untag (Lev3.of_map 0 def m)
end

module ArrayLeaf (H : Hashtbl.HashedType) = struct
  type elt = H.t
  type t = elt array
  type node = t
  let level = 0

  module NodeHash = struct
    type t = node tagged

    let equal x y =
      let a = untag x in
      let b = untag y in
      let rec loop i =
        if i >= 255 then true else
        if H.equal a.(i) b.(i) then loop (i + 1) else
          false in
      loop 0

    let hash = id
  end

  module Pool = Weak.Make (NodeHash)

  let pool = Pool.create 256

  let crc_hash v =
    let rec loop i sum =
      if i < 0 then sum else
        let a = H.hash v.(i) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte3 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte2 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte1 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte0 a) land 0x7f) in
        loop (i - 1) sum in
    loop 255 0

  let hashcons a =
    let n = crc_hash a in
    let x = Tag (a, n) in
    try Pool.find pool x with Not_found -> 
      Pool.add pool x;
      x

  let make_raw def = Array.make 256 def
  let make def = hashcons (make_raw def)

  let of_map n0 def m =
    let a = make_raw def in
    IMap.iter_range (fun n1 n2 v ->
        (*       Printf.eprintf "Tl31.ArrayLeaf.of_map : %x %x - %x: %s\n" n0 n1 n2 *)
        (* 	(String.escaped (Obj.magic v)); *)
        for i = n1 - n0 to n2 - n0 do a.(i) <- v done)
      m;
    hashcons a

  let of_set n0 def s v =
    let a = make_raw def in
    ISet.iter_range (fun n1 n2 ->
        for i = n1 - n0 to n2 - n0 do a.(i) <- v done)
      s;
    hashcons a
end

module type Type = sig
  type elt
  type t = elt tbl
  val get : elt tbl -> int -> elt
  val of_map : elt -> elt IMap.t -> elt tbl
end

module Make (H : Hashtbl.HashedType) = MakeTbl(ArrayLeaf(H))

module StringContentsHash = struct
  type t = Bytes.t tagged

  let equal x1 x2 =
    let s1 = untag x1 in
    let s2 = untag x2 in
    if Bytes.length s1 <> Bytes.length s2 then false else
      let rec loop i =
        if i < 0 then true else
        if Bytes.get s1 i <> Bytes.get s2 i then false else
          loop (i - 1) in
      loop (Bytes.length s1 - 1)

  let hash = id

end

let bytes_hash v =
  let rec loop i sum =
    if i < 0 then sum else
      let a = Char.code (Bytes.get v i) in
      let sum = sum lsr 7 lxor crc_tbl.(sum lxor a land 0x7f) in
      loop (i - 1) sum in
  loop (Bytes.length v - 5) 0

module BoolLeaf = struct
  type elt = bool
  type t = Bytes.t
  let level = 0

  module Pool = Weak.Make (StringContentsHash)
  let pool = Pool.create 256

  let hashcons s = 
    let n = bytes_hash s in
    let x = Tag (s, n) in
    try Pool.find pool x with Not_found -> 
      Pool.add pool x;
      x

  let make_raw def = Bytes.make 32 (if def then '\255' else '\000')

  let make def = hashcons (make_raw def)

  let boolset s k b =
    let j = Char.code (Bytes.get s (k / 8)) in
    let j' = if b then j lor (1 lsl (k mod 8)) else j in 
    Bytes.set s (k / 8) (Char.chr j')

  let of_map n0 def m =
    let a = make_raw def in
    IMap.iter_range (fun n1 n2 v ->
        for i = n1 - n0 to n2 - n0 do boolset a i v done)
      m;
    hashcons a

  let of_set n0 def s v =
    let a = make_raw def in
    ISet.iter_range (fun n1 n2 ->
        for i = n1 - n0 to n2 - n0 do boolset a i v done)
      s;
    hashcons a
end

module Bool = struct
  module BoolTbl = MakeTbl (BoolLeaf)
  include BoolTbl

  let of_set s = untag (BoolTbl.of_set 0 false s true)

  let get tbl n =
    let lev = Array.unsafe_get tbl (byte3 n) in
    let lev = Array.unsafe_get lev (byte2 n) in
    let lev = Array.unsafe_get lev (byte1 n) in
    let k = byte0 n in
    let i = Char.code (Bytes.unsafe_get lev (k / 8)) in
    i lsr (k mod 8) land 1 <> 0
end

module CharLeaf = struct
  type elt = char
  type t = Bytes.t
  let level = 0

  module Pool = Weak.Make (StringContentsHash)
  let pool = Pool.create 256

  let hashcons s = 
    let n = bytes_hash s in
    let x = Tag (s, n) in
    try Pool.find pool x with Not_found -> 
      Pool.add pool x;
      x

  let make_raw c = Bytes.make 256 c
  let make c = hashcons (make_raw c)

  let of_map n0 def m =
    let a = make_raw def in
    IMap.iter_range (fun n1 n2 v ->
        for i = n1 - n0 to n2 - n0 do Bytes.set a i v done)
      m;
    hashcons a

  let of_set n0 def s v =
    let a = make_raw def in
    ISet.iter_range (fun n1 n2 ->
        for i = n1 - n0 to n2 - n0 do Bytes.set a i v done)
      s;
    hashcons a
end

module Char = struct
  module CharTbl = MakeTbl (CharLeaf)
  include CharTbl

  let get tbl n =
    let lev = Array.unsafe_get tbl (byte3 n) in
    let lev = Array.unsafe_get lev (byte2 n) in
    let lev = Array.unsafe_get lev (byte1 n) in
    Bytes.unsafe_get lev (byte0 n)
end

module BitsContentsHash = struct
  type t = Bitsvect.t tagged

  let equal x1 x2 = 
    let a1 = untag x1 in
    let a2 = untag x2 in
    let rec loop i =
      if i < 0 then true else
      if Bitsvect.get a1 i = Bitsvect.get a2 i then loop (i - 1) else false in
    loop 255

  let hash = id 
end

module BitsLeaf = struct
  type elt = int
  type t = Bitsvect.t
  let level = 0

  module Pool = Weak.Make (BitsContentsHash)
  let pool = Pool.create 256

  let hash v =
    let rec loop i sum =
      if i < 0 then sum else
        let a = Bitsvect.get v i in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor a land 0x7f) in
        loop (i - 1) sum in
    loop (Bitsvect.length v - 5) 0

  let hashcons a = 
    let n = hash a in
    let x = Tag (a, n) in
    try Pool.find pool x with Not_found -> 
      Pool.add pool x;
      x

  let make_raw = Bitsvect.make 256
  let make def = hashcons (make_raw def)

  let of_map n0 def m =
    let a = make_raw def in
    IMap.iter_range (fun n1 n2 v ->
        for i = n1 - n0 to n2 - n0 do Bitsvect.set a i v done)
      m;
    hashcons a

  let of_set n0 def s v =
    let a = make_raw def in
    ISet.iter_range (fun n1 n2 ->
        for i = n1 - n0 to n2 - n0 do Bitsvect.set a i v done)
      s;
    hashcons a
end

module Bits = struct
  include MakeTbl (BitsLeaf)
  let get tbl n =
    let lev = Array.unsafe_get tbl (byte3 n) in
    let lev = Array.unsafe_get lev (byte2 n) in
    let lev = Array.unsafe_get lev (byte1 n) in
    Bitsvect.unsafe_get lev (byte0 n)
end

module BytesContentsHash = struct
  type t = Bytesvect.t tagged

  let equal x1 x2 = 
    let a1 = untag x1 in
    let a2 = untag x2 in
    let rec loop i =
      if i < 0 then true else
      if Bytesvect.get a1 i = Bytesvect.get a2 i then 
        loop (i - 1) 
      else false in
    loop 255

  let hash = id
end

module BytesLeaf = struct
  type elt = int
  type t = Bytesvect.t
  let level = 0

  module Pool = Weak.Make (BytesContentsHash)
  let pool = Pool.create 256

  let hash v =
    let rec loop i sum =
      if i < 0 then sum else
        let a = Bytesvect.get v i in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte3 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte2 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte1 a) land 0x7f) in
        let sum = sum lsr 7 lxor crc_tbl.(sum lxor (byte0 a) land 0x7f) in
        loop (i - 1) sum in
    loop 255 0

  let hashcons a = 
    let n = hash a in
    let x = Tag (a, n) in
    try Pool.find pool x with Not_found -> 
      Pool.add pool x;
      x

  let make_raw = Bytesvect.make 256
  let make def = hashcons (make_raw def)

  let of_map n0 def m =
    let a = make_raw def in
    IMap.iter_range (fun n1 n2 v ->
        for i = n1 - n0 to n2 - n0 do Bytesvect.set a i v done)
      m;
    hashcons a

  let of_set n0 def s v =
    let a = make_raw def in
    ISet.iter_range (fun n1 n2 ->
        for i = n1 - n0 to n2 - n0 do Bytesvect.set a i v done)
      s;
    hashcons a
end

module Bytes = struct
  include MakeTbl (BytesLeaf)
  let get tbl n =
    let lev = Array.unsafe_get tbl (byte3 n) in
    let lev = Array.unsafe_get lev (byte2 n) in
    let lev = Array.unsafe_get lev (byte1 n) in
    Bytesvect.unsafe_get lev (byte0 n)
end
