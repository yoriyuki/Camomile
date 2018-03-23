(** Sub-texts, parts of original (ur-) texts.
    The signature and semantics matches those of UStorage. *)
(* Copyright (C) 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

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


module type Type = sig
  type t

  val get : t -> int -> UChar.t

  val init : int -> (int -> UChar.t) -> t
  val length : t -> int

  type index
  val look : t -> index -> UChar.t
  val nth : t -> int -> index
  val first : t -> index
  val last : t -> index

  val next : t -> index -> index
  val prev : t -> index -> index
  val move : t -> index -> int -> index
  val out_of_range : t -> index -> bool
  val compare_index : t -> index -> index -> int

  val iter : (UChar.t -> unit) -> t -> unit
  val compare : t -> t -> int

  module Buf : sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end

  type ur_text
  type ur_index

  val refer : ur_text -> ur_index -> ur_index -> t
  val excerpt : t -> ur_text
  val context : t -> ur_text * ur_index * ur_index
  val ur_index_of : t -> index -> ur_index
end

module Make (Text : UnicodeString.Type) = struct

  type t = Text.t * Text.index * Text.index
  type index = Text.index

  let out_of_range (t, i0, j) i =
    if Text.compare_index t i0 i > 0 then true else
    if Text.compare_index t i j >= 0 then true else
      Text.out_of_range t i

  let look ((t, _, _) as s) i =
    if out_of_range s i then failwith "SubText.look" else
      Text.look t i

  let next (t, _, _) i = Text.next t i

  let prev (t, _, _) i = Text.prev t i

  let move (t, _, _) i n = Text.move t i n

  let nth ((_, i, _) as s) n = move s i n

  let first (_, i, _) = i

  let last (t, _, i) = Text.prev t i

  let compare_index (t, _, _) i j = Text.compare_index t i j

  let get s n = look s (nth s n)

  let init len f =
    let t = Text.init len f in
    (t, Text.nth t 0, Text.next t (Text.last t))

  let length (t, i, j) =
    let rec loop i n =
      if Text.compare_index t i j >= 0 then n else
        loop (Text.next t i) (n + 1) in
    loop i 0

  let iter proc (t, i, j) =
    let rec loop i =
      if Text.compare_index t i j >= 0 then () else begin
        proc (Text.look t i);
        loop (Text.next t i)
      end in
    loop i

  let compare (t1, i1, j1) (t2, i2, j2) =
    let rec loop i1 i2 =
      if Text.compare_index t1 i1 j1 >= 0 then
        if Text.compare_index t2 i2 j2 >= 0 then 0 else ~-1
      else if Text.compare_index t2 i2 j2 >= 0 then 1 else
        let sgn = UChar.compare (Text.look t1 i1) (Text.look t2 i2) in
        if sgn = 0 then
          loop (Text.next t1  i1) (Text.next t2 i2)
        else sgn in
    loop i1 i2

  module Buf = struct
    include Text.Buf

    let add_string buf s = iter (add_char buf) s

    let contents buf =
      let t = Text.Buf.contents buf in
      (t, Text.nth t 0, Text.next t (Text.last t))
  end

  type ur_text = Text.t
  type ur_index = Text.index

  let refer t i j = (t, i, j)

  let excerpt s =
    let buf = Buf.create 0 in
    Buf.add_string buf s;
    Text.Buf.contents buf

  let context s = s

  let ur_index_of _ i = i
end
