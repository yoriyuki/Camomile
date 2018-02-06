(** bits vector *)
(* Copyright (C) 2002 Yamagata Yoriyuki. *)

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


let rec bits n = if n = 0 then 0 else 1 + bits (n lsr 1)

type t = 
  {len : int; 
   mutable bits : int; 
   mutable contents : Bytes.t;
   mutable id : int}

(* get v i : read the i-th element of v *)
(* b < 8 *)
let get vect i =
  let b = vect.bits in
  let v = vect.contents in
  let k = b * i in
  let m0 = ((Char.code (Bytes.get v (k lsr 3))) lsr (k land 0b111)) in
  let m =
    ((Char.code (Bytes.get v ((k lsr 3) + 1))) lsl (8 - (k land 0b111))) lor m0
  in m land ((1 lsl b) - 1)

let unsafe_get vect i =
  let b = vect.bits in
  let v = vect.contents in
  let k = b * i in
  let j = k lsr 3 in
  let v1 = Char.code (Bytes.unsafe_get v j) in
  let v2 = Char.code (Bytes.unsafe_get v (j + 1)) in
  let j' = k land 0b111 in
  (v2 lsl (8 - j')) lor (v1 lsr j') land ((1 lsl b) - 1)

let set_raw vect i n =
  let b = vect.bits in
  let v = vect.contents in
  let i0 = (i * b) lsr 3 in
  let i1 = (i * b) land 7 in
  let masq1 = (1 lsl b - 1) in
  let c0 = (Char.code (Bytes.get v i0)) land (lnot (masq1 lsl i1)) in
  let c0' = c0 lor ((n lsl i1) land 255) in
  Bytes.set v i0 (Char.chr c0');
  if b + i1 <= 8 then () else
    let masq2 = (1 lsl (b + i1 - 8)) - 1 in
    let c1 = (Char.code (Bytes.get v (i0 + 1))) land (lnot masq2) in
    let c1' = c1 lor (n lsr (8 - i1)) in
    Bytes.set v (i0 + 1) (Char.chr c1')

let bits_to_bytes b = b / 8 + 2

let set vect i n =
  let b = bits n in
  if vect.bits < b then
    let save = 
      {len = vect.len; 
       bits = vect.bits; 
       contents = vect.contents;
       id = 0} 
    in
    let len = vect.len in
    vect.contents <- Bytes.make (bits_to_bytes (b * len)) (Char.chr 0);
    vect.bits <- b;
    for i = 0 to len - 1 do set_raw vect i (get save i) done;
  else ();
  set_raw vect i n

let make i0 df = 
  let b = max (bits df) 1 in
  let v = 
    {len = i0;
     bits = b; 
     contents = Bytes.make (bits_to_bytes (b * i0)) (Char.chr 0);
     id = 0} 
  in
  for i = 0 to i0 - 1 do set v i df done; v

let copy v =
  {len = v.len;
   bits = v.bits;
   contents = Bytes.copy v.contents;
   id = 0}

let iteri proc v =
  for i = 0 to v.len - 1 do
    proc i (get v i)
  done

let length v = v.len

let set_id v id = v.id <- id
let id v = v.id
