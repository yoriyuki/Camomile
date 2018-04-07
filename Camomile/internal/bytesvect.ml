(** Vectors inplemented bytes *)
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
(* yoriyuki.y@gmail.com *)

type t = 
  {len : int; 
   mutable bytes : int; 
   mutable contents : Bytes.t;
   mutable id : int}

(* get b v i : read b-bytes from the k-th byte of v *)
(* b <= 4 *) 
let rec get_raw acc b v k =
  if b = 0 then acc else
    let acc' = (acc lsl 8) lor (Char.code (Bytes.get v k)) in
    get_raw acc' (pred b) v (succ k)

let get v i = get_raw 0 v.bytes v.contents (i * v.bytes)

let rec unsafe_get_raw acc b v k =
  if b = 0 then acc else
    let acc' = (acc lsl 8) lor (Char.code (Bytes.unsafe_get v k)) in
    unsafe_get_raw acc' (pred b) v (succ k)

let unsafe_get v i = unsafe_get_raw 0 v.bytes v.contents (i * v.bytes)

let rec set_bytes_raw b v k n =
  let c = (n lsr ((b - 1) lsl 3)) land 255 in
  Bytes.set v k (Char.chr c);
  if b > 1 then set_bytes_raw (b - 1) v (k + 1) n

let rec bytes n = if n = 0 then 0 else 1 + bytes (n lsr 8)

let set v i n =
  let b = bytes n in
  if v.bytes < b then
    let save = {len = v.len; bytes = v.bytes; contents = v.contents; id = 0} in
    let len = (Bytes.length v.contents) / v.bytes in
    v.contents <- Bytes.make (len * b) (Char.chr 0);
    v.bytes <- b;
    for i = 0 to len - 1 do 
      set_bytes_raw b v.contents (b * i) (get save i)
    done
  else ();
  set_bytes_raw v.bytes v.contents (i * v.bytes) n

let make i0 df =
  let b = max (bytes df) 1 in
  let v = 
    {len = i0; 
     bytes = b; 
     contents = Bytes.make (b * i0) (Char.chr 0);
     id = 0} 
  in
  for i = 0 to i0 - 1 do set v i df done; v

let copy v =
  {len = v.len;
   bytes = v.bytes;
   contents = Bytes.copy v.contents;
   id = 0}

let iteri proc v =
  let len = (Bytes.length v.contents) / v.bytes in
  for i = 0 to len - 1 do
    proc i (get v i)
  done

let length v = v.len

let set_id v id = v.id <- id
let id v = v.id
