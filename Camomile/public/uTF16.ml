(** UTF-16 encoded string. *)

(* Copyright (C) 2002, 2003, 2004 Yamagata Yoriyuki. *)

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

open Bigarray

exception Out_of_range

(* UTF-16 encoded string. the type is bigarray of 16-bits integers. *)
type t = (int, int16_unsigned_elt, c_layout) Array1.t

type index = int

exception Malformed_code

let rec validate_aux (a:t) i =
  if i >= Array1.dim a then () else
  let n = a.{i} in
  if n < 0xd800 || n >= 0xe000 && n < 0xfffe then
    validate_aux a (i + 1)
  else if n >= 0xd800 && n < 0xdc00 then
    if i + 1 >= Array1.dim a then raise Malformed_code else
    let n' = a.{i + 1} in
    if n' < 0xdc00 || n' >= 0xe000 then raise Malformed_code else
    validate_aux a (i + 2)
  else raise Malformed_code

let validate (a:t) = validate_aux a 0

let look (a:t) i : UChar.t =
  let n0 = a.{i} in
  if n0 < 0xd800 || n0 >= 0xe000 then UChar.chr_of_uint n0 else
  if n0 < 0xdc00 then
    let n1 = a.{i + 1} in
    UChar.chr_of_uint 
      (((n0 - 0xd800) lsl 10) + (n1 - 0xdc00) + 0x10000)
  else invalid_arg "UTF16.look"

let rec length_aux (a:t) c i =
  if i >= Array1.dim a then c else
  let n = a.{i} in
  if n < 0xd800 || n >= 0xe000 then length_aux a (c + 1) (i + 1)
  else length_aux a (c + 1) (i + 2)

let length (a:t) = length_aux a 0 0

let next (a:t) i =
  let n = a.{i} in
  if n < 0xd800 || n >= 0xdc00 then i + 1 else
  i + 2

let prev (a:t) i =
  let i' = i - 1 in
  let n = a.{i'} in
  if n < 0xdc00 || n >= 0xe000 then i' else i' - 1

let rec move_forward (a:t) i c =
  if c > 0 then move_forward a (next a i) (c - 1) else i

let rec move_backward (a:t) i c =
  if c < 0 then move_backward a (prev a i) (c + 1) else i

let move (a:t) i c = 
  if c > 0 then move_forward a i c else
  if c < 0 then move_backward a i c else
  i

let first _ = 0

let last (a:t) = prev a (Array1.dim a)

let out_of_range (a:t) i = i < 0 || i >= Array1.dim a

let compare_index _ i j = i - j

let nth (a:t) c = move_forward a 0 c

let get (a:t) c = look a (nth a c)

let rec iter_aux proc (a:t) i =
  if i >= Array1.dim a then () else begin
    proc (look a i);
    iter_aux proc a (next a i)
  end

let iter proc (a:t) = iter_aux proc a 0

module Buf = struct

  let set (a:t) i u =
    let n = UChar.uint_code u in
    if n < 0 then raise Out_of_range else
    if n < 0xd800 || n >= 0xe000 && n <= 0xfffd then begin
      a.{i} <- n;
      1
    end else if n >= 0x10000 && n <= 0x10ffff then begin
      a.{i} <- ((n - 0x10000) lsr 10) + 0xd800;
      a.{i + 1} <- ((n - 0x10000) land 0x3ff) + 0xdc00;
      2
    end else
      raise Out_of_range

  type buf = {init_size : int; mutable pos : index; mutable contents : t}

  let create n =
    let contents = Array1.create int16_unsigned c_layout n in
    {init_size = n; pos = 0; contents = contents}

  let clear buf = buf.pos <- 0

  let reset buf =
    buf.contents <- Array1.create int16_unsigned c_layout buf.init_size;
    buf.pos <- 0

  let contents buf =
    let a = Array1.create int16_unsigned c_layout buf.pos in
    let src = Array1.sub buf.contents 0 buf.pos in
    Array1.blit src a;
    a

  let resize buf n =
    if Array1.dim buf.contents >= n then () else
    let a = Array1.create int16_unsigned c_layout (2 * n) in
    let a' = Array1.sub a 0 (Array1.dim buf.contents) in
    Array1.blit buf.contents a';
    buf.contents <- a

  let add_char buf u =
    resize buf (buf.pos + 2);
    buf.pos <- buf.pos + set buf.contents buf.pos u

  let add_string buf (a:t) =
    let len = buf.pos + Array1.dim a in
    resize buf len;
    let b = Array1.sub buf.contents buf.pos (Array1.dim a) in
    Array1.blit a b;
    buf.pos <- len

  let add_buffer buf1 buf2 =
    let len = buf1.pos + buf2.pos in
    resize buf1 len;
    let a = Array1.sub buf2.contents 0 buf2.pos in
    let b = Array1.sub buf1.contents buf1.pos buf2.pos in
    Array1.blit a b;
    buf1.pos <- len
end

let init len f =
  let buf = Buf.create (len + 1) in
  for i = 0 to len - 1 do Buf.add_char buf (f i) done;
  Buf.contents buf

let rec compare_aux (a:t) b i =
  if i >= Array1.dim a then 0 else
  let n1 = a.{i} in
  let n2 = b.{i} in
  if n1 = n2 then compare_aux a b (i + 1) else
  (if n1 < 0xd800 || n1 >= 0xdc00 then n1 else 0x10000 lor n1) -
    (if n2 < 0xd800 || n2 >= 0xdc00 then n2 else 0x10000 lor n2)

let compare (a:t) b =
  let sgn = Array1.dim a - Array1.dim b in
  if sgn = 0 then compare_aux a b 0 else sgn
