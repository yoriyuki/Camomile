(** An implementation of Unicode string. *)

(* Copyright (C) 2002, 2003 Yamagata Yoriyuki. *)

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

include Array
type mutability = [ `Mutable | `Immutable ]

type 'a text = UChar.t array
type utext = [`Imutable] text
type t = utext
type ustring = [`Mutable] text

let utext_of_ustring = Array.copy
let ustring_of_utext = Array.copy

type index = int

let look s i = get s i
let nth _ i = i
let first _ = 0
let last s = Array.length s - 1
let out_of_range s i = i < 0 || i >= Array.length s
let next _ i = i + 1
let prev _ i = i - 1
let move _ i n = i + n
let compare_index _ (i : int) (j : int) = i - j

let make len init = Array.make len init

let init_ustring = init

let of_string s = init (String.length s) (fun i -> UChar.of_char s.[i])

let rec compare_aux i t1 t2 =
  if i >= length t1 then
    if i >= length t2 then 0 else ~-1
  else if i >= length t2 then 1 else
    match UChar.compare (get t1 i) (get t2 i) with
      0 -> compare_aux (i + 1) t1 t2
    | sgn -> sgn

let compare t1 t2 = compare_aux 0 t1 t2

module Buf =
struct
  include XArray

  type buf = UChar.t xarray

  let create bufsize = XArray.make ~bufsize 0 (UChar.chr_of_uint 0)
  let contents = array_of
  let contents_string = array_of
  let add_char = add_element
  let add_string = add_array
  let add_buffer = add_xarray
end
