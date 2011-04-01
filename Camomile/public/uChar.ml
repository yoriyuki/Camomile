(** Unicode (ISO-UCS) characters.

   This module implements Unicode (actually ISO-UCS) characters.  All
   31-bit code points are allowed.
*)

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


type t = int

exception Out_of_range

external unsafe_chr_of_uint : int -> t = "%identity"
external uint_code : t -> int = "%identity"

let char_of c = 
  if c >= 0 && c < 0x100 then Char.chr c else raise Out_of_range

let of_char = Char.code

let code c = if c >= 0 then c else raise Out_of_range

let chr n =
  if n >= 0 && n lsr 31 = 0 then n else invalid_arg "UChar.chr"

let chr_of_uint n = 
  if n lsr 31 = 0 then n else 
  invalid_arg "UChar.char_of_uint"
  
let eq (u1 : t) (u2 : t) = u1 = u2
let compare u1 u2 =
  let sgn = (u1 lsr 16) - (u2 lsr 16) in
  if sgn = 0 then (u1 land 0xFFFF) -  (u2 land 0xFFFF) else sgn

type uchar = t

let int_of u = uint_code u
let of_int n = chr_of_uint n
