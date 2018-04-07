(** eXtensible Unicode string.  
    The semantics matches the description of UStorage. 
    The detail may be going to change.*)

(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

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

module type XStringSig = sig
  type xstring = UChar.t XArray.t

  val get : xstring -> int -> UChar.t
  val set : xstring -> int -> UChar.t -> unit
  val length : xstring -> int

  type index
  val look : xstring -> index -> UChar.t
  val nth : xstring -> int -> index
  val first : xstring -> index
  val last : xstring -> index
  val out_of_range : xstring -> index -> bool
  val next : xstring -> index -> index
  val prev : xstring -> index -> index
  val move : xstring -> index -> int -> index
  val compare_index : xstring -> index -> index -> int

  val make : ?bufsize:int -> int -> UChar.t -> xstring
  val clear : xstring -> unit
  val reset : xstring -> unit
  val copy : xstring -> xstring
  val sub : xstring -> int -> int -> xstring
  val add_char : xstring -> UChar.t -> unit
  val add_text : xstring -> 'a UText.text -> unit
  val add_xstring : xstring -> xstring -> unit
  val shrink : xstring -> int -> unit
  val append : xstring -> xstring -> xstring
  val utext_of : xstring -> UText.t
  val ustring_of : xstring -> UText.ustring

  val iter : (UChar.t -> unit) -> xstring -> unit
  val compare : xstring -> xstring -> int
end

module XStringAux : XStringSig = struct
  include XArray
  type xstring = UChar.t XArray.t

  let rec compare_aux i t1 t2 =
    if i >= length t1 then
      if i >= length t2 then 0 else ~-1
    else if i >= length t2 then 1 else
      match UChar.compare (XArray.get t1 i) (XArray.get t2 i) with
        0 -> compare_aux (i + 1) t1 t2
      | sgn -> sgn

  let compare t1 t2 = compare_aux 0 t1 t2
  let add_xstring = add_xarray
  let add_char = add_element
  let add_text b t = UText.iter (add_char b) t
  let ustring_of b = UText.init_ustring (length b) (get b)
  let utext_of b = UText.init (length b) (get b)
end

include XStringAux
type t = xstring

let init len f = XArray.init len (UChar.chr_of_uint 0) f

module Buf =
struct
  include XStringAux
  type buf = xstring
  let create bufsize = make ~bufsize 0 (UChar.chr_of_uint 0)
  let contents x = x
  let add_string = add_xstring
  let add_buffer = add_xstring
end
