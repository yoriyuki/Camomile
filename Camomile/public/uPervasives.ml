(** Functions for toplevel *)

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

type uchar = UChar.t

let int_of_uchar u = UChar.uint_code u
let uchar_of_int n = UChar.chr_of_uint n

let sprint_uchar u =
  let n = UChar.uint_code u in
  let n2 = n land 0xffff in
  let n1 = n lsr 16 in
  if n1 = 0 then 
    Printf.sprintf "\\u%04X" n2
  else
    Printf.sprintf "\\U%04X%04X" n1 n2

let escaped_uchar u =
  let n = int_of_uchar u in
  if n > 0x7f || n < 0 then
    sprint_uchar u
  else
    Char.escaped (Char.chr n)

let backslash = Char.code '\\'

let escaped_utf8 s =
  let buf = Buffer.create 0 in
  let proc u =
    let n = int_of_uchar u in
    if n > 0x7f || n < 0 then
      Buffer.add_string buf (sprint_uchar u)
    else
      Buffer.add_string buf (String.escaped (String.make 1 (Char.chr n)))
  in
  UTF8.iter proc s;
  Buffer.contents buf

let printer_utf8 f s =
  let b = UTF8.Buf.create 0 in
  UTF8.iter
    (fun u ->
       if UChar.uint_code u = 92  then
         UTF8.Buf.add_string b "\\\\"
       else if UChar.uint_code u < 0x80 then UTF8.Buf.add_char b u
       else
         let s = sprint_uchar u in
         UTF8.Buf.add_string b s)
    s;
  let s = UTF8.Buf.contents b in
  Format.fprintf f "\"%s\"" s

let printer_uchar f u =
  Format.fprintf f "'%s'"
    (if UChar.uint_code u = backslash then "\\\\"
     else if UChar.uint_code u < 0x80 then 
       UTF8.init 1 (fun _ -> u)
     else
       sprint_uchar u)
