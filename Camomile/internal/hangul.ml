(** Hangul *)
(* Copyright (C) 2003 Yamagata Yoriyuki. *)

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


let sbase = 0xac00
let lbase = 0x1100
let vbase = 0x1161
let tbase = 0x11a7
let lcount = 19
let vcount = 21
let tcount = 28
let ncount = vcount * tcount
let scount = lcount * ncount

let decompose u =
  let n = UChar.uint_code u - sbase in
  let l = lbase + n / ncount in
  let v = vbase + (n mod ncount) / tcount in
  let t = tbase + n mod tcount in
  if t = tbase then
    [UChar.chr_of_uint l; UChar.chr_of_uint v]
  else
    [UChar.chr_of_uint l; UChar.chr_of_uint v; UChar.chr_of_uint t]

let add_decomposition x u =
  let n = UChar.uint_code u - sbase in
  if n < 0 || n >= scount then XString.add_char x u else begin
    XString.add_char x (UChar.chr_of_uint (lbase + n / ncount));
    XString.add_char x (UChar.chr_of_uint (vbase + (n mod ncount) / tcount));
    let t = tbase + n mod tcount in
    if t = tbase then () else
      XString.add_char x (UChar.chr_of_uint t)
  end

let compose x' x =
  if XString.length x = 0 then () else
    let pos = ref 0 in
    let last = ref (UChar.uint_code (XString.get x 0)) in
    for i = 1 to XString.length x - 1 do
      let n = UChar.uint_code (XString.get x i) in
      let l = !last - lbase in
      let v = n - vbase in
      if 0 <= l && l < lcount	&& 0 <= v && v < vcount then
        last := sbase + (l * vcount + v) * tcount
      else
        let s = !last - sbase in
        let t = n - tbase in
        if 
          0 <= s && s < scount && s mod tcount = 0 && 
          0 <= t && t < tcount
        then
          last := !last + t
        else begin
          XString.set x' !pos (UChar.chr_of_uint !last);
          last := n;
          incr pos
        end
    done;
    XString.set x' !pos (UChar.chr_of_uint !last);
    XString.shrink x' (!pos + 1)

