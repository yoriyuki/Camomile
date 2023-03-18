(** Sets of Unicode characters, implemented as sets of intervals.
    The signature is mostly same to Set.S in stdlib *)

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

include ISet

let mem u s = ISet.mem (UChar.uint_code u) s
let add u s = ISet.add (UChar.uint_code u) s

let add_range u1 u2 s =
  ISet.add_range (UChar.uint_code u1) (UChar.uint_code u2) s

let singleton u = ISet.singleton (UChar.uint_code u)
let remove u s = ISet.remove (UChar.uint_code u) s

let remove_range u1 u2 s =
  ISet.remove_range (UChar.uint_code u1) (UChar.uint_code u2) s

let from u s = ISet.from (UChar.uint_code u) s
let after u s = ISet.after (UChar.uint_code u) s
let until u s = ISet.until (UChar.uint_code u) s
let before u s = ISet.before (UChar.uint_code u) s
let iter f s = ISet.iter (fun n -> f (UChar.chr_of_uint n)) s

let iter_range f s =
  let f' n1 n2 = f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) in
  ISet.iter_range f' s

let fold f s a =
  let f' n a = f (UChar.chr_of_uint n) a in
  ISet.fold f' s a

let fold_range f s a =
  let f' n1 n2 a = f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) a in
  ISet.fold_range f' s a

let for_all p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.for_all p' s

let exists p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.exists p' s

let filter p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.filter p' s

let partition p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.partition p' s

let elements s = List.map UChar.chr_of_uint (ISet.elements s)

let ranges s =
  let f (n1, n2) = (UChar.chr_of_uint n1, UChar.chr_of_uint n2) in
  List.map f (ISet.ranges s)

let min_elt s = UChar.chr_of_uint (ISet.min_elt s)
let max_elt s = UChar.chr_of_uint (ISet.max_elt s)
let choose s = UChar.chr_of_uint (ISet.choose s)
let uset_of_iset s = s
let iset_of_uset s = s
