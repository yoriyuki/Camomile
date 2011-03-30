(** Maps over Unicode characters. *) 

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
(* yoriyuki.yamagata@aist.go.jp *)


include IMap

let umap_of_imap m = m
let imap_of_umap m = m

let add ?eq u v m = IMap.add ?eq (UChar.uint_code u) v m

let add_range ?eq u1 u2 v m =
  IMap.add_range ?eq (UChar.uint_code u1) (UChar.uint_code u2) v m

let find u m = IMap.find (UChar.uint_code u) m
let remove u m = IMap.remove (UChar.uint_code u) m

let remove_range u1 u2 m =
  IMap.remove_range (UChar.uint_code u1) (UChar.uint_code u2) m

let from u m = IMap.from (UChar.uint_code u) m
let after u m = IMap.after (UChar.uint_code u) m

let until u m = IMap.until (UChar.uint_code u) m
let before u m = IMap.before (UChar.uint_code u) m

let mem u m = IMap.mem (UChar.uint_code u) m

let iter f m =
  let f' n = f (UChar.chr_of_uint n) in
  IMap.iter f' m

let iter_range f m =
  let f' n1 n2 = 
    f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) in
  IMap.iter_range f' m

let fold f m a =
  let f' n v a = f (UChar.chr_of_uint n) v a in
  IMap.fold f' m a

let fold_range f m a =
  let f' n1 n2 v a = 
    f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) v a in
  IMap.fold_range f' m a

let mapi ?eq f m =
  let f' n v = f (UChar.chr_of_uint n) v in
  IMap.mapi ?eq f' m

let set_to_map s = IMap.set_to_map (USet.iset_of_uset s)

let domain m = USet.uset_of_iset (IMap.domain m)

let map_to_set p m = USet.uset_of_iset (IMap.map_to_set p m)
