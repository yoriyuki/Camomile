(** mappings from integer to arbitrary types *)
(* Copyright (C) 2003 Yamagata Yoriyuki. distributed with LGPL *)

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

let compare_uint n1 n2 =
  let sgn1 = (n1 lsr 24) - (n2 lsr 24) in
  if sgn1 = 0 then (n1 land 0xffffff) - (n2 land 0xffffff) else sgn1

let ( > ) n1 n2 = compare_uint n1 n2 > 0
let ( < ) n1 n2 = compare_uint n1 n2 < 0
let ( <= ) n1 n2 = compare_uint n1 n2 <= 0
let max_int = ~-1
let min_int = 0

type 'a t = (int * int * 'a) AvlTree.tree
type key = int

include AvlTree

let make ?(eq = ( = )) l (n1, n2, v) r =
  let n1, l =
    if is_empty l || n1 = min_int then (n1, empty)
    else (
      let (k1, k2, v0), l' = split_rightmost l in
      if k2 + 1 = n1 && eq v v0 then (k1, l') else (n1, l))
  in
  let n2, r =
    if is_empty r || n2 = max_int then (n2, empty)
    else (
      let (k1, k2, v0), r' = split_leftmost r in
      if n2 + 1 = k1 && eq v v0 then (k2, r') else (n2, r))
  in
  make_tree l (n1, n2, v) r

let rec from n s =
  if is_empty s then empty
  else (
    let ((n1, n2, v) as x) = root s in
    let s0 = left_branch s in
    let s1 = right_branch s in
    if n < n1 then make_tree (from n s0) x s1
    else if n > n2 then from n s1
    else make_tree empty (n, n2, v) s1)

let after n s = if n = max_int then empty else from (n + 1) s

let rec until n s =
  if is_empty s then empty
  else (
    let ((n1, n2, v) as x) = root s in
    let s0 = left_branch s in
    let s1 = right_branch s in
    if n > n2 then make_tree s0 x (until n s1)
    else if n < n1 then until n s0
    else make_tree s0 (n1, n, v) empty)

let before n s = if n = min_int then empty else until (n - 1) s

let add_range ?eq n1 n2 v s =
  if n1 > n2 then invalid_arg "IMap.add_range"
  else make ?eq (before n1 s) (n1, n2, v) (after n2 s)

let add ?eq n v s = add_range ?eq n n v s

let rec find n m =
  if is_empty m then raise Not_found
  else (
    let n1, n2, v = root m in
    if n < n1 then find n (left_branch m)
    else if n1 <= n && n <= n2 then v
    else find n (right_branch m))

let remove_range n1 n2 m =
  if n1 > n2 then invalid_arg "IMap.remove_range"
  else concat (before n1 m) (after n2 m)

let remove n m = remove_range n n m

let rec mem n m =
  if is_empty m then false
  else (
    let n1, n2, _ = root m in
    if n < n1 then mem n (left_branch m)
    else if n1 <= n && n <= n2 then true
    else mem n (right_branch m))

let iter_range proc m = AvlTree.iter (fun (n1, n2, v) -> proc n1 n2 v) m
let fold_range f m a = AvlTree.fold (fun (n1, n2, v) a -> f n1 n2 v a) m a

let fold f m a =
  let rec loop n1 n2 v a =
    let a = f n1 v a in
    if n1 = n2 then a else loop (n1 + 1) n2 v a
  in
  fold_range loop m a

let iter proc m = fold (fun n v () -> proc n v) m ()

let rec map ?eq f m =
  if is_empty m then empty
  else (
    let n1, n2, v = root m in
    let l = map f (left_branch m) in
    let r = map f (right_branch m) in
    let v = f v in
    make ?eq l (n1, n2, v) r)

let mapi ?eq f m = fold (fun n v a -> add ?eq n (f n v) a) m empty

let rec set_to_map s v =
  if is_empty s then empty
  else (
    let n1, n2 = root s in
    let l = left_branch s in
    let r = right_branch s in
    make_tree (set_to_map l v) (n1, n2, v) (set_to_map r v))

let domain m =
  let f n1 n2 _ s = ISet.add_range n1 n2 s in
  fold_range f m ISet.empty

let map_to_set p m =
  let f n1 n2 v s = if p v then ISet.add_range n1 n2 s else s in
  fold_range f m ISet.empty
