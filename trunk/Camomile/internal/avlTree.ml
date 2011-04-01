(** AVL tree *)
(* Copyright (C) 2003, 2010 Yamagata Yoriyuki. *)

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

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree * int

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let singleton_tree x = Node (Empty, x, Empty, 1)

let left_branch = function
    Empty -> raise Not_found
  | Node (l, _, _, _) -> l

let right_branch = function
    Empty -> raise Not_found
  | Node (_, _, r, _) -> r

let root = function
    Empty -> raise Not_found
  | Node (_, v, _, _) -> v

let height = function
    Empty -> 0
  | Node (_, _, _, h) -> h

let create l v r =
  let h' = 1 + max (height l) (height r) in
  assert(abs (height l - height r ) < 2);
  Node (l, v, r, h')

(* Assume |hl - hr| < 3 *)
let rec bal l v r =
  let hl = height l in
  let hr = height r in
  if hl >= hr + 2 then
    match l with
      Empty -> assert false
    | Node (ll, lv, lr, _) ->
	if height ll >= height lr then
	  create ll lv (create lr v r)
	else
	  match lr with
	    Empty -> assert false
	  | Node (lrl, lrv, lrr, _) ->
	      create (create ll lv lrl) lrv (create lrr v r)
  else if hr >= hl + 2 then
    match r with
      Empty -> assert false
    | Node (rl, rv, rr, _) ->
	if height rr >= height rl then
	  create (create l v rl) rv rr 
	else
	  match rl with
	    Empty -> assert false
	  | Node (rll, rlv, rlr, _) ->
	      create (create l v rll) rlv (create rlr rv rr) 
  else
    create l v r

let rec add_left v = function
    Empty -> Node(Empty, v, Empty, 1)
  | Node(l, v', r, _) -> bal (add_left v l) v' r

let rec add_right v = function
    Empty -> Node(Empty, v, Empty, 1)
  | Node(l, v', r, _) -> bal l v' (add_right v r)

(* No assumption of height of l and r. *)
let rec make_tree l v r =
  match l , r with
    Empty, _ -> add_left v r
  | _, Empty -> add_right v l
  | Node(ll, lv, lr, lh), Node(rl, rv, rr, rh) ->
      if lh > rh + 1 then bal ll lv (make_tree lr v r) else
      if rh > lh + 1 then bal (make_tree l v rl) rv rr else
      create l v r

(* Utilities *)
let rec split_leftmost = function
    Empty -> raise Not_found
  | Node (Empty, v, r, _) -> (v, r)
  | Node (l, v, r, _) ->
      let v0, l' = split_leftmost l in
      (v0, make_tree l' v r)

let rec split_rightmost = function
    Empty -> raise Not_found
  | Node (l, v, Empty, _) -> (v, l)
  | Node (l, v, r, _) ->
      let v0, r' = split_rightmost r in
      (v0, make_tree l v r')

let rec concat t1 t2 =
  match t1, t2 with
    Empty, _ -> t2
  | _, Empty -> t1
  | Node (l1, v1, r1, h1), Node (l2, v2, r2, h2) ->
      if h1 < h2 then
	make_tree (concat t1 l2) v2 r2
      else
	make_tree l1 v1 (concat r1 t2)

let rec iter proc = function
    Empty -> ()
  | Node (l, v, r, _) ->
      iter proc l;
      proc v;
      iter proc r

let rec fold f t init =
  match t with
    Empty -> init
  | Node (l, v, r, _) ->
      let x = fold f l init in
      let x = f v x in
      fold f r x
