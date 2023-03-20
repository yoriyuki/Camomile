(** Avltree *)
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
(* yoriyuki.y@gmail.com *)

type +'a tree

val empty : 'a tree
val is_empty : 'a tree -> bool
val make_tree : 'a tree -> 'a -> 'a tree -> 'a tree
val height : 'a tree -> int

(* If tree is empty, then raise Not_found *)
val left_branch : 'a tree -> 'a tree
val right_branch : 'a tree -> 'a tree
val root : 'a tree -> 'a

(* Utilities *)
val singleton_tree : 'a -> 'a tree
val split_leftmost : 'a tree -> 'a * 'a tree
val split_rightmost : 'a tree -> 'a * 'a tree
val concat : 'a tree -> 'a tree -> 'a tree
val iter : ('a -> unit) -> 'a tree -> unit
val fold : ('a -> 'b -> 'b) -> 'a tree -> 'b -> 'b
