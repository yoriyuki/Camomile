(** Set of integers *)
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
(* yoriyuki.yamagata@aist.go.jp *)

type t = (int * int) AvlTree.tree

type elt = int

val empty : t

val is_empty : t -> bool

val mem : int -> t -> bool

val add : int -> t -> t

val add_range : int -> int -> t -> t

val singleton : int -> t

val remove : int -> t -> t

val remove_range : int -> int -> t -> t

val union : t -> t -> t

val inter : t -> t -> t

val diff : t -> t -> t

val compl : t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val subset : t -> t -> bool

val from : int -> t -> t

val after : int -> t -> t

val until : int -> t -> t

val before : int -> t -> t

val iter : (int -> unit) -> t -> unit

val iter_range : (int -> int -> unit) -> t -> unit

val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

val fold_range : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (int -> bool) -> t -> bool

val exists : (int -> bool) -> t -> bool
    
val filter : (int -> bool) -> t -> t

val partition : (int -> bool) -> t -> t * t

val cardinal : t -> int

val elements : t -> int list

val ranges : t -> (int * int) list

val min_elt : t -> int

val max_elt : t -> int

val choose : t -> int
