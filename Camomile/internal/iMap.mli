(** mappings from integer to arbitrary types *)
(* Copyright (C) 2003-2010 Yamagata Yoriyuki. distributed with LGPL *)

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

type +'a t = (int * int * 'a) AvlTree.tree

type key = int

val empty : 'a t

val is_empty : 'a t -> bool

val add : ?eq:('a -> 'a -> bool) -> int -> 'a -> 'a t -> 'a t

val add_range : ?eq:('a -> 'a -> bool) -> int -> int -> 'a -> 'a t -> 'a t

val find : int -> 'a t -> 'a

val remove : int -> 'a t -> 'a t

val remove_range : int -> int -> 'a t -> 'a t

val from : int -> 'a t -> 'a t

val after : int -> 'a t -> 'a t

val until : int -> 'a t -> 'a t

val before : int -> 'a t -> 'a t

val mem : int -> 'a t -> bool

val iter : (int -> 'a -> unit) -> 'a t -> unit

val iter_range : (int -> int -> 'a -> unit) -> 'a t -> unit

val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t

val mapi : ?eq:('b -> 'b -> bool) -> (int -> 'a -> 'b) -> 'a t -> 'b t

val fold : (int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a

val fold_range : (int -> int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a

val set_to_map : ISet.t -> 'a -> 'a t

val domain : 'a t -> ISet.t

val map_to_set : ('a -> bool) -> 'a t -> ISet.t
