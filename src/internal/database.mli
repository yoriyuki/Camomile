(** Database.ml : Unified interfaces of stored data for Camomile *)
(* Copyright (C) 2011 Yoriyuki Yamagata *)

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

(** [read dir suffix reader key] reads information using [reader].
    Data are supposed to reside in the files under [dir] directory
    with suffix [suffix].  [reader] takes [in_channel] as an argument
    and read data from in_channel.  The [key] specifies key associated
    the value.  Any characters can be used in [key], since they are
    properly escaped.  If there are no data associated to [key], raise
    Not_found.
*)
val read : string -> string -> (in_channel -> 'a) -> string -> 'a

(** [writer dir suffix writer key data] write [data] associated the
    [key] into the directory [dir] with [suffix]. You can use
    any characters in [key] since they are propery escaped.*)
val write :
  string -> string -> (out_channel -> 'a -> unit) -> string -> 'a -> unit
