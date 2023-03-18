(* Copyright (C) 2003 Yamagata Yoriyuki *)

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

(** Camomile has a locale system similar to Java.
    A locale is a string with a form as
    "<LANG>_<COUNTRY>_<MODIFIER>..." where <LANG> is
    a 2-letter ISO 639 language code, <COUNTRY> is a 2-letter ISO 3166
    country code.  Some field may not present. *)

(** Type of locales. *)
type t = string

(** [read root suffix reader locale]
    reads locale information using [reader].
    Locale data is supposed to reside in [root] directory with
    the name [locale].[suffix].
    [reader] takes [in_channel] as an argument and read data from in_channel.
    If data is not found, then [reader] should raise Not_found.
    If the file is not found or [reader] raises Not_found, then
    more generic locales are tried.
    For example, if fr_CA.[suffix] is not found, then [read] tries fr.[suffix].
    If fr.[suffix] is also not found, then the file [root].[suffix] is tried.
    Still the data is not found, then [Not_found] is raised. *)
val read : string -> string -> (in_channel -> 'a) -> string -> 'a

(** [contain loc1 loc2] :
    If [loc1] is contained in [loc2] then true otherwise false.
    For example, "fr" is contained in "fr_CA" while "en_CA"
    does not contain "fr" *)
val contain : string -> string -> bool
