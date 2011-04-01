(** Database.ml : Unified interfaces of stored data for Camomile *)
(* Copyright (C) 2011 National Institute of Advanced Science and *)
(* Technology *)

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


let escape s =
  let b = Buffer.create 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
      '0'..'9' | 'a'..'z'|'A'..'Z' | '-' | '_' | '@' as c -> Buffer.add_char b c;
    | _ as c -> 
	Printf.ksprintf (Buffer.add_string b) "%%%02X" (Char.code c)
  done;
  Buffer.contents b

let read dir suffix reader key =
  let fname = escape key in
  let path = Filename.concat dir (fname ^ "." ^ suffix) in
  let c = try open_in_bin path with Sys_error _  -> raise Not_found in
  let v = reader c in
  close_in c;
  v

let write dir suffix writer key data =
  let fname = escape key in
  let path = Filename.concat dir (fname ^ "." ^ suffix) in
  let c = try open_out_bin path with Sys_error _  -> raise Not_found in
  writer c data;
  close_out c;

