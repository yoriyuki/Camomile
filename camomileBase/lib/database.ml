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

open Rresult

let root = ref None

let set_root dir =
  match !root with
    None -> root := Some dir; dir
  | Some d -> d

let get_root () =
  match !root with
    None -> failwith "Please initialize root dir first"
  | Some dir -> dir

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
  let realdir = Fpath.add_seg (Fpath.v (get_root ())) dir in
  let fname = escape key in
  let path = Fpath.add_ext suffix (Fpath.append realdir (Fpath.v fname)) in
  if not (Fpath.is_rooted (Fpath.v (get_root ())) path) then
    Error (`Database "out of root dir")
  else
    let c = try Ok (open_in_bin (Fpath.to_string path)) with
        Sys_error m  -> Error (`SysError m) in
    let v = c >>= reader in
    (match c with
      Ok c -> close_in c
     | _ ->());
    v

let write dir suffix writer key data =
  let realdir = Fpath.add_seg (Fpath.v (get_root ())) dir in
  let fname = escape key in
  let path = Fpath.add_ext suffix (Fpath.append realdir (Fpath.v fname)) in
  if not (Fpath.is_rooted (Fpath.v (get_root ())) path) then
    failwith "Out of root dir"
  else
    let c = open_out_bin (Fpath.to_string path) in
    writer c data;
    close_out c
