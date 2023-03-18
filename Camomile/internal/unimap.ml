(** Unicode <-> Asian charctor codings *)
(* Copyright (C) 2002 Yamagata Yoriyuki *)

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

module type Type = sig
  type mapping

  val read_map : mapping -> int -> int

  type mapping_rw

  val create_mapping_rw : int -> mapping_rw
  val mapping_rw_to_ro : mapping_rw -> mapping
  val add_mapping : mapping_rw -> int -> int -> unit

  type t = { enc_to_ucs : mapping; ucs_to_enc : mapping }
  type rw = { rw_enc_to_ucs : mapping_rw; rw_ucs_to_enc : mapping_rw }

  val create_rw : int -> int -> rw
  val add : rw -> int -> int -> unit
  val rw_to_ro : rw -> t
  val no_char_ucs : t -> int
  val no_char_enc : t -> int
  val enc_to_ucs : t -> int -> int
  val ucs_to_enc : t -> int -> int
  val of_name : string -> t
end

module Make (Config : ConfigInt.Type) : Type = struct
  type mapping = { no_char : int; tbl : Tbl31.Bytes.t }

  let read_map map i = Tbl31.Bytes.get map.tbl i

  type mapping_rw = { rw_no_char : int; mutable rw_tbl : int IMap.t }

  let create_mapping_rw no_char = { rw_no_char = no_char; rw_tbl = IMap.empty }

  let mapping_rw_to_ro rw =
    {
      no_char = rw.rw_no_char;
      tbl = Tbl31.Bytes.of_map rw.rw_no_char rw.rw_tbl;
    }

  let add_mapping map i n = map.rw_tbl <- IMap.add i n map.rw_tbl

  type t = { enc_to_ucs : mapping; ucs_to_enc : mapping }
  type rw = { rw_enc_to_ucs : mapping_rw; rw_ucs_to_enc : mapping_rw }

  let create_rw unused_enc unused_ucs =
    {
      rw_enc_to_ucs = create_mapping_rw unused_ucs;
      rw_ucs_to_enc = create_mapping_rw unused_enc;
    }

  let add map enc ucs =
    add_mapping map.rw_enc_to_ucs enc ucs;
    add_mapping map.rw_ucs_to_enc ucs enc

  let rw_to_ro map =
    {
      enc_to_ucs = mapping_rw_to_ro map.rw_enc_to_ucs;
      ucs_to_enc = mapping_rw_to_ro map.rw_ucs_to_enc;
    }

  let no_char_ucs map = map.enc_to_ucs.no_char
  let no_char_enc map = map.ucs_to_enc.no_char
  let enc_to_ucs map enc = read_map map.enc_to_ucs enc
  let ucs_to_enc map ucs = read_map map.ucs_to_enc ucs
  let loaded = Hashtbl.create 0

  let of_name name : t =
    try
      let b = Hashtbl.find loaded name in
      match Weak.get b 0 with
        | None ->
            Hashtbl.remove loaded name;
            raise Not_found
        | Some x -> x
    with Not_found ->
      let map = Database.read Config.unimapdir "mar" input_value name in
      let b = Weak.create 1 in
      Weak.set b 0 (Some map);
      Hashtbl.add loaded name b;
      map
end
