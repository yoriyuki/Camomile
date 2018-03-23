(** Data defined in appendix of RFC 3454 *)
(* Copyright (C) 2010 Pierre Chambart *)

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


type mapping =
  | Diff of int
  | List of UChar.t list

module MappingHash =
struct
  type t = mapping
  let hash = Hashtbl.hash
  let equal = (=)
end

module MappingMap = UCharTbl.Make ( MappingHash )

let mapping_to_list index = function
  | Diff (0) -> [index]
  | Diff (diff) -> [UChar.of_int ((UChar.code index) + diff)]
  | List l -> l


module type Type =
sig
  val map_b1b2 : unit -> MappingMap.t
  val map_b1 : unit -> MappingMap.t
  val saslprep_map : unit -> MappingMap.t
  val nodeprep_prohibited : unit -> UCharTbl.Bool.t
  val resourceprep_prohibited : unit -> UCharTbl.Bool.t
  val nameprep_prohibited : unit -> UCharTbl.Bool.t
  val saslprep_prohibited : unit -> UCharTbl.Bool.t
  val trace_prohibited : unit -> UCharTbl.Bool.t
  val iscsi_prohibited : unit -> UCharTbl.Bool.t
  val mib_prohibited : unit -> UCharTbl.Bool.t
  val d1 : unit -> UCharTbl.Bool.t
  val d2 : unit -> UCharTbl.Bool.t
end

module Make(Config : ConfigInt.Type) : Type =
struct

  module UData = Unidata.Make(Config)

  let map_b1b2 ()                = UData.read_data "map_b1b2"
  let map_b1 ()                  = UData.read_data "map_b1"
  let saslprep_map ()            = UData.read_data "saslprep_map"
  let nodeprep_prohibited ()     = UData.read_data "nodeprep_prohibited"
  let resourceprep_prohibited () = UData.read_data "resourceprep_prohibited"
  let nameprep_prohibited ()     = UData.read_data "nameprep_prohibited"
  let saslprep_prohibited ()     = UData.read_data "saslprep_prohibited"
  let trace_prohibited ()        = UData.read_data "trace_prohibited"
  let iscsi_prohibited ()        = UData.read_data "iscsi_prohibited"
  let mib_prohibited ()          = UData.read_data "mib_prohibited"
  let d1 ()                      = UData.read_data "d1"
  let d2 ()                      = UData.read_data "d2"

end
