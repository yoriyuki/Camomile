(* Copyright 2010 Pierre Chambart *)

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
  open UData

  let map_b1b2 () = read_data "map_b1b2"
  let map_b1 () = read_data "map_b1"
  let saslprep_map () = read_data "saslprep_map"
  let nodeprep_prohibited () = read_data "nodeprep_prohibited"
  let resourceprep_prohibited () = read_data "resourceprep_prohibited"
  let nameprep_prohibited () = read_data "nameprep_prohibited"
  let saslprep_prohibited () = read_data "saslprep_prohibited"
  let trace_prohibited () = read_data "trace_prohibited"
  let iscsi_prohibited () = read_data "iscsi_prohibited"
  let mib_prohibited () = read_data "mib_prohibited"
  let d1 () = read_data "d1"
  let d2 () = read_data "d2"

end
