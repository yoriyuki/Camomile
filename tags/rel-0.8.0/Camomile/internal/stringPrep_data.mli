(* Copyright 2010 Pierre Chambart *)
(** Data defined in appendix of RFC 3454 *)

(** type of replacement for characters *)
type mapping =
  | Diff of int (** character replaced by its codepoint + the diff value *)
  | List of UChar.t list (** character replaced by this list of characters *)

module MappingMap : UCharTbl.Type with type elt = mapping

val mapping_to_list : UChar.t -> mapping -> UChar.t list

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

module Make(Config : ConfigInt.Type) : Type
