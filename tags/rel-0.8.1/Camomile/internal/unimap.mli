(* $Id: unimap.mli,v 1.3 2006/08/13 17:09:17 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

module type Type = sig
type mapping
val read_map : mapping -> int -> int

type mapping_rw
val create_mapping_rw : int -> mapping_rw
val mapping_rw_to_ro : mapping_rw -> mapping
val add_mapping : mapping_rw -> int -> int -> unit

type t = { enc_to_ucs : mapping; ucs_to_enc : mapping; } 
type rw = { rw_enc_to_ucs : mapping_rw; rw_ucs_to_enc : mapping_rw; } 

val create_rw : int -> int -> rw
val add : rw -> int -> int -> unit
val rw_to_ro : rw -> t
val no_char_ucs : t -> int
val no_char_enc : t -> int
val enc_to_ucs : t -> int -> int
val ucs_to_enc : t -> int -> int

val of_name : string -> t
end

module Make (Config : ConfigInt.Type) : Type
