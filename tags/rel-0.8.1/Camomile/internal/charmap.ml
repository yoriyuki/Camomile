(* $Id: charmap.ml,v 1.11 2006/08/13 17:09:17 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

module type Interface = sig
type enc_to_ucs

val make_enc_to_ucs : int -> (string * int) list -> enc_to_ucs
val no_char_of : enc_to_ucs -> int

type probe_state
val start_probe : enc_to_ucs -> probe_state
val look_probe : probe_state -> int -> int
val next_probe : probe_state -> int -> probe_state option

type t = {
  name : string;
  ucs_to_enc : string Tbl31.t;
  enc_to_ucs : enc_to_ucs;
} 

type data = Alias of string | CMap of t

val of_name : string -> t
end

module Configure (Config : ConfigInt.Type) = struct
type enc_to_ucs = 
    {enc_to_ucs_no_char : int;
     enc_to_ucs_tbl : Byte_labeled_dag.bytes}

let make_enc_to_ucs no_char vs =
  {enc_to_ucs_no_char = no_char;
   enc_to_ucs_tbl = Byte_labeled_dag.make_bytes no_char vs}

let no_char_of enc_to_ucs = enc_to_ucs.enc_to_ucs_no_char

type probe_state = Byte_labeled_dag.bytes

let start_probe enc_to_ucs = enc_to_ucs.enc_to_ucs_tbl

let look_probe state i = 
  Byte_labeled_dag.look_leaf_bytes state i

let next_probe state i =
  Byte_labeled_dag.look_branch_bytes state i

type t = 
    {name : string; ucs_to_enc : string Tbl31.t; enc_to_ucs : enc_to_ucs}

type data = Alias of string | CMap of t

let loaded = Hashtbl.create 0

let of_name name =
  try 
    let b = Hashtbl.find loaded name in
    match Weak.get b 0 with
      None ->
	Hashtbl.remove loaded name;
	raise Not_found
    | Some x -> x
  with Not_found ->
    let rec look count name = 
      (try Helpers.sanitize name with _ -> raise Not_found);
      let filename = Filename.concat Config.charmapdir (name ^ ".mar") in
      let c = try open_in_bin filename with Sys_error _  -> raise Not_found in
      let data : data = input_value c in
      match data with
	Alias s -> 
	  if count = 1 then raise Not_found else 
	  look 1 s
      | CMap t -> t in
  let t = look 0 name  in
  let b = Weak.create 1 in
  Weak.set b 0 (Some t);
  Hashtbl.add loaded name b;
  t
end
