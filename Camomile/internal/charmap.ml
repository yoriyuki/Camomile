(** Charactor mapping table *)
(* Copyright (C) 2002, 2011 Yamagata Yoriyuki *)

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
        let data : data = Database.read Config.charmapdir "mar" input_value name in
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
