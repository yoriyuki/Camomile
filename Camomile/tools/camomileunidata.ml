(* Copyright (C) 2018 Yamagata Yoriyuki.*)

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

let ucd_or_die inf = try
  let ic = if inf = "-" then stdin else open_in inf in
  let d = Uucd.decoder (`Channel ic) in
  match Uucd.decode d with
  | `Ok db -> db
  | `Error e ->
    let (l0, c0), (l1, c1) = Uucd.decoded_range d in
    Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e;
    exit 1
with Sys_error e -> Printf.eprintf "%s\n%!" e; exit 1

let convert_boolean_prop_to_tbl reportrie uucd_prop =
  let cpmap = Uucd.Cpmap.map (fun props ->
      Uucd.find props uucd_prop) reportrie in
  let set = Uucd.Cpmap.fold (fun cp p set ->
      match p with
        Some true -> USet.add (UChar.of_int cp) set
      | _ -> set) cpmap USet.empty in
  Unidata.Boolean (UCharTbl.Bool.of_set set)

module type Property = sig type t end

module GenVariantTbl(P: Property) = struct

  module Comparable(P : Property) = struct
    type t = P.t
    let compare e1 e2 = Pervasives.compare e1 e2
  end

  module PSet = Set.Make(Comparable(P))
  module PMap = Map.Make(Comparable(P))

  let convert_prop_to_tbl reportrie uucd_prop =
    let cpmap = Uucd.Cpmap.map (fun props ->
        Uucd.find props uucd_prop) reportrie in
    let prop_map = Uucd.Cpmap.fold (fun cp p m ->
        UMap.add (UChar.of_int cp) p m) cpmap UMap.empty in
    let unoption = UMap.fold_range (fun first last v unoption ->
        match v with
          None -> unoption
        | Some real_value -> UMap.add_range first last real_value unoption)
        prop_map UMap.empty in
    let val_set = UMap.fold_range (fun _ _ v val_set ->
        PSet.add v val_set) unoption PSet.empty in
    if PSet.cardinal val_set > 254 then
      failwith "Number of propoty values exceeds 254"
    else
      let id_to_prop = Array.of_list (PSet.elements val_set) in
      let prop_to_id =
        let ref_pmap = ref PMap.empty in
        for i = 0 to Array.length(id_to_prop) do
          ref_pmap := PMap.add id_to_prop.(i) (i+1) !ref_pmap
        done;
        !ref_pmap in
      let propid_map = UMap.map (fun p -> PMap.find p prop_to_id) unoption in
      Unidata.Variant (id_to_prop, UCharTbl.Bits.of_map 0 propid_map)
end

let convert_byte_prop_to_tbl reportrie uucd_prop =
  let cpmap = Uucd.Cpmap.map (fun props ->
      Uucd.find props uucd_prop) reportrie in
  let m = Uucd.Cpmap.fold (fun cp p m ->
      match p with
      Some n ->
        if n > 255 then
          failwith "Byte property is > 255"
        else
          UMap.add (UChar.of_int cp) (Char.chr n) m
      |None -> m) cpmap UMap.empty in
  Unidata.Byte (UCharTbl.Char.of_map '\000' m)

module GenTbl(P: Property) = struct
  module Option(P : Property) = struct
    type t = P.t option
    let equal e1 e2 = Pervasives.compare e1 e2 == 0
    let hash = Hashtbl.hash
  end

  module UTbl = UCharTbl.Make(Option(P))

  let convert_prop_to_tbl reportrie uucd_prop =
    let cpmap = Uucd.Cpmap.map (fun props ->
        Uucd.find props uucd_prop) reportrie in
    let m = Uucd.Cpmap.fold (fun cp p m ->
        UMap.add (UChar.of_int cp) p m) cpmap UMap.empty in
    Unidata.Any (UTbl.of_map None m)
end

let () =
  if Array.length Sys.argv < 1 then begin
    Printf.eprintf "Too few argument";
    exit 1
  end else
    let ucd = ucd_or_die (Sys.argv.(0)) in
    ()
