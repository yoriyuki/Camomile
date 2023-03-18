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

module type Type = sig
  type text

  exception Prohibited of UChar.t
  exception Bad_bidi

  type profile =
    [ `Nameprep  (** RFC 3491 *)
    | `Nodeprep  (** RFC 3920, Appendix A *)
    | `Resourceprep  (** RFC 3920, Appendix B *)
    | `Saslprep  (** RFC 4013 *)
    | `Trace  (** for SASL Anonymous, RFC 4505, Section 3 *)
    | `Iscsi  (** RFC 3722 *)
    | `Mib  (** RFC 4011 *) ]

  val stringprep : profile -> text -> text
end

module Make (Config : ConfigInt.Type) (Text : UnicodeString.Type) :
  Type with type text = Text.t = struct
  module UNF = UNF.Make (Config) (Text)
  module UCharInfo = UCharInfo.Make (Config)
  module StringPrep_data' = StringPrep_data.Make (Config)
  open StringPrep_data'

  type text = Text.t

  exception Prohibited of UChar.t
  exception Bad_bidi

  type normalisation = [ `C | `KC | `D | `KD | `No ]

  type profile =
    [ `Nameprep  (** RFC 3491 *)
    | `Nodeprep  (** RFC 3920, Appendix A *)
    | `Resourceprep  (** RFC 3920, Appendix B *)
    | `Saslprep  (** RFC 4013 *)
    | `Trace  (** for SASL Anonymous, RFC 4505, Section 3 *)
    | `Iscsi  (** RFC 3722 *)
    | `Mib  (** RFC 4011 *) ]

  type internal_profile = {
    map : UChar.t -> UChar.t list;
    normalize : normalisation;
    prohibited : UChar.t -> bool;
    check_bidi : bool;
    unicode_version : UCharInfo.version_type;
    bidi_ral : UChar.t -> bool;
    bidi_l : UChar.t -> bool;
  }

  let make_map map =
    let f x =
      let m = StringPrep_data.MappingMap.get map x in
      StringPrep_data.mapping_to_list x m
    in
    f

  let make_set set =
    let f x = UCharTbl.Bool.get set x in
    f

  let nodeprep () =
    {
      map = make_map (map_b1b2 ());
      normalize = `KC;
      prohibited = make_set (nodeprep_prohibited ());
      check_bidi = true;
      unicode_version = `v3_2;
      bidi_ral = make_set (d1 ());
      bidi_l = make_set (d2 ());
    }

  let resourceprep () =
    {
      map = make_map (map_b1 ());
      normalize = `KC;
      prohibited = make_set (resourceprep_prohibited ());
      check_bidi = true;
      unicode_version = `v3_2;
      bidi_ral = make_set (d1 ());
      bidi_l = make_set (d2 ());
    }

  let nameprep () =
    {
      map = make_map (map_b1b2 ());
      normalize = `KC;
      prohibited = make_set (nameprep_prohibited ());
      check_bidi = true;
      unicode_version = `v3_2;
      bidi_ral = make_set (d1 ());
      bidi_l = make_set (d2 ());
    }

  let saslprep () =
    {
      map = make_map (saslprep_map ());
      normalize = `KC;
      prohibited = make_set (saslprep_prohibited ());
      check_bidi = true;
      unicode_version = `v3_2;
      bidi_ral = make_set (d1 ());
      bidi_l = make_set (d2 ());
    }

  let trace () =
    {
      map = (fun x -> [x]);
      normalize = `No;
      prohibited = make_set (trace_prohibited ());
      check_bidi = true;
      unicode_version = `v3_2;
      bidi_ral = make_set (d1 ());
      bidi_l = make_set (d2 ());
    }

  let iscsi () =
    {
      map = make_map (map_b1b2 ());
      normalize = `KC;
      prohibited = make_set (iscsi_prohibited ());
      check_bidi = true;
      unicode_version = `v3_2;
      bidi_ral = make_set (d1 ());
      bidi_l = make_set (d2 ());
    }

  (** rfc 4011 *)
  let mib () =
    {
      map = make_map (map_b1 ());
      normalize = `KC;
      prohibited = make_set (mib_prohibited ());
      check_bidi = false;
      unicode_version = `v3_2;
      bidi_ral = (fun _ -> false);
      bidi_l = (fun _ -> false);
    }

  let to_internal_profile : profile -> unit -> internal_profile = function
    | `Nameprep -> nameprep
    | `Nodeprep -> nodeprep
    | `Resourceprep -> resourceprep
    | `Saslprep -> saslprep
    | `Trace -> trace
    | `Iscsi -> iscsi
    | `Mib -> mib

  let is_correct_bidi profile text =
    let is_rand_al_cat index = profile.bidi_ral (Text.look text index) in
    let is_lcat index = profile.bidi_l (Text.look text index) in
    let rec check_rand_al_cat index =
      let next = Text.next text index in
      if Text.out_of_range text next then is_rand_al_cat index
      else if is_lcat index then false
      else check_rand_al_cat next
    in
    let rec check_not_rand_al_cat index =
      if is_rand_al_cat index then false
      else (
        let next = Text.next text index in
        if Text.out_of_range text next then true else check_not_rand_al_cat next)
    in
    let first = Text.first text in
    if Text.out_of_range text first then (* empty text *) true
    else if is_rand_al_cat first then check_rand_al_cat first
    else check_not_rand_al_cat first

  let normalisation : normalisation -> text -> text = function
    | `C -> UNF.nfc
    | `KC -> UNF.nfkc
    | `D -> UNF.nfd
    | `KD -> UNF.nfkd
    | `No -> fun x -> x

  let stringprep profile text =
    let profile = to_internal_profile profile () in
    let buffer = Text.Buf.create 10 in
    let add_char = Text.Buf.add_char buffer in
    let map c = List.iter add_char (profile.map c) in
    Text.iter map text;
    let text = Text.Buf.contents buffer in
    Text.Buf.clear buffer;
    let text = normalisation profile.normalize text in
    let rec check_prohibited index =
      if Text.out_of_range text index then ()
      else begin
        let char = Text.look text index in
        let prohibited =
          (not (UCharInfo.older (UCharInfo.age char) profile.unicode_version))
          || profile.prohibited char
        in
        if prohibited then raise (Prohibited (Text.look text index))
        else check_prohibited (Text.next text index)
      end
    in
    check_prohibited (Text.first text);
    if profile.check_bidi then begin
      if is_correct_bidi profile text then text else raise Bad_bidi
    end
    else text
end
