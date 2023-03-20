(** Generate the mapping tables Unicode <-> Asian encodings *)
(* Copyright (C) 2002, 2003 Yamagata Yoriyuki *)

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

(*
#directory "../"
#directory "../public"
#directory "../internal"
#load "str.cma"
#load "camomile.cma"
*)

open Camomile.Private
module Unimap = Unimap.Make (Toolslib.Camomileconfig)

let escape_char = ref '\\'
let comment_char = ref '#'
let codeset_name = ref "default"
let blank_pat = Str.regexp "[ \t]+"
let empty_line = Str.regexp "[ \t]*$"

exception Break

let begin_with s s' =
  if String.length s < String.length s' then false
  else (
    try
      for i = 0 to String.length s' - 1 do
        if s.[i] <> s'.[i] then raise Break
      done;
      true
    with Break -> false)

let header c =
  try
    while true do
      let s = input_line c in
      if Str.string_match empty_line s 0 || s.[0] = !comment_char then ()
      else if begin_with s "<code_set_name>" then
        codeset_name := List.nth (Str.split blank_pat s) 1
      else if begin_with s "<comment_char>" then begin
        comment_char := (List.nth (Str.split blank_pat s) 1).[0]
      end
      else if begin_with s "<escape_char>" then
        escape_char := (List.nth (Str.split blank_pat s) 1).[0]
      else if begin_with s "<mb_cur_min>" then ()
      else if begin_with s "<mb_cur_max>" then ()
      else if begin_with s "CHARMAP" then raise Break
      else if begin_with s (String.make 1 !comment_char) then ()
      else failwith "Unknown header."
    done
  with Break -> ()

let sym_name_pat = Str.regexp "<U\\([0-9A-F]*\\)>"

let get_enc s esc =
  let proc_constant s =
    match s.[0] with
      | 'x' -> int_of_string ("0" ^ s)
      | 'd' -> int_of_string (String.sub s 1 (String.length s))
      | _ -> int_of_string ("0o" ^ s)
  in
  let constants = Str.split (Str.regexp (String.make 1 esc)) s in
  let chars = List.map Char.chr (List.map proc_constant constants) in
  let s' = Bytes.create (List.length chars) in
  for i = 0 to Bytes.length s' - 1 do
    Bytes.set s' i (List.nth chars i)
  done;
  Bytes.to_string s'

type cjk_type = CN | GR | JP | KO | TW

let gb2312 = Unimap.create_rw 0x00 0x00
let iso88597 = Unimap.create_rw 0xff 0xff
let jisx0201 = Unimap.create_rw 0xff 0xff
let jisx0208 = Unimap.create_rw 0x00 0x00
let jisx0212 = Unimap.create_rw 0x00 0x00
let ksc5601 = Unimap.create_rw 0x00 0x00
let cns11643 = Unimap.create_rw 0x00 0x00

let charmap cjk c =
  try
    while true do
      let s = input_line c in
      if String.length s = 0 || s.[0] = !comment_char then ()
      else if begin_with s "END CHARMAP" then raise Break
      else (
        let tokens = Str.split blank_pat s in
        let sym_name = List.hd tokens in
        if Str.string_match sym_name_pat sym_name 0 then (
          let u = int_of_string ("0x" ^ Str.matched_group 1 sym_name) in
          let enc = get_enc (List.nth tokens 1) !escape_char in
          match cjk with
            | CN -> (
                match String.length enc with
                  | 2 ->
                      let enc1 = Char.code enc.[0] in
                      let enc2 = Char.code enc.[1] in
                      let gb = ((enc1 - 0x80) lsl 8) lor (enc2 - 0x80) in
                      Unimap.add gb2312 gb u
                  | _ -> ())
            | GR -> Unimap.add iso88597 (Char.code enc.[0]) u
            | JP -> (
                match String.length enc with
                  | 1 ->
                      (*jisx0201_roman*)
                      Unimap.add jisx0201 (Char.code enc.[0]) u
                  | 2 ->
                      (*jisx0201_kana or jisx0208*)
                      let enc1 = Char.code enc.[0] in
                      let enc2 = Char.code enc.[1] in
                      begin
                        match enc1 with
                          | 0x8e -> Unimap.add jisx0201 enc2 u
                          | _ ->
                              let jis =
                                ((enc1 - 0x80) lsl 8) lor (enc2 - 0x80)
                              in
                              Unimap.add jisx0208 jis u
                      end
                  | 3 ->
                      (*jisx0212*)
                      let enc1 = Char.code enc.[0] in
                      let enc2 = Char.code enc.[1] in
                      let enc3 = Char.code enc.[2] in
                      if enc1 <> 0x8f then failwith "Broken entry."
                      else (
                        let jis = ((enc2 - 0x80) lsl 8) lor (enc3 - 0x80) in
                        Unimap.add jisx0212 jis u)
                  | _ -> failwith "Broken entry.")
            | KO -> (
                match String.length enc with
                  | 2 ->
                      let enc1 = Char.code enc.[0] in
                      let enc2 = Char.code enc.[1] in
                      let ksc = ((enc1 - 0x80) lsl 8) lor (enc2 - 0x80) in
                      Unimap.add ksc5601 ksc u
                  | _ -> ())
            | TW -> (
                match String.length enc with
                  | 1 -> () (*ascii*)
                  | 2 ->
                      (*CNS 11643 Plane 1*)
                      let enc1 = Char.code enc.[0] in
                      let enc2 = Char.code enc.[1] in
                      let cns =
                        0x010000 lor ((enc1 - 0x80) lsl 8) lor (enc2 - 0x80)
                      in
                      Unimap.add cns11643 cns u
                  | 4 ->
                      (*other planes*)
                      let enc1 = Char.code enc.[0] in
                      let enc2 = Char.code enc.[1] in
                      let enc3 = Char.code enc.[2] in
                      let enc4 = Char.code enc.[3] in
                      if enc1 <> 0x8e then failwith "Broken entry."
                      else (
                        let cns =
                          ((enc2 - 0xa0) lsl 16)
                          lor ((enc3 - 0x80) lsl 8)
                          lor (enc4 - 0x80)
                        in
                        Unimap.add cns11643 cns u)
                  | _ -> failwith "Broken entry."))
        else failwith "Broken entry.")
    done
  with Break -> ()

let main () =
  let found = ref [] in
  (try
     let map = open_in "../charmaps/GB2312" in
     header map;
     charmap CN map;
     found := CN :: !found
   with Sys_error _ -> ());
  (try
     let map = open_in "../charmaps/ISO-8859-7" in
     header map;
     charmap GR map;
     found := GR :: !found
   with Sys_error _ -> ());
  (try
     let map = open_in "../charmaps/EUC-JP" in
     header map;
     charmap JP map;
     found := JP :: !found
   with Sys_error _ -> ());
  (try
     let map = open_in "../charmaps/EUC-KR" in
     header map;
     charmap KO map;
     found := KO :: !found
   with Sys_error _ -> ());
  (try
     let map = open_in "../charmaps/EUC-TW" in
     header map;
     charmap TW map;
     found := TW :: !found
   with Sys_error _ -> ());
  if List.mem CN !found then
    Database.write "." "mar" output_value "gb2312" (Unimap.rw_to_ro gb2312);
  if List.mem GR !found then
    Database.write "." "mar" output_value "iso88597" (Unimap.rw_to_ro iso88597);
  if List.mem JP !found then begin
    Database.write "." "mar" output_value "jisx0201" (Unimap.rw_to_ro jisx0201);
    Database.write "." "mar" output_value "jisx0208" (Unimap.rw_to_ro jisx0208);
    Database.write "." "mar" output_value "jisx0212" (Unimap.rw_to_ro jisx0212)
  end;
  if List.mem KO !found then
    Database.write "." "mar" output_value "ksc5601" (Unimap.rw_to_ro ksc5601);
  if List.mem TW !found then
    Database.write "." "mar" output_value "cns11643" (Unimap.rw_to_ro cns11643)

let _ = main ()
