(* Copyright (C) 2003 Yamagata Yoriyuki *)

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


#load "bigarray.cma";;
#load "camomile.cma";;
#load "str.cma";;

let name_pat = Str.regexp "Name:[ \t]+\\([^ \t]+\\)"
let alias_pat = Str.regexp "Alias:[ \t]+\\([^ \t]+\\)"

let codegen name iana_names =
  match name with
    None -> ()
  | Some name ->
    List.iter (fun alias ->
        Printf.printf "let () = alias \"IANA/%s\" \"%s\"\n" alias name)
      iana_names

let () = 
  let iana_names = ref [] in
  let name = ref None in
  try while true do
      let line = read_line () in
      if Str.string_match name_pat line 0 then begin
        codegen !name !iana_names;
        let s = Str.matched_group 1 line in
        iana_names := [s];
        name := (try 
                   let enc = CharEncoding.of_name s in
                   Some (CharEncoding.name_of enc)
                 with Not_found ->
                 try
                   let s = String.uppercase s in
                   let enc = CharEncoding.of_name s in
                   Some (CharEncoding.name_of enc)
                 with Not_found ->
                   None);
        Printf.printf "(* %s *)\n" line;
      end else if Str.string_match alias_pat line 0 then
        let alias = Str.matched_group 1 line in
        if alias = "None" then () else begin
          iana_names := alias :: !iana_names;
          if !name <> None then () else
            name := (try 
                       let enc = CharEncoding.of_name alias in
                       Some (CharEncoding.name_of enc)
                     with Not_found -> 
                     try
                       let s = String.uppercase alias in
                       let enc = CharEncoding.of_name alias in
                       Some (CharEncoding.name_of enc)
                     with Not_found ->
                       None);
          Printf.printf "(* %s *)\n" line
        end
      else ()
    done with End_of_file -> ()
