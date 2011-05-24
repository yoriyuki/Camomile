(** Charactor mapping table generator *)

(* Copyright (C) 2002, 2003, 2011 Yamagata Yoriyuki *)

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
(* yori@sourceforge.net *)

open Toolslib

let parse_arg () =
  let dir = ref "." in
  let file = ref None in
  let helptext =
    "camomilecharmap[.byte, .opt] -d outputpath file: 
      reads charmap from file and places output to outputpath."
  in
  Arg.parse 
    [("-d", 
      Arg.String ((:=) dir),
      "[directory]\toutputpath")]
    (fun s -> 
      if !file <> None then failwith "Too many arguments" else
      file := Some s)
    helptext;
  (!file, !dir)

let escape_char = ref '/'
let comment_char = ref '#'

let blank_pat = Str.regexp "[ \t]+"
let empty_line = Str.regexp "[ \t]*$"
let alias_pat = Str.regexp ".[ \t]*alias[ \t]+\\(.*\\)$"

let entry_pat = Str.regexp 
    "\\(.IRREVERSIBLE.\\)?<U\\([0-9A-F]*\\)>[ \t]+\\([^ \t]*\\)"

let range_pat = Str.regexp 
    "\\(.IRREVERSIBLE.\\)?<U\\([0-9A-F]*\\)>\\.\\.<U\\([0-9A-F]*\\)>[ \t]+\\([^ \t]*\\)"

exception Break

let begin_with s s' =
  if String.length s' < String.length s then false else
  try for i = 0 to (String.length s) - 1 do
    if s.[i] <> s'.[i] then raise Break
  done; 
    true 
  with Break -> false

let parse_header file inchan =
  let codeset_name = ref file in
  let aliases = ref [] in
  let unread_line = ref None in
  try while true do
    let s = input_line inchan in
    if begin_with "<code_set_name>" s then begin
      codeset_name := Some (List.nth (Str.split blank_pat s) 1)
    end else if begin_with "<comment_char>" s then
      comment_char := (List.nth (Str.split blank_pat s) 1).[0]
    else if begin_with "<escape_char>" s then
      escape_char := (List.nth (Str.split blank_pat s) 1).[0]
    else if begin_with "<mb_cur_min>" s then ()
    else if begin_with "<mb_cur_max>" s then ()
    else if Str.string_match alias_pat s 0 then
      let a = Str.split blank_pat (Str.matched_group 1 s) in
      aliases := a @ !aliases
    else if begin_with "CHARMAP" s then raise Break
    else if Str.string_match empty_line s 0 || s.[0] = !comment_char then ()
    else begin unread_line := Some s; raise Break end
  done; assert false with Break -> 
    match !codeset_name with
      None -> failwith "Codeset name is not defined"
    | Some s -> s, !aliases, unread_line

let zero = Char.code '0'

let rec int_of_hex s i j =
  let n = Char.code s.[i] in
  let n =
    if n >= 48 && n <= 57 then n - 48 else
    if n >= 65 && n <= 70 then n - 55 else
    if n >= 97 && n <= 102 then n - 87 else
    invalid_arg "int_of_hex" in
  if i >= j then n else
  n lsl 4 lor (int_of_hex s (i + 1) j)

let rec int_of_oct s i j =
  let n = Char.code s.[i] - zero in
  if n < 0 || n > 7 then invalid_arg "int_of_oct" else
  if i >= j then n else
  n lsl 3 lor (int_of_hex s (i + 1) j)

let rec int_of_dec s i j =
  let n = Char.code s.[i]- zero in
  if n < 0 || n > 9 then invalid_arg "int_of_oct" else
  if i >= j then n else
  n * 10 + (int_of_hex s (i + 1) j)

let get_enc s esc =
  let b = Buffer.create 1 in
  let rec loop i j =
    let n =
      match s.[i + 1] with
	'x' -> int_of_hex s (i + 2) (j - 1)
      | 'd' -> int_of_dec s (i + 2) (j - 1)
      | _ -> int_of_oct s (i + 1) (j - 1) in
    Buffer.add_char b (Char.chr n);
    let i = j in
    if i >= String.length s then () else
    let j = 
      try String.index_from s (i + 1) esc with 
	Not_found -> String.length s in
    loop i j in
  if s.[0] <> esc then invalid_arg ("get_enc: " ^ s) else
  loop 
    0
    (try String.index_from s 1 esc with Not_found -> 
      String.length s);
  Buffer.contents b

let int_of_name name = (int_of_string ("0x"^name))

let incr_enc s =
  let s' = String.copy s in
  let i = String.length s' - 1 in
  let c' = Char.chr (1 + Char.code s.[i]) in
  s'.[i] <- c'
    
let irreversible = ".IRREVERSIBLE."

let is_irreversible s =
  irreversible.[0] <- !escape_char;
  irreversible.[13] <- !escape_char;
  begin_with irreversible s

let parse_body unread_line inchan =
  let enc2u = ref [] in
  let u2enc = ref IMap.empty in
  try while true do
    let s =
      match !unread_line with
	None -> input_line inchan 
      | Some s -> unread_line := None; s in
    if Str.string_match entry_pat s 0 then
      let n = int_of_name (Str.matched_group 2 s) in
      let enc = get_enc (Str.matched_group 3 s) !escape_char in
      enc2u := (enc, n) :: !enc2u;
      if not (is_irreversible s || IMap.mem n !u2enc) then
	u2enc := IMap.add n enc !u2enc;
    else if Str.string_match range_pat s 0 then
	let n1 = int_of_name (Str.matched_group 2 s) in
	let n2 = int_of_name (Str.matched_group 3 s) in
	if n1 > n2 then failwith ("Broken range: " ^ s);
	let enc = get_enc (Str.matched_group 4 s) !escape_char in
	let irreversible = is_irreversible s in
	for n = n1 to n2 do
	  enc2u := (enc, n) :: !enc2u;
	  if not (irreversible || IMap.mem n !u2enc) then
	    u2enc := IMap.add n enc !u2enc;
	  incr_enc enc
	done
    else if Str.string_match empty_line s 0 || s.[0] = !comment_char then ()
    else if begin_with "END CHARMAP" s then raise Break else
    failwith ("Broken entry: " ^ s)
  done; assert false with Break ->
    !enc2u, !u2enc

module StringTbl = Tbl31.Make (struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end)

open Charmap

let main () =
  let file, dir = parse_arg () in
  let inchan = 
    match file with
      None -> stdin
    | Some file -> open_in file in
  let codeset_name, aliases, unread_line = parse_header file inchan in
  let enc2u, u2enc = parse_body unread_line inchan in
  let ucs_to_enc = StringTbl.of_map "" u2enc in
(* search unused ucs-character *)
  let no_char = 
    let rec scan i =
      let s = Tbl31.get ucs_to_enc i in
(*    Printf.eprintf "%d - %s\n" i (String.escaped s); *)
      match s with
	"" -> i
      | _ ->
	  if i > 255 then 0xffff else scan (i + 1)
    in scan 0 in
  let enc_to_ucs = Charmap.make_enc_to_ucs no_char enc2u in
  let data = CMap {name = codeset_name; 
		   ucs_to_enc = ucs_to_enc;
		   enc_to_ucs = enc_to_ucs} in 
  begin
    Database.write dir "mar" output_value codeset_name data;
    List.iter (fun a -> 
      Database.write dir "mar" output_value a (Alias codeset_name))
      aliases;
  end

let () = main ()
