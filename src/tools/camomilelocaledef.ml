(** Locale definition compiler *)

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
(* yoriyuki.y@gmail.com *)

open CamomileLib
open Camomilelocaledef_lexer
open Toolslib
module CE = CharEncoding.Configure (Toolslib.Camomileconfig)

let enc, readfile, dir =
  let enc = ref CE.utf8 in
  let readfile = ref stdin in
  let dir = ref Filename.current_dir_name in
  Arg.parse
    [
      ( "--enc",
        Arg.String (fun encname -> enc := CE.of_name encname),
        "Encoding name" );
      ( "--file",
        Arg.String (fun filename -> readfile := open_in_bin filename),
        "Reading file" );
    ]
    (fun dirname -> dir := dirname)
    "camomilelocaledef --enc ENCNAME --file INPUTFILE DIRECTORY:\n\
     Read the localedef INPUTFILE using the encoding ENCNAME and put the \
     compiled data into DIRECTORY. If ENCNAME is ommited, UTF-8 is used.  If \
     INPUTFILE is ommited, reading from stdin. If DIRECTORY is ommited, the \
     current directory is used.";
  (!enc, !readfile, !dir)

module Utf8NF = UNF.Make (Toolslib.Camomileconfig) (UTF8)

let string_to_binary s =
  let n = String.length s / 2 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    let d = int_of_string ("0x" ^ String.sub s (i * 2) 2) in
    Bytes.set b i (Char.chr d)
  done;
  Bytes.to_string b

type data =
  | Table of (string, data) Hashtbl.t
  | Array_data of data array
  | String_data of string
  | Binary of string
  | Int of int
  | Intvect of int array
  | Tagged of string * data

let rec parse_intvect l a =
  match l with
    | Text num :: Comma :: rest -> parse_intvect rest (int_of_string num :: a)
    | Text num :: rest ->
        (Intvect (Array.of_list (List.rev (int_of_string num :: a))), rest)
    | _ -> (Intvect (Array.of_list (List.rev a)), l)

let rec parse_table l a =
  match parse l with
    | Some d, rest -> parse_table rest (d :: a)
    | None, rest ->
        let tbl = Hashtbl.create (List.length a) in
        let proc ent =
          match ent with
            | Tagged (name, data) -> Hashtbl.add tbl name data
            | _ -> failwith "A broken table entry."
        in
        List.iter proc a;
        (Table tbl, rest)

and parse_array l a =
  match l with
    | Brace_l :: rest -> (
        let data, rest = parse_unknown rest in
        match rest with
          | Brace_r :: Comma :: rest -> parse_array rest (data :: a)
          | Brace_r :: rest -> parse_array rest (data :: a)
          | _ -> failwith "A brace is not enclosed.")
    | Text text :: Comma :: rest -> parse_array rest (String_data text :: a)
    | Text text :: rest ->
        (Array_data (Array.of_list (List.rev (String_data text :: a))), rest)
    | _ -> (Array_data (Array.of_list (List.rev a)), l)

and parse_unknown l =
  match l with
    | Text text :: Brace_r :: rest -> (String_data text, Brace_r :: rest)
    | Text _ :: Comma :: _ -> parse_array l []
    | Text _ :: _ -> parse_table l []
    | _ -> parse_array l []

and parse l =
  match l with
    | Text tname :: Colon :: Text "table" :: Brace_l :: rest -> (
        let data, rest = parse_table rest [] in
        match rest with
          | Brace_r :: rest -> (Some (Tagged (tname, data)), rest)
          | _ -> failwith "A brace is not enclosed.")
    | Text tname :: Colon :: Text "array" :: Brace_l :: rest -> (
        let data, rest = parse_array rest [] in
        match rest with
          | Brace_r :: rest -> (Some (Tagged (tname, data)), rest)
          | _ -> failwith "A brace is not enclosed.")
    | Text tname
      :: Colon
      :: Text "string"
      :: Brace_l
      :: Text data
      :: Brace_r :: rest ->
        (Some (Tagged (tname, String_data data)), rest)
    | Text tname
      :: Colon
      :: Text "bin"
      :: Brace_l
      :: Text data
      :: Brace_r :: rest ->
        let b = string_to_binary data in
        (Some (Tagged (tname, Binary b)), rest)
    | Text tname
      :: Colon
      :: Text "import"
      :: Brace_l :: Text _ :: Brace_r :: rest ->
        prerr_endline "Warning : file loading is not supported.";
        (Some (Tagged (tname, Binary "")), rest)
    | Text tname
      :: Colon
      :: Text "int"
      :: Brace_l
      :: Text num
      :: Brace_r :: rest ->
        let n = int_of_string num in
        (Some (Tagged (tname, Int n)), rest)
    | Text tname :: Colon :: Text "intvector" :: Brace_l :: rest -> (
        let data, rest = parse_intvect rest [] in
        match rest with
          | Brace_r :: rest -> (Some (Tagged (tname, data)), rest)
          | _ -> failwith "A brace is not enclosed.")
    | Text name :: Brace_l :: rest -> (
        let data, rest = parse_unknown rest in
        match rest with
          | Brace_r :: rest -> (Some (Tagged (name, data)), rest)
          | _ -> failwith "A brace is not enclosed.")
    | _ -> (None, l)

let col_parse s =
  let s = Utf8NF.nfd s in
  let lexbuf = Lexing.from_string s in
  let ace_info = ColParser.main ColLexer.token lexbuf in
  AbsCe.cetbl_of ace_info

let localedef = function
  | Table tbl ->
      let col_info =
        try
          Some
            (match Hashtbl.find tbl "CollationElements" with
              | Table tbl -> (
                  match Hashtbl.find tbl "Sequence" with
                    | String_data s -> col_parse s
                    | _ -> assert false)
              | _ -> assert false)
        with Not_found -> None
      in
      { DefaultModules.Unidata.col_info }
  | _ -> assert false

let main () =
  let cs = Stream.of_channel readfile in
  let stream = CE.ustream_of enc cs in
  let lexed = lexer stream in
  let data, rest = parse_table lexed [] in
  if rest <> [] then (
    let rest =
      let b = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer b in
      Format.pp_print_list pp fmt rest;
      Buffer.contents b
    in
    failwith ("Strange trailing data: '" ^ rest ^ "'"));
  let proc key entry =
    let locale_info = localedef entry in
    Camomile.Private.Database.write dir "mar" output_value key locale_info
  in
  match data with
    | Table tbl -> Hashtbl.iter proc tbl
    | _ -> failwith "Broken data."

let _ = main ()
