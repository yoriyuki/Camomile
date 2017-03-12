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
(* yori@users.sourceforge.net *)
open Toolslib
  
open AbsCe
  
module CE = CharEncoding.Configure(Camomileconfig)
  
module Info = UCharInfo.Make(Camomileconfig)
  
let (enc, readfile, dir) =
  let enc = ref CE.utf8 in
  let readfile = ref stdin in
  let dir = ref Filename.current_dir_name
  in
    (Arg.parse
       [ ("--enc", (Arg.String (fun encname -> enc := CE.of_name encname)),
          "Encoding name");
         ("--file",
          (Arg.String (fun filename -> readfile := open_in_bin filename)),
          "Reading file") ]
       (fun dirname -> dir := dirname)
       "camomilelocaledef --enc ENCNAME --file INPUTFILE DIRECTORY:\n\
    Read the localedef INPUTFILE using the encoding ENCNAME \
and put the compiled data into DIRECTORY. \
    If ENCNAME is ommited, UTF-8 is used.  \
    If INPUTFILE is ommited, reading from stdin. \
    If DIRECTORY is ommited, the current directory is used.";
     ((!enc), (!readfile), (!dir)))
  
module Utf8Buffer = UTF8.Buf
  
module Utf8NF = UNF.Make(Camomileconfig)(UTF8)
  
let ff = 0x000c
  
(*form feed*)
let cr = Char.code '\r'
  
let lf = Char.code '\n'
  
let nel = 0x0085
  
let tab = Char.code '\t'
  
let backslash = Char.code '\\'
  
let sq = Char.code '\\'
  
let dq = Char.code '"'
  
let backslash = Str.regexp "\\\\\\\\"
  
let literal_1 =
  Str.regexp "\\\\[u]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"
  
let literal_2 =
  Str.regexp
    "\\\\[v]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"
  
let unescape s =
  let s =
    Str.global_substitute literal_1
      (fun _ ->
         let n = int_of_string (Str.replace_matched "0x\\1" s)
         in UTF8.init 1 (fun _ -> UChar.chr_of_uint n))
      s in
  let s =
    Str.global_substitute literal_2
      (fun _ ->
         let n = int_of_string (Str.replace_matched "0x\\1" s)
         in UTF8.init 1 (fun _ -> UChar.chr_of_uint n))
      s
  in Str.global_replace backslash "\\\\" s
  
let rec stream_to_list_aux a s =
  (fun (__strm : _ Stream.t) ->
     match Stream.peek __strm with
     | Some e -> (Stream.junk __strm; stream_to_list_aux (e :: a) __strm)
     | _ -> List.rev a)
    s
  
let stream_to_list s = stream_to_list_aux [] s
  
type token = | Text of string | Brace_r | Brace_l | Colon | Comma

let rec prep (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some u ->
      (Stream.junk __strm;
       let rest = __strm in
       let c = (try Some (UChar.char_of u) with | _ -> None)
       in
         (match Info.general_category u with
          | `Cc | `Cf when c <> (Some '\n') -> prep rest
          | ct ->
              Stream.lcons (fun _ -> (c, ct, u))
                (Stream.slazy (fun _ -> prep rest))))
  | _ -> Stream.sempty
  
let rec remove_comment (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (((Some '/', _, _) as data)) ->
      (Stream.junk __strm;
       let rest = __strm
       in
         (fun (__strm : _ Stream.t) ->
            match Stream.peek __strm with
            | Some ((Some '/', _, _)) -> (Stream.junk __strm; comment __strm)
            | Some ((Some '*', _, _)) ->
                (Stream.junk __strm; comment2 __strm)
            | _ ->
                let rest = __strm
                in
                  Stream.icons data
                    (Stream.slazy (fun _ -> remove_comment rest)))
           rest)
  | Some (((Some '"', _, _) as data)) ->
      (Stream.junk __strm;
       let rest = __strm
       in Stream.icons data (Stream.slazy (fun _ -> in_quote rest)))
  | Some data ->
      (Stream.junk __strm;
       let rest = __strm
       in Stream.icons data (Stream.slazy (fun _ -> remove_comment rest)))
  | _ -> Stream.sempty
and comment (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some ((Some ('\r' | '\n' | '\133'), _, _) | (_, (`Zl | `Zp), _)) ->
      (Stream.junk __strm; remove_comment __strm)
  | Some data -> (Stream.junk __strm; comment __strm)
  | _ -> Stream.sempty
and comment2 (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (((Some '*', _, _) as data)) ->
      (Stream.junk __strm;
       let rest = __strm
       in
         (fun (__strm : _ Stream.t) ->
            match Stream.peek __strm with
            | Some ((Some '/', _, _)) ->
                (Stream.junk __strm; remove_comment __strm)
            | _ -> comment2 __strm)
           rest)
  | Some data -> (Stream.junk __strm; comment2 __strm)
  | _ -> Stream.sempty
and in_quote (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (((Some '\\', _, _) as data1)) ->
      (Stream.junk __strm;
       (match Stream.peek __strm with
        | Some data2 ->
            (Stream.junk __strm;
             let rest = __strm
             in
               Stream.icons data1
                 (Stream.icons data2 (Stream.slazy (fun _ -> in_quote rest))))
        | _ -> raise (Stream.Error "")))
  | Some (((Some '"', _, _) as data)) ->
      (Stream.junk __strm;
       let rest = __strm
       in Stream.icons data (Stream.slazy (fun _ -> remove_comment rest)))
  | Some data ->
      (Stream.junk __strm;
       let rest = __strm
       in Stream.icons data (Stream.slazy (fun _ -> in_quote rest)))
  | _ -> Stream.sempty
  
let rec merge_text (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (Text s) -> (Stream.junk __strm; do_merge s __strm)
  | Some e ->
      (Stream.junk __strm;
       let rest = __strm
       in Stream.icons e (Stream.slazy (fun _ -> merge_text rest)))
  | _ -> Stream.sempty
and do_merge s (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some (Text s') -> (Stream.junk __strm; do_merge (s ^ s') __strm)
  | Some e ->
      (Stream.junk __strm;
       let rest = __strm
       in
         Stream.icons (Text s)
           (Stream.icons e (Stream.slazy (fun _ -> merge_text rest))))
  | _ -> Stream.sempty
  
let lexer s =
  let rec parse (__strm : _ Stream.t) =
    match Stream.peek __strm with
    | Some ((Some '{', _, _)) ->
        (Stream.junk __strm;
         let rest = __strm
         in Stream.icons Brace_l (Stream.slazy (fun _ -> parse rest)))
    | Some ((Some '}', _, _)) ->
        (Stream.junk __strm;
         let rest = __strm
         in Stream.icons Brace_r (Stream.slazy (fun _ -> parse rest)))
    | Some ((Some ':', _, _)) ->
        (Stream.junk __strm;
         let rest = __strm
         in Stream.icons Colon (Stream.slazy (fun _ -> parse rest)))
    | Some ((Some ',', _, _)) ->
        (Stream.junk __strm;
         let rest = __strm
         in Stream.icons Comma (Stream.slazy (fun _ -> parse rest)))
    | Some ((Some '"', _, _)) -> (Stream.junk __strm; quote __strm)
    | Some
        ((Some ('\r' | '\n' | '\133' | '\t'), _, _) |
           (_, (`Zs | `Zl | `Zp), _))
        -> (Stream.junk __strm; parse __strm)
    | Some e ->
        (Stream.junk __strm; let rest = __strm in text (Stream.icons e rest))
    | _ -> Stream.sempty
  and quote s =
    let buf = Utf8Buffer.create 16 in
    let rec loop (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some ((Some '\\', _, u1)) ->
          (Stream.junk __strm;
           (match Stream.peek __strm with
            | Some ((_, _, u2)) ->
                (Stream.junk __strm;
                 let rest = __strm
                 in
                   (Utf8Buffer.add_char buf u1;
                    Utf8Buffer.add_char buf u2;
                    loop rest))
            | _ -> raise (Stream.Error "")))
      | Some ((Some '"', _, _)) ->
          (Stream.junk __strm;
           let rest = __strm in
           let s = Utf8Buffer.contents buf in
           let s' = unescape s
           in Stream.icons (Text s') (Stream.slazy (fun _ -> parse rest)))
      | Some ((_, _, u)) ->
          (Stream.junk __strm;
           let rest = __strm in (Utf8Buffer.add_char buf u; loop rest))
      | _ -> failwith "A quote is not enclosed."
    in loop s
  and text s =
    let buf = Utf8Buffer.create 16 in
    let rec loop (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some
          ((Some ('\r' | '\n' | '\133' | '\t'), _, _) |
             (_, (`Zs | `Zl | `Zp), _))
          ->
          (Stream.junk __strm;
           let rest = __strm in
           let s = Utf8Buffer.contents buf in
           let s' = unescape s
           in Stream.icons (Text s') (Stream.slazy (fun _ -> parse rest)))
      | Some (((Some ('{' | '}' | ':' | ',' | '"'), _, _) as e)) ->
          (Stream.junk __strm;
           let rest = __strm in
           let s = Utf8Buffer.contents buf in
           let s' = unescape s
           in
             Stream.icons (Text s')
               (Stream.slazy (fun _ -> parse (Stream.icons e rest))))
      | Some ((_, _, u)) ->
          (Stream.junk __strm;
           let rest = __strm in (Utf8Buffer.add_char buf u; loop rest))
      | _ ->
          let s = Utf8Buffer.contents buf in
          let s' = unescape s in Stream.ising (Text s')
    in loop s in
  let p = prep s in
  let p1 = remove_comment p in
  let tokens = parse p1 in
  let tokens1 = merge_text tokens in let l = stream_to_list tokens1 in l
  
let string_to_binary s =
  let n = (String.length s) / 2 in
  let b = String.create n
  in
    (for i = 0 to n - 1 do
       (let d = int_of_string ("0x" ^ (String.sub s (i * 2) 2))
        in b.[i] <- Char.chr d)
     done;
     b)
  
let root = ref ""
  
let load_file filename =
  let file =
    if Filename.is_implicit filename
    then Filename.concat !root filename
    else filename in
  let c = open_in_bin file in
  let buf = Buffer.create 16
  in
    try (while true do Buffer.add_channel buf c 1 done; assert false)
    with | End_of_file -> Buffer.contents buf
  
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
  | Text num :: Comma :: rest ->
      parse_intvect rest ((int_of_string num) :: a)
  | Text num :: rest ->
      ((Intvect (Array.of_list (List.rev ((int_of_string num) :: a)))), rest)
  | _ -> ((Intvect (Array.of_list (List.rev a))), l)
  
let rec parse_table l a =
  match parse l with
  | (Some d, rest) -> parse_table rest (d :: a)
  | (None, rest) ->
      let tbl = Hashtbl.create (List.length a) in
      let proc ent =
        (match ent with
         | Tagged (name, data) -> Hashtbl.add tbl name data
         | _ -> failwith "A broken table entry.")
      in (List.iter proc a; ((Table tbl), rest))
and parse_array l a =
  match l with
  | Brace_l :: rest ->
      let (data, rest) = parse_unknown rest
      in
        (match rest with
         | Brace_r :: Comma :: rest -> parse_array rest (data :: a)
         | Brace_r :: rest -> parse_array rest (data :: a)
         | _ -> failwith "A brace is not enclosed.")
  | Text text :: Comma :: rest -> parse_array rest ((String_data text) :: a)
  | Text text :: rest ->
      ((Array_data (Array.of_list (List.rev ((String_data text) :: a)))),
       rest)
  | _ -> ((Array_data (Array.of_list (List.rev a))), l)
and parse_unknown l =
  match l with
  | Text text :: Brace_r :: rest -> ((String_data text), (Brace_r :: rest))
  | Text text :: Comma :: rest -> parse_array l []
  | Text text :: rest -> parse_table l []
  | _ -> parse_array l []
and parse l =
  match l with
  | Text tname :: Colon :: Text "table" :: Brace_l :: rest ->
      let (data, rest) = parse_table rest []
      in
        (match rest with
         | Brace_r :: rest -> ((Some (Tagged (tname, data))), rest)
         | _ -> failwith "A brace is not enclosed.")
  | Text tname :: Colon :: Text "array" :: Brace_l :: rest ->
      let (data, rest) = parse_array rest []
      in
        (match rest with
         | Brace_r :: rest -> ((Some (Tagged (tname, data))), rest)
         | _ -> failwith "A brace is not enclosed.")
  | Text tname :: Colon :: Text "string" :: Brace_l :: Text data ::
      Brace_r :: rest -> ((Some (Tagged (tname, (String_data data)))), rest)
  | Text tname :: Colon :: Text "bin" :: Brace_l :: Text data :: Brace_r ::
      rest ->
      let b = string_to_binary data
      in ((Some (Tagged (tname, (Binary b)))), rest)
  | Text tname :: Colon :: Text "import" :: Brace_l :: Text filename ::
      Brace_r :: rest ->
      (prerr_endline "Warning : file loading is not supported.";
       ((Some (Tagged (tname, (Binary "")))), rest))
  | Text tname :: Colon :: Text "int" :: Brace_l :: Text num :: Brace_r ::
      rest ->
      let n = int_of_string num in ((Some (Tagged (tname, (Int n)))), rest)
  | Text tname :: Colon :: Text "intvector" :: Brace_l :: rest ->
      let (data, rest) = parse_intvect rest []
      in
        (match rest with
         | Brace_r :: rest -> ((Some (Tagged (tname, data))), rest)
         | _ -> failwith "A brace is not enclosed.")
  | Text name :: Brace_l :: rest ->
      let (data, rest) = parse_unknown rest
      in
        (match rest with
         | Brace_r :: rest -> ((Some (Tagged (name, data))), rest)
         | _ -> failwith "A brace is not enclosed.")
  | _ -> (None, l)
  
let col_parse s =
  let s = Utf8NF.nfd s in
  let lexbuf = Lexing.from_string s in
  let ace_info = ColParser.main ColLexer.token lexbuf in cetbl_of ace_info
  
let localedef =
  function
  | Table tbl ->
      let col_info =
        (try
           Some
             (match Hashtbl.find tbl "CollationElements" with
              | Table tbl ->
                  (match Hashtbl.find tbl "Sequence" with
                   | String_data s -> col_parse s
                   | _ -> assert false)
              | _ -> assert false)
         with | Not_found -> None)
      in { Unidata.col_info = col_info; }
  | _ -> assert false
  
let main () =
  let cs = Stream.of_channel readfile in
  let stream = CE.ustream_of enc cs in
  let lexed = lexer stream in
  let (data, rest) = parse_table lexed []
  in
    (if rest <> [] then failwith "Strange trailing data." else ();
     let proc key entry =
       let locale_info = localedef entry
       in Database.write dir "mar" output_value key locale_info
     in
       match data with
       | Table tbl -> Hashtbl.iter proc tbl
       | _ -> failwith "Broken data.")
  
let _ = main ()
  

