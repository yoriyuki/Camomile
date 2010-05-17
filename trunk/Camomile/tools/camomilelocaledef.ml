(* $Id: camomilelocaledef.ml,v 1.1 2006/08/13 17:21:24 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki *)

open Toolslib
open UCharInfo
open AbsCe

let enc, readfile, dir =
  let enc = ref CharEncoding.utf8 in
  let readfile = ref stdin in
  let dir = ref Filename.current_dir_name in
  Arg.parse
    ["--enc", Arg.String (fun encname ->
      enc := CharEncoding.of_name encname), "Encoding name";
     "--file", Arg.String (fun filename ->
       readfile := open_in_bin filename), "Reading file"]
    (fun dirname -> dir := dirname)
    "camomilelocaledef --enc ENCNAME --file INPUTFILE DIRECTORY:\n\
    Read the localedef INPUTFILE using the encoding ENCNAME \
and put the compiled data into DIRECTORY. \
    If ENCNAME is ommited, UTF-8 is used.  \
    If INPUTFILE is ommited, reading from stdin. \
    If DIRECTORY is ommited, the current directory is used.";
    !enc, !readfile, !dir

module Utf8Buffer = UTF8.Buf
module Utf8NF = UNF.Make (UTF8)

let ff = 0x000c				(*form feed*)
let cr = Char.code '\r'
let lf = Char.code '\n'
let nel = 0x0085
let tab = Char.code '\t'

let backslash = Char.code '\\'
let sq = Char.code '\\'
let dq = Char.code '"'

let backslash = Str.regexp "\\\\\\\\"
let literal_1 = Str.regexp 
    "\\\\[u]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"

let literal_2 = Str.regexp
    "\\\\[v]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"

let unescape s =
  let s = 
    Str.global_substitute literal_1 (fun _ ->
      let n = int_of_string (Str.replace_matched "0x\\1" s) in
      UTF8.init 1 (fun _ -> (UChar.chr_of_uint n))) 
      s in
  let s = 
    Str.global_substitute literal_2 (fun _ ->
      let n = int_of_string (Str.replace_matched "0x\\1" s) in
      UTF8.init 1 (fun _ -> (UChar.chr_of_uint n))) 
      s in
  Str.global_replace backslash "\\\\" s

let rec stream_to_list_aux a s = (parser
    [< 'e; rest >] -> stream_to_list_aux (e :: a) rest
  | [< >] -> List.rev a) s

let stream_to_list s = stream_to_list_aux [] s

type token = 
    Text of string
  | Brace_r
  | Brace_l
  | Colon
  | Comma

let rec prep = parser 
    [< 'u; rest >] ->
      let c = try Some (UChar.char_of u) with _ -> None in
      (match general_category u with
	`Cc | `Cf when c <> Some '\n' ->  prep rest
      | ct -> [< '(c, ct, u); prep rest >])
  | [< >] -> [< >]

let rec remove_comment = parser
    [< '( Some '/', _, _) as data; rest >] ->
      (parser
	  [< '(Some '/', _, _); rest >] -> comment rest
	| [< '(Some '*', _, _); rest >] -> comment2 rest
	| [< rest >] -> [< 'data; remove_comment rest >])
	rest
  | [< '( Some '"', _, _) as data; rest >]  -> 
      [< 'data; in_quote rest >]
  | [< 'data; rest >] -> [< 'data; remove_comment rest >]
  | [< >] -> [< >]
and comment = parser
    [< '( Some ('\r' | '\n' | '\133'), _, _) | ( _, (`Zl | `Zp), _); rest >] 
    -> remove_comment rest
  | [< 'data; rest >] -> comment rest
  | [< >] -> [< >]
and comment2 = parser
    [< '( Some '*', _, _) as data; rest >] -> (parser
	[<  '(Some '/', _, _); rest >] -> remove_comment rest
      |	[< rest >] -> comment2 rest) rest
  | [< 'data; rest >] -> comment2 rest
  | [< >] -> [< >]
and in_quote = parser
    [< '( Some '\\', _, _) as data1; 'data2; rest >] ->
      [< 'data1; 'data2; in_quote rest >]
  | [< '( Some '"', _, _) as data; rest >]  -> 
      [<' data; remove_comment rest >]
  | [< 'data; rest >] -> [< 'data; in_quote rest >]
  | [< >] -> [< >]

let rec merge_text = parser
    [< 'Text s; rest >] -> do_merge s rest
  | [< 'e; rest >] -> [< 'e; merge_text rest >]
  | [< >] -> [< >]
and do_merge s = parser
    [< 'Text s'; rest >] -> do_merge (s ^ s') rest
  | [< 'e; rest >] -> [< 'Text s; 'e; merge_text rest >]
  | [< >] -> [< >]

let lexer s =
  let rec parse = parser
      [< '( Some '{', _, _); rest >] -> [< 'Brace_l; parse rest >]
    | [< '( Some '}', _, _); rest >] -> [< 'Brace_r; parse rest >]
    | [< '( Some ':', _, _); rest >] -> [< 'Colon; parse rest >]
    | [< '( Some ',', _, _); rest >] -> [< 'Comma; parse rest >]
    | [< '( Some '"', _, _); rest >] -> quote rest
    | [< '( Some ('\r' | '\n' | '\133' | '\t'), _, _)
    | ( _, (`Zs | `Zl | `Zp), _) ; rest >] ->
	parse rest
    | [< 'e; rest >] -> text [< 'e; rest >]
    | [< >] -> [< >]
  and quote s =
    let buf = Utf8Buffer.create 16 in
    let rec loop = parser
	[< '( Some '\\', _, u1); '(_, _, u2); rest >] ->
	  Utf8Buffer.add_char buf u1;
	  Utf8Buffer.add_char buf u2;
	  loop rest
      |	[< '( Some '"', _, _); rest >]  -> 
	  let s = Utf8Buffer.contents buf in
	  let s' = unescape s in
	  [< 'Text s'; parse rest >]
      |	[< '( _, _, u); rest >] ->
	  Utf8Buffer.add_char buf u;
	  loop rest
      | [< >] -> failwith "A quote is not enclosed."
    in
    loop s
  and text s =
    let buf = Utf8Buffer.create 16 in
    let rec loop = parser
	[<'( Some ('\r' | '\n' | '\133' | '\t'), _, _) | 
	( _, (`Zs | `Zl | `Zp), _) ; rest >] ->
	  let s = Utf8Buffer.contents buf in
	  let s' = unescape s in
	  [< 'Text s'; parse rest >]
      |	[< '( Some ('{' | '}' | ':' | ','| '"'), _, _) as e; rest >] ->
	  let s = Utf8Buffer.contents buf in
	  let s' = unescape s in
	  [< 'Text s'; parse [< 'e; rest >] >]
      |	[< '( _, _, u); rest >] ->
	  Utf8Buffer.add_char buf u;
	  loop rest
      |	[< >] ->
	  let s = Utf8Buffer.contents buf in
	  let s' = unescape s in
	  [< 'Text s' >]
    in
    loop s
  in
  let p = prep s in
  let p1 = remove_comment p in
  let tokens = parse p1 in
  let tokens1 = merge_text tokens in
  let l = stream_to_list tokens1 in l

let string_to_binary s =
  let n = String.length s / 2 in
  let b = String.create n in
  for i = 0 to n - 1 do
    let d = int_of_string ("0x" ^ (String.sub s (i * 2) 2)) in
    b.[i] <- Char.chr d
  done;
  b

let root = ref ""

let load_file filename =
  let file =
    if Filename.is_implicit filename then
      Filename.concat !root filename else
      filename
  in
  let c = open_in_bin file in
  let buf = Buffer.create 16 in
  try begin while true do
    Buffer.add_channel buf c 1
  done; assert false end
  with End_of_file ->
    Buffer.contents buf

type data = 
    Table of (string, data) Hashtbl.t
  | Array_data of data array
  | String_data of string
  | Binary of string
  | Int of int
  | Intvect of int array
  | Tagged of string * data

let rec parse_intvect l a =
  match l with
    Text num :: Comma :: rest ->
      parse_intvect rest ((int_of_string num) :: a)
  | Text num :: rest ->
      Intvect (Array.of_list (List.rev ((int_of_string num) :: a))), rest
  | _ -> 
      Intvect (Array.of_list (List.rev a)), l

let rec parse_table l a =
  match parse l with
    Some d, rest -> parse_table rest (d :: a)
  | None, rest -> 
      let tbl = Hashtbl.create (List.length a) in
      let proc ent =
	match ent with
	  Tagged (name, data) ->
	    Hashtbl.add tbl name data
	| _ -> failwith "A broken table entry."
      in
      List.iter proc a;
      Table tbl, rest

and parse_array l a =
  match l with
    Brace_l :: rest ->
      let data, rest = parse_unknown rest in
      (match rest with
	Brace_r :: Comma :: rest -> 
	  parse_array rest (data :: a)
      |	Brace_r :: rest -> 
	  parse_array rest (data :: a)
      |	_ -> failwith "A brace is not enclosed.")
  | Text text :: Comma :: rest ->
      parse_array rest ((String_data text) :: a)
  | Text text :: rest ->
      Array_data (Array.of_list (List.rev ((String_data text) :: a))), rest
  | _ ->
      Array_data (Array.of_list (List.rev a)), l

and parse_unknown l =
  match l with
    Text text :: Brace_r :: rest ->
      String_data text, Brace_r :: rest
  | Text text :: Comma :: rest -> parse_array l []
  | Text text :: rest -> parse_table l []
  | _ -> parse_array l []

and parse l = match l with
    Text tname :: Colon :: Text "table" :: Brace_l :: rest ->
      let data, rest = parse_table rest [] in
      (match rest with
	Brace_r :: rest ->
	  Some (Tagged (tname, data)), rest
      |	_ -> failwith "A brace is not enclosed.")
  | Text tname :: Colon :: Text "array" :: Brace_l :: rest ->
      let data, rest = parse_array rest [] in
      (match rest with
	Brace_r :: rest ->
	  Some (Tagged (tname, data)), rest
      |	_ -> failwith "A brace is not enclosed.")
  | Text tname :: Colon :: Text "string" :: Brace_l ::
    Text data :: Brace_r :: rest ->  
      Some (Tagged (tname, String_data data)), rest
  | Text tname :: Colon :: Text "bin" :: Brace_l :: 
    Text data :: Brace_r :: rest ->
      let b = string_to_binary data in
      Some (Tagged (tname, Binary b)), rest
  | Text tname :: Colon :: Text "import" :: Brace_l ::
    Text filename :: Brace_r :: rest ->
      prerr_endline "Warning : file loading is not supported.";
      Some (Tagged (tname, Binary "")), rest
  | Text tname :: Colon :: Text "int" :: Brace_l ::
    Text num :: Brace_r :: rest ->
      let n = int_of_string num in
      Some (Tagged (tname, Int n)), rest
  | Text tname :: Colon :: Text "intvector" :: Brace_l :: rest ->
      let data, rest = parse_intvect rest [] in
      (match rest with
	Brace_r :: rest ->
	  Some (Tagged (tname, data)), rest
      |	_ -> failwith "A brace is not enclosed.")
  | Text name :: Brace_l :: rest ->
      let data, rest = parse_unknown rest in
      (match rest with
	Brace_r :: rest ->
	  Some (Tagged (name, data)), rest
      |	_ -> failwith "A brace is not enclosed.")
  | _ -> None, l

let col_parse s =
  let s = Utf8NF.nfd s in
  let lexbuf = Lexing.from_string s in
  let ace_info = ColParser.main ColLexer.token lexbuf in
  cetbl_of ace_info

let localedef = function Table tbl ->
  let col_info = try
    Some (match Hashtbl.find tbl "CollationElements" with 
      Table tbl ->
	(match Hashtbl.find tbl "Sequence" with 
	  String_data s -> col_parse s
	| _ -> assert false)
    | _ -> assert false)
  with Not_found -> None
  in
  {Unidata.col_info = col_info}
  | _ -> assert false

let main () =
  let cs = Stream.of_channel readfile in
  let stream = CharEncoding.ustream_of enc cs in
  let lexed = lexer stream in
  let data, rest = parse_table lexed [] in
  if rest <> [] then failwith "Strange trailing data.";
  let proc key entry =
    let locale_info = localedef entry in
    let file = Filename.concat dir (key ^ ".mar") in
    let c = open_out_bin file in
    output_value c locale_info
  in
  (match data with
    Table tbl -> Hashtbl.iter proc tbl
  | _ -> failwith "Broken data.")
    
let _ = main ()
