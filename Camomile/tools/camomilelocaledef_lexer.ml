module Info = UCharInfo.Make(Camomileconfig)
module Utf8Buffer = UTF8.Buf

type token =
  | Text of string
  | Brace_r
  | Brace_l
  | Colon
  | Comma

let rec stream_to_list_aux a s = (parser
  | [< 'e; rest >] -> stream_to_list_aux (e :: a) rest
  | [< >] -> List.rev a) s
let pp fmt = function
  | Brace_r -> Format.fprintf fmt "}"
  | Brace_l -> Format.fprintf fmt "}"
  | Colon -> Format.fprintf fmt ":"
  | Comma -> Format.fprintf fmt ","
  | Text s -> Format.fprintf fmt "\"%s\"" s
let stream_to_list s = stream_to_list_aux [] s

let backslash = Char.code '\\'
let literal_1 = Str.regexp
    "\\\\[u]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"
let literal_2 = Str.regexp
    "\\\\[v]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"

let backslash = Str.regexp "\\\\\\\\"
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

let rec prep = parser
  | [< 'u; rest >] ->
    let c = try Some (UChar.char_of u) with _ -> None in
    (match Info.general_category u with
    | `Cc | `Cf when c <> Some '\n' ->  prep rest
    | ct -> [< '(c, ct, u); prep rest >])
  | [< >] -> [< >]

let rec remove_comment = parser
  | [< '( Some '/', _, _) as data; rest >] ->
    (parser
    | [< '(Some '/', _, _); rest >] -> comment rest
    | [< '(Some '*', _, _); rest >] -> comment2 rest
    | [< rest >] -> [< 'data; remove_comment rest >]) rest
  | [< '( Some '"', _, _) as data; rest >]  -> [< 'data; in_quote rest >]
  | [< 'data; rest >] -> [< 'data; remove_comment rest >]
  | [< >] -> [< >]
and comment = parser
  | [< '( Some ('\r' | '\n' | '\133'), _, _)
    | ( _, (`Zl | `Zp), _); rest >] -> remove_comment rest
  | [< 'data; rest >] -> comment rest
  | [< >] -> [< >]
and comment2 = parser
  | [< '( Some '*', _, _); rest >] ->
    (parser
    | [< '(Some '/', _, _); rest >] -> remove_comment rest
    |	[< rest >] -> comment2 rest) rest
  | [< 'data; rest >] -> comment2 rest
  | [< >] -> [< >]
and in_quote = parser
  | [< '( Some '\\', _, _) as data1; 'data2; rest >] ->
    [< 'data1; 'data2; in_quote rest >]
  | [< '( Some '"', _, _) as data; rest >]  ->
    [<' data; remove_comment rest >]
  | [< 'data; rest >] -> [< 'data; in_quote rest >]
  | [< >] -> [< >]

let rec merge_text = parser
  | [< 'Text s; rest >] -> do_merge s rest
  | [< 'e; rest >] -> [< 'e; merge_text rest >]
  | [< >] -> [< >]
and do_merge s = parser
  | [< 'Text s'; rest >] -> do_merge (s ^ s') rest
  | [< 'e; rest >] -> [< 'Text s; 'e; merge_text rest >]
  | [< >] -> [< >]

let lexer s =
  let rec parse = parser
    | [< '( Some '{', _, _); rest >] -> [< 'Brace_l; parse rest >]
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
      | [< '( Some '\\', _, u1); '(_, _, u2); rest >] ->
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
      | [< >] -> failwith "A quote is not enclosed." in
    loop s
  and text s =
    let buf = Utf8Buffer.create 16 in
    let rec loop = parser
      | [<'( Some ('\r' | '\n' | '\133' | '\t'), _, _)
      | ( _, (`Zs | `Zl | `Zp), _) ; rest >] ->
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
        [< 'Text s' >] in
    loop s
  in
  let p = prep s in
  let p1 = remove_comment p in
  let tokens = parse p1 in
  let tokens1 = merge_text tokens in
  let l = stream_to_list tokens1 in l
