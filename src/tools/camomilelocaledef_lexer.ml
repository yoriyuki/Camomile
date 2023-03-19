open CamomileLib
module Info = UCharInfo.Make (Toolslib.Camomileconfig)
module Utf8Buffer = UTF8.Buf

type token = Text of string | Brace_r | Brace_l | Colon | Comma

let pp fmt = function
  | Brace_r -> Format.fprintf fmt "}"
  | Brace_l -> Format.fprintf fmt "}"
  | Colon -> Format.fprintf fmt ":"
  | Comma -> Format.fprintf fmt ","
  | Text s -> Format.fprintf fmt "\"%s\"" s

let rec stream_to_list_aux a s =
  match Stream.next s with
    | e -> stream_to_list_aux (e :: a) s
    | exception Stream.Failure -> List.rev a

let stream_to_list s = stream_to_list_aux [] s

let literal_1 =
  Str.regexp "\\\\[u]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"

let literal_2 =
  Str.regexp
    "\\\\[v]\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)"

let backslash = Str.regexp "\\\\\\\\"

let unescape s =
  let s =
    Str.global_substitute literal_1
      (fun _ ->
        let n = int_of_string (Str.replace_matched "0x\\1" s) in
        UTF8.init 1 (fun _ -> UChar.chr_of_uint n))
      s
  in
  let s =
    Str.global_substitute literal_2
      (fun _ ->
        let n = int_of_string (Str.replace_matched "0x\\1" s) in
        UTF8.init 1 (fun _ -> UChar.chr_of_uint n))
      s
  in
  Str.global_replace backslash "\\\\" s

let rec prep s =
  match Stream.peek s with
    | None -> Stream.sempty
    | Some u ->
        Stream.junk s;
        let c = try Some (UChar.char_of u) with _ -> None in
        begin
          match Info.general_category u with
            | (`Cc | `Cf) when c <> Some '\n' -> prep s
            | ct -> Stream.icons (c, ct, u) (Stream.slazy (fun () -> prep s))
        end

let rec remove_comment s =
  match Stream.next s with
    | (Some '/', _, _) as data -> begin
        match Stream.next s with
          | Some '/', _, _ -> comment s
          | Some '*', _, _ -> comment2 s
          | _ -> Stream.icons data (remove_comment s)
          | exception Stream.Failure -> Stream.icons data (remove_comment s)
      end
    | (Some '"', _, _) as data -> Stream.icons data (in_quote s)
    | data -> Stream.icons data (remove_comment s)
    | exception Stream.Failure -> Stream.sempty

and comment s =
  match Stream.next s with
    | Some ('\r' | '\n' | '\133'), _, _ | _, (`Zl | `Zp), _ -> remove_comment s
    | _ -> comment s
    | exception Stream.Failure -> Stream.sempty

and comment2 s =
  match Stream.next s with
    | Some '*', _, _ -> begin
        match Stream.next s with
          | Some '/', _, _ -> remove_comment s
          | _ -> comment2 s
          | exception Stream.Failure -> comment2 s
      end
    | _ -> comment2 s
    | exception Stream.Failure -> Stream.sempty

and in_quote s =
  match Stream.npeek 2 s with
    | [((Some '\\', _, _) as data1); data2] ->
        Stream.junk s;
        Stream.junk s;
        Stream.icons data1 (Stream.icons data2 (in_quote s))
    | [((Some '"', _, _) as data); _] | [((Some '"', _, _) as data)] ->
        Stream.junk s;
        Stream.icons data (remove_comment s)
    | _ -> begin
        match Stream.next s with
          | data -> Stream.icons data (in_quote s)
          | exception Stream.Failure -> Stream.sempty
      end

let rec merge_text st =
  match Stream.next st with
    | Text s -> do_merge s st
    | e -> Stream.icons e (merge_text st)
    | exception Stream.Failure -> Stream.sempty

and do_merge s st =
  match Stream.next st with
    | Text s' -> do_merge (s ^ s') st
    | e -> Stream.icons (Text s) (Stream.icons e (merge_text st))
    | exception Stream.Failure -> Stream.sempty

let lexer s =
  let rec parse s =
    match Stream.next s with
      | Some '{', _, _ -> Stream.icons Brace_l (parse s)
      | Some '}', _, _ -> Stream.icons Brace_r (parse s)
      | Some ':', _, _ -> Stream.icons Colon (parse s)
      | Some ',', _, _ -> Stream.icons Comma (parse s)
      | Some '"', _, _ -> quote s
      | Some ('\r' | '\n' | '\133' | '\t'), _, _ | _, (`Zs | `Zl | `Zp), _ ->
          parse s
      | e -> text (Stream.icons e s)
      | exception Stream.Failure -> Stream.sempty
  and quote s =
    let buf = Utf8Buffer.create 16 in
    let rec loop st =
      match Stream.npeek 2 s with
        | [(Some '\\', _, u1); (_, _, u2)] ->
            Stream.junk st;
            Stream.junk st;
            Utf8Buffer.add_char buf u1;
            Utf8Buffer.add_char buf u2;
            loop st
        | _ -> begin
            match Stream.next st with
              | Some '"', _, _ ->
                  let s = Utf8Buffer.contents buf in
                  let s' = unescape s in
                  Stream.icons (Text s') (parse st)
              | _, _, u ->
                  Utf8Buffer.add_char buf u;
                  loop st
              | exception Stream.Failure -> failwith "A quote is not enclosed."
          end
    in
    loop s
  and text s =
    let buf = Utf8Buffer.create 16 in
    let rec loop st =
      match Stream.next st with
        | Some ('\r' | '\n' | '\133' | '\t'), _, _ | _, (`Zs | `Zl | `Zp), _ ->
            let s = Utf8Buffer.contents buf in
            let s' = unescape s in
            Stream.icons (Text s') (parse st)
        | (Some ('{' | '}' | ':' | ',' | '"'), _, _) as e ->
            let s = Utf8Buffer.contents buf in
            let s' = unescape s in
            Stream.icons (Text s') (parse (Stream.icons e st))
        | _, _, u ->
            Utf8Buffer.add_char buf u;
            loop st
        | exception Stream.Failure ->
            let s = Utf8Buffer.contents buf in
            let s' = unescape s in
            Stream.of_list [Text s']
    in
    loop s
  in
  let p = prep s in
  let p1 = remove_comment p in
  let tokens = parse p1 in
  let tokens1 = merge_text tokens in
  let l = stream_to_list tokens1 in
  l
