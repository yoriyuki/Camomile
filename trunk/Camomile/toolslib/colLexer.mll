(* $Id: colLexer.mll,v 1.6 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki *)

{
open ColParser
let blank = Str.regexp "[ \t\n]+"
}

let utf8_char = 
  ['\000'-'\127'] 
| ['\194'-'\223']['\128'-'\191']
| ['\224'-'\239']['\128'-'\191']['\128'-'\191']
| ['\240'-'\247']['\128'-'\191']['\128'-'\191']['\128'-'\191']
| ['\248'-'\251']['\128'-'\191']['\128'-'\191']['\128'-'\191']['\128'-'\191']
| ['\252'-'\253']['\128'-'\191']['\128'-'\191']['\128'-'\191']['\128'-'\191']['\128'-'\191']

rule token = parse
  eof {(* print_endline "EOF";*) EOF}
| '\'' utf8_char '\'' {
  let s = Lexing.lexeme lexbuf in
  let u = UTF8.look s 0 in
(*  Printf.printf "UCHAR1 \\u%02x\n" (int_of_uchar u); *)
  UCHAR u}
| '[' [^']']* ']' {(* print_endline "OPTION"; *)
  let s = Lexing.lexeme lexbuf in
  let option = String.sub s 1 (String.length s - 2) in
  OPTION (Str.split blank option)}
| '<' {(* print_endline "PRIMARY"; *) PRIMARY}
| "<<" {(* print_endline "SECONDARY";*) SECONDARY}
| ';' {(*print_endline "SECONDARY";*) SECONDARY}
| "<<<" {(*print_endline "TERTIARY";*) TERTIARY}
| ',' {(*print_endline "TERTIARY";*) TERTIARY}
| '=' {(*print_endline "EQ";*) EQ}
| '&' {(*print_endline "RESET";*) RESET}
| '/' {(*print_endline "EXPAND";*) EXPAND}
| '|' {(*print_endline "PREFIX";*) PREFIX}
| [' ' '\t' '\n'] {token lexbuf}
| utf8_char {
  let s = Lexing.lexeme lexbuf in
  let u = UTF8.look s 0 in
(*  Printf.printf "UCHAR2 \\u%04x\n" (int_of_uchar u);*)
  UCHAR u}


