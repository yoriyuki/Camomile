(* $Id: uReStrLexer.mll,v 1.4 2003/12/19 17:24:34 yori Exp $
   Copyright 2003 Yamagata Yoriyuki *)

{
open UReStrParser
}

let ascii_char = ['\000'-'\127']

let utf8_char =
  ['\194'-'\223']['\128'-'\191']
| ['\224'-'\239']['\128'-'\191']['\128'-'\191']
| ['\240'-'\247']['\128'-'\191']['\128'-'\191']['\128'-'\191']
| ['\248'-'\251']['\128'-'\191']['\128'-'\191']['\128'-'\191']['\128'-'\191']
| ['\252'-'\253']['\128'-'\191']['\128'-'\191']['\128'-'\191']['\128'-'\191']['\128'-'\191']

let hex_char = ['0'-'9''a'-'f''A'-'F']

rule token = parse
| '.' {DOT}
| '*' {ASTARISK}
| '+' {PLUS}
| '?' {QUESTION}
| '[' {LEFT_BRACKET}
| ']' {RIGHT_BRACKET}
| '-' {MINUS}
| '^' {HAT}
| '$' {DOLLAR}
| "\\|" {ALT}
| "\\(" {LEFT_PAREN}
| "\\)" {RIGHT_PAREN}
| "\\`" {BOS}
| "\\'" {EOS}
| "\\{" ['0'-'9']+ (',' ['0'-'9']*)? "\\}" {
  let s = Lexing.lexeme lexbuf in
  try
    let n = String.index s ',' in
    let s1 = String.sub s 2 (n - 2) in
    let n1 = int_of_string s1 in
    let len2 = String.length s - n - 3 in
    if len2 <= 0 then REPN (n1, None, s) else
    let s2 = String.sub s (n + 1) len2 in
    let m = int_of_string s2 in
    REPN (n1, Some m, s)
  with Not_found ->
    let s1 = String.sub s 2 (String.length s - 4) in
    let n = int_of_string s1 in
    REPN (n, Some n, s)}
| '{' {LEFT_BRACE}
| '}' {RIGHT_BRACE}
| ' ' {SPACE}
| '&' {AND}
| '|'  {OR}
| ':' {COLON}
| "\\u" hex_char hex_char hex_char hex_char 
| "\\U" hex_char hex_char hex_char hex_char hex_char hex_char hex_char hex_char
    {let s = Lexing.lexeme lexbuf in
    let s = "0x" ^ (String.sub s 2 (String.length s - 2)) in
    let n = int_of_string s in
    UCHAR (UChar.chr_of_uint n)}
| '\\' ascii_char {
  let s = Lexing.lexeme lexbuf in
  ASCII s.[0]}
| '\\' utf8_char {
  let s = Lexing.lexeme lexbuf in
  UTF8.validate s;
  UCHAR (UTF8.look s 2)}
| ascii_char {
  let s = Lexing.lexeme lexbuf in
  ASCII s.[0]}
| utf8_char {
  let s = Lexing.lexeme lexbuf in
  UTF8.validate s;
  UCHAR (UTF8.look s 0)}
| eof {END}
