(* Lexer for regular expresions *)
(* Copyright (C) 2003 - 2011 Yamagata Yoriyuki *)

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
  ASCII s.[1]}
| '\\' utf8_char {
  let s = Lexing.lexeme lexbuf in
  UTF8.validate s;
  UCHAR (UTF8.look s 1)}
| ascii_char {
  let s = Lexing.lexeme lexbuf in
  ASCII s.[0]}
| utf8_char {
  let s = Lexing.lexeme lexbuf in
  UTF8.validate s;
  UCHAR (UTF8.look s 0)}
| eof {END}
