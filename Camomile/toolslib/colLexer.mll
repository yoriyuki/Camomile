(** Lexer for collation rules *)
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

{
open CamomileLibrary
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
