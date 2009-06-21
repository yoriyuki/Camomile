(* $Id: uPervasives.ml,v 1.1 2004/09/04 16:06:25 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

type uchar = UChar.t

let int_of_uchar u = UChar.uint_code u
let uchar_of_int n = UChar.chr_of_uint n
let validate_utf8 s = UTF8.validate s

let sprint_uchar u =
  let n = UChar.uint_code u in
  let n2 = n land 0xffff in
  let n1 = n lsr 16 in
  if n1 = 0 then 
    Printf.sprintf "\\u%04X" n2
  else
    Printf.sprintf "\\U%04X%04X" n1 n2

let escaped_uchar u =
  let n = int_of_uchar u in
  if n > 0x7f || n < 0 then
    sprint_uchar u
  else
    Char.escaped (Char.chr n)

let backslash = Char.code '\\'
let sq = Char.code '\''
let dq = Char.code '"'
let lf = Char.code '\n'
let cr = Char.code '\r'
let tab = Char.code '\t'
let backspace = Char.code '\b'

let bb =  "\\\\"
let bdq = "\\\""
let bsq =   "\\'"

let escaped_utf8 s =
  let buf = Buffer.create 0 in
  let proc u =
    let n = int_of_uchar u in
    if n > 0x7f || n < 0 then
      Buffer.add_string buf (sprint_uchar u)
    else
      Buffer.add_string buf (String.escaped (String.make 1 (Char.chr n)))
  in
  UTF8.iter proc s;
  Buffer.contents buf

let printer_utf8 f s =
  let b = UTF8.Buf.create 0 in
  UTF8.iter
    (fun u ->
      if UChar.uint_code u = 92  then
	UTF8.Buf.add_string b "\\\\"
      else if UChar.uint_code u < 0x80 then UTF8.Buf.add_char b u
      else
	let s = sprint_uchar u in
	UTF8.Buf.add_string b s)
    s;
  let s = UTF8.Buf.contents b in
    Format.fprintf f "\"%s\"" s

let printer_uchar f u =
  Format.fprintf f "'%s'"
    (if UChar.uint_code u = backslash then "\\\\"
    else if UChar.uint_code u < 0x80 then 
      UTF8.init 1 (fun _ -> u)
    else
      sprint_uchar u)
