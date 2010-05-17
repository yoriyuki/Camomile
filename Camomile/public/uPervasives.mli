(* $Id: uPervasives.mli,v 1.1 2004/09/04 16:06:25 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

type uchar = UChar.t

(** Aliases for UChar.uint_code, UChar.chr_of_uint *)

val int_of_uchar : uchar -> int
val uchar_of_int : int -> uchar

val escaped_uchar : uchar -> string
val escaped_utf8 : string -> string

val printer_utf8 : Format.formatter -> string -> unit
val printer_uchar : Format.formatter -> uchar -> unit
