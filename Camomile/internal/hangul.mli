(* $Id: hangul.mli,v 1.3 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

val decompose : UChar.t -> UChar.t list
val add_decomposition : XString.t -> UChar.t -> unit
val compose : XString.t -> XString.t -> unit
