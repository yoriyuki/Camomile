(* $Id: xString.ml,v 1.3 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

module type XStringSig = sig
  type xstring = UChar.t XArray.t

  val get : xstring -> int -> UChar.t
  val set : xstring -> int -> UChar.t -> unit
  val length : xstring -> int

  type index
  val look : xstring -> index -> UChar.t
  val nth : xstring -> int -> index
  val first : xstring -> index
  val last : xstring -> index
  val out_of_range : xstring -> index -> bool
  val next : xstring -> index -> index
  val prev : xstring -> index -> index
  val move : xstring -> index -> int -> index
  val compare_index : xstring -> index -> index -> int

  val make : ?bufsize:int -> int -> UChar.t -> xstring
  val clear : xstring -> unit
  val reset : xstring -> unit
  val copy : xstring -> xstring
  val sub : xstring -> int -> int -> xstring
  val add_char : xstring -> UChar.t -> unit
  val add_text : xstring -> 'a UText.text -> unit
  val add_xstring : xstring -> xstring -> unit
  val shrink : xstring -> int -> unit
  val append : xstring -> xstring -> xstring
  val utext_of : xstring -> UText.t
  val ustring_of : xstring -> UText.ustring
      
  val iter : (UChar.t -> unit) -> xstring -> unit
  val compare : xstring -> xstring -> int
end

module XStringAux : XStringSig = struct
  include XArray
  type xstring = UChar.t XArray.t

  let rec compare_aux i t1 t2 =
    if i >= length t1 then
      if i >= length t2 then 0 else ~-1
    else if i >= length t2 then 1 else
    match UChar.compare (XArray.get t1 i) (XArray.get t2 i) with
      0 -> compare_aux (i + 1) t1 t2
    | sgn -> sgn

  let compare t1 t2 = compare_aux 0 t1 t2
  let add_xstring = add_xarray
  let add_char = add_element
  let add_text b t = UText.iter (add_char b) t
  let ustring_of b = UText.init_ustring (length b) (get b)
  let utext_of b = UText.init (length b) (get b)
end

  include XStringAux
  type text = xstring
  type t = xstring

  let init len f = XArray.init len (UChar.chr_of_uint 0) f

  module Buf =
    struct
      include XStringAux
      type buf = xstring
      let create bufsize = make ~bufsize 0 (UChar.chr_of_uint 0)
      let contents x = x
      let add_string = add_xstring
      let add_buffer = add_xstring
    end
