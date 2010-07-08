(* $Id: xString.mli,v 1.2 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** eXtensible Unicode string.  
   The semantics matches the description of UStorage. 
   The detail may be going to change.*)

  type xstring
  type t = xstring

  val get : xstring -> int -> UChar.t
  val set : xstring -> int -> UChar.t -> unit
  val length : xstring -> int
  val init : int -> (int -> UChar.t) -> xstring

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
  val compare : t -> t -> int

  module Buf : sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val length : buf -> int
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end
