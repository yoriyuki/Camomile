(* $Id: unicodeString.mli,v 1.3 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)
(** Signature for Unicode strings.  
   {!UText}, {!XString}, {!UTF8}, {!UTF16}, {!UCS4}
   have matched signatures to UStorage
   and satisfy the semantics described below.  If users want to supply
   their own Unicode strings, please design the module with the 
   following signature and properties. *)

module type Type = sig
(** The type of string. *)
  type t

(** [get t i] : [i]-th character of the storage.*)
  val get : t -> int -> UChar.t

(** [init len f] creates a new storage.
   the returned storage has length [len], its nth-element is [f n].
   [f] is called with integers [0 ... len - 1], only once for each integer.
   The call is in the increasing order f 0, f 1, f 2, ... *)
  val init : int -> (int -> UChar.t) -> t

(** The number of Unicode characters in the storage *)
  val length : t -> int

(** locations in storages.*)
  type index

(** [look t i] : The character in the location [i] of [t].*)
  val look : t -> index -> UChar.t

(** [nth t n] : the location of the [n]-th character in [t].*)
  val nth : t -> int -> index

(** [next x i, prev x i] :
   The operation is valid if [i] points the valid element, i.e. the
   returned value may point the location beyond valid elements by one.
   If [i] does not point a valid element, the results are unspecified. *)

  val next : t -> index -> index
  val prev : t -> index -> index

(* [out_of_range t i] tests whether [i] is inside of [t]. *)
  val out_of_range : t -> index -> bool
      
  val iter : (UChar.t -> unit) -> t -> unit

(* Code point comparison *)
  val compare : t -> t -> int

(** The location of the first character in the storage. *)
  val first : t -> index

(** The location of the last character in the storage. *)
  val last : t -> index

(** [move t i n] :
   if [n] >= 0, then returns [n]-th character after [i] and
   otherwise returns -[n]-th character before [i].
   If there is no such character, or [i] does not point 
   a valid character, the result is unspecified. *)
  val move : t -> index -> int -> index

(** [compare_index t i j] returns
   a positive integer if [i] is the location placed after [j] in [t],
   0 if [i] and [j] point the same location, and
   a negative integer if [i] is the location placed before [j] in [t]. *)
  val compare_index : t -> index -> index -> int

(** Character buffers.  Similar to Buffer. *)
  module Buf : sig
    type buf

(** [create n] creates the buffer.  [n] is used to determine
   the initial size of the buffer.  The meaning of [n] differs from
   modules to modules. *)
    val create : int -> buf

    val contents : buf -> t
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end
end

