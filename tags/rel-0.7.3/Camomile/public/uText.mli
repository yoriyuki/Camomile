(* $Id: uText.mli,v 1.2 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** An implementation of Unicode string.  
   Internally, it uses integer array.
   The semantics matches the description of UStorage. *)
  
(** Phantom type for distinguishing mutability *)
  type mutability = [ `Mutable | `Immutable ]

  type 'a text
  type utext = [`Immutable] text
  type ustring = [`Mutable] text
  type t = utext

  val utext_of_ustring : ustring -> utext
  val ustring_of_utext : utext -> ustring
  val get :  'a text -> int -> UChar.t

(** [set s i u] sets the [i]-th character in [s] to [u]. *)
  val set :  ustring -> int -> UChar.t -> unit

  type index

  val look : 'a text -> index -> UChar.t
  val nth : 'a text -> int -> index
  val first : 'a text -> index
  val last : 'a text -> index
  val out_of_range : 'a text -> index -> bool
  val compare_index : 'a text -> index -> index -> int
  val next : 'a text -> index -> index
  val prev : 'a text -> index -> index
  val move : 'a text -> index -> int -> index

  val length : 'a text -> int

(** Conversion from Latin-1 strings. *)
  val of_string : string -> utext

  val init : int -> (int -> UChar.t) -> utext
  val init_ustring : int -> (int -> UChar.t) -> ustring

(** The semantics of these function are similar to 
   the equivalents of string. *)
  val make : int -> UChar.t -> ustring
  val copy : ustring -> ustring
  val sub : 'a text -> int -> int -> 'a text
  val fill : ustring -> int -> int -> UChar.t -> unit
  val blit : 'a text -> int -> ustring -> int -> int -> unit
  val append : 'a text -> 'b text -> 'a text
  val iter : (UChar.t -> unit) -> 'a text -> unit
  val compare : 'a text -> 'b text -> int

  module Buf : sig
    type buf

(** [create n] creates the buffer which initially can contain
   [n] Unicode characters. *)
    val create : int -> buf

    val contents : buf -> t
    val contents_string : buf -> ustring
    val length : buf -> int
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> 'a text -> unit
    val add_buffer : buf -> buf -> unit
  end
