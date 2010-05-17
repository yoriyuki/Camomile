(* $Id: subText.mli,v 1.2 2004/09/04 16:08:40 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** Sub-texts, parts of original (ur-) texts.  
   The signature and semantics matches those of UStorage. *)
module type Type = sig
  type t

  val get : t -> int -> UChar.t

  val init : int -> (int -> UChar.t) -> t
  val length : t -> int

  type index
  val look : t -> index -> UChar.t
  val nth : t -> int -> index
  val first : t -> index
  val last : t -> index

  val next : t -> index -> index
  val prev : t -> index -> index
  val move : t -> index -> int -> index
  val out_of_range : t -> index -> bool
  val compare_index : t -> index -> index -> int
      
  val iter : (UChar.t -> unit) -> t -> unit
  val compare : t -> t -> int

  module Buf : sig
    type buf
    val create : int -> buf
    val contents : buf -> t
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> t -> unit
    val add_buffer : buf -> buf -> unit
  end      

(** The type of original texts. *)
  type ur_text

(** The type of indexes of original texts. *)
  type ur_index

(** [refer t i j] returns the part of [t] from [i] until [j].
   The character pointed by [j] is not included in the result.
   If [j] is equal to [i] or located before [j], the result is
   an empty string. *)
  val refer : ur_text -> ur_index -> ur_index -> t

(** [excerpt t] copies the contents of [t] as a new ur_text. *)
  val excerpt : t -> ur_text

(** [context t] returns the tuple [(s, i, j)] such that
   [t = refer s i j]. *)
  val context : t -> ur_text * ur_index * ur_index

(** Conversion from indexes of sub-texts to ur_texts. *)
  val ur_index_of : t -> index -> ur_index
end

module Make : functor (Text : UnicodeString.Type) -> 
  (Type with type ur_text = Text.t and type ur_index = Text.index)
