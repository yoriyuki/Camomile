(* $Id: uNF.mli,v 1.2 2006/08/06 19:48:55 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki. distributed with LGPL *)

(** Unicode normal form (NFD, NFKD, NFC, NFKC) as described in UTR #15 *)

module type Type =
sig
  type text

  open OOChannel
  
  class nfd : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel
  class nfc : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel
  class nfkd : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel
  class nfkc : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel

(** Conversion to NFD, NFKD, NFC, NFKC forms. *)

  val nfd : text -> text
  val nfkd : text -> text
  val nfc : text -> text
  val nfkc : text -> text

  module NFCBuf :  sig
    type buf
    val create : int -> buf

    val contents : buf -> text
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> text -> unit
    val add_buffer : buf -> buf -> unit
  end

  val nfc_append : text -> text -> text

(** [put_nfd b t], [put_nfkd b t], [put_nfc b t], [put_nfkc b t]
   clear the contents of [b] and put the NFD, NFKD, NFC, NFKC 
   forms of [t] into [b] respectively. *)

  val put_nfd : XString.t -> text -> unit
  val put_nfkd : XString.t -> text -> unit
  val put_nfc : XString.t -> text -> unit
  val put_nfkc : XString.t -> text -> unit

  type index

  val nfd_inc : 
      text -> index -> 
	([`Inc of UChar.t list * index * 'a lazy_t ] as 'a)

  val canon_compare : text -> text -> int

  val nfd_decompose : UChar.t -> UChar.t list
  val nfkd_decompose : UChar.t -> UChar.t list

end

module Make  (Config : ConfigInt.Type) (Text : UnicodeString.Type) :
  Type with type text = Text.t and type index = Text.index
