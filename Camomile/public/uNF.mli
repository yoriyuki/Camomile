(** Unicode normal form (NFD, NFKD, NFC, NFKC) as described in UTR #15 *)

(* Copyright (C) 2002 Yamagata Yoriyuki. *)

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

module type Type = sig
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

  module NFCBuf : sig
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
    text -> index -> ([ `Inc of UChar.t list * index * 'a lazy_t ] as 'a)

  val canon_compare : text -> text -> int
  val nfd_decompose : UChar.t -> UChar.t list
  val nfkd_decompose : UChar.t -> UChar.t list
end

module Make (_ : ConfigInt.Type) (Text : UnicodeString.Type) :
  Type with type text = Text.t and type index = Text.index
