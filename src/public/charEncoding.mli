(* Copyright (C) 2001, 2002, 2003, Yamagata Yoriyuki *)

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

module type Interface = sig
  (** Module for character encodings. *)
  open OOChannel

  (**Failure of decoding*)
  exception Malformed_code

  (**Failure of encoding*)
  exception Out_of_range

  (** Type for encodings. *)
  type t

  (** [automatic name [enc_1; enc_2; ... enc_n] enc]
      creates the new encoding [name]
      doing automatic encoding detection among [enc_1], [enc_2], ..., [enc_n]
      by the given order.   [enc] is used for encoding. *)
  val automatic : string -> t list -> t -> t

  (** [new_enc name enc] registers the new encoding [enc]
      under the name [name] *)
  val new_enc : string -> t -> unit

  (** [alias alias name] : Define [alias] as an alias of
      the encoding with the name [name]. *)
  val alias : string -> string -> unit

  (** Returns the encoding of the given name.
      Fails if the encoding is unknown.
      Encoding names are the same to codeset names in charmap files for
      the encodings defined by charmap.
      See charmaps directory in the source directory for the available encodings.
      In addition to the encodings via the charmap files, camomile supports
      ISO-2022-CN, ISO-2022-JP, ISO-2022-JP-2, ISO-2022-KR, jauto (Auto
      detection of Japanese encodings), UTF-8, UTF-16, UTF-16BE, UTF-16LE.
      UTF-32, UTF-32BE, UTF-32LE, UCS-4(Big endian order).
      The encoding also can be referred by "IANA/<IANA name>", if the encoding
      is supported. *)
  val of_name : string -> t

  (** Returns the name of the encoding. *)
  val name_of : t -> string

  (** Shortcuts *)

  val ascii : t
  val latin1 : t
  val utf8 : t
  val utf16 : t
  val utf16be : t
  val utf16le : t
  val utf32 : t
  val utf32be : t
  val utf32le : t
  val ucs4 : t

  (** [recode_string ~in_enc ~out_enc s]
      converts the string [s] from [in_enc] to [out_enc]. *)
  val recode_string : in_enc:t -> out_enc:t -> string -> string

  (** [new uchar_input_channel_of enc c_in] creates the new intput
      channel which convert characters to Unicode using encoding
      [enc]. *)
  class uchar_input_channel_of :
    t -> char_input_channel -> [UChar.t] obj_input_channel

  (** [new uchar_ouput_channel_of enc c_out] creates the new output
      channel which convert Unicode to its byte representation using
      encoding [enc]. *)
  class uchar_output_channel_of :
    t -> char_output_channel -> [UChar.t] obj_output_channel

  (** [new convert_uchar_input enc c_in] creates the new channel which
      convert Unicode input to its byte representation using encoding
      [enc]. *)
  class convert_uchar_input :
    t -> UChar.t obj_input_channel -> char_input_channel

  (** [new convert_uchar_output enc c_in] creates the new channel which
      convert character output to Unicode using encoding [enc]. *)
  class convert_uchar_output :
    t -> UChar.t obj_output_channel -> char_output_channel

  (** [new convert_input in_enc out_enc c_in] create the new input
      channel using encoding [out_enc] from the input channel using
      encoding [in_enc] *)
  class convert_input :
    in_enc:t -> out_enc:t -> char_input_channel -> char_input_channel

  (** [new convert_ouput in_enc out_enc c_in] create the new output
      channel using encoding [in_enc] from the output channel using
      encoding [out_enc] *)
  class convert_output :
    in_enc:t -> out_enc:t -> char_output_channel -> char_output_channel

  (** [new out_channel enc outchan] creates the output channel object
      {!OOChannel.obj_output_channel} which
      receives Unicode characters and outputs them to [outchan] using
      the encoding [enc]. *)
  class out_channel : t -> Stdlib.out_channel -> [UChar.t] obj_output_channel

  (** [new in_channel enc inchan] creates the intput channel object
      {!OOChannel.obj_input_channel} which
      reads bytes from [inchan] and converts them to Unicode characters. *)
  class in_channel : t -> Stdlib.in_channel -> [UChar.t] obj_input_channel

  (** [ustream_of enc chars] converts the byte stream [chars]
      to the Unicode character stream by the encoding [enc]. *)
  val ustream_of : t -> char Stream.t -> UChar.t Stream.t

  (** [char_stream_of enc uchars] converts the Unicode character stream
      [uchars] to the byte stream by the encoding [enc] *)
  val char_stream_of : t -> UChar.t Stream.t -> char Stream.t

  module type Type = sig
    type text

    (** [decode enc s] converts the string [s] encoded
        by the encoding [enc] to the Unicode text. *)
    val decode : t -> string -> text

    (** [encode enc t] converts the Unicode text [t] to the string
        by the encoding [enc].*)
    val encode : t -> text -> string
  end

  module Make (Text : UnicodeString.Type) : Type with type text = Text.t
end

module Configure (_ : Config.Type) : Interface
