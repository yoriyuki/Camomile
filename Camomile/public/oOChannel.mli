(** Object Oriented Channel *)
(* Copyright (C) 2002, 2003, 2010 Yamagata Yoriyuki. *)

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
(* yori@users.sourceforge.net *)

(** Generic input channel
  Have the same interface of Polymorphic input channel of
  http://www.ocaml-programming.de/rec/IO-Classes.html
  All channels of Camomile having this interface must confirm
  the behaviour defined in the recommendation above.
*)
class type ['a] obj_input_channel =
  object
    method close_in : unit -> unit
    method get : unit -> 'a
  end

(** Generic output channel
  Have the same interface of Polymorphic output channel of
  http://www.ocaml-programming.de/rec/IO-Classes.html
  All channels of Camomile having this interface must confirm
  the behaviour defined in the recommendation above.
*)
class type ['a] obj_output_channel =
  object

    (** If [close_out] cannot output all buffered objects, flush raises
        [Failure] *)
    method close_out : unit -> unit

    (** If [flush] cannot output all buffered objects, flush raises Failure *)
    method flush : unit -> unit

    method put : 'a -> unit
  end

(** Convert stream to obj_input_channel *)
class ['a] channel_of_stream : 'a Stream.t -> ['a] obj_input_channel

(** Convert obj_input_channel to stream *)
val stream_of_channel : 'a #obj_input_channel -> 'a Stream.t

(** Character(byte) input channel.  Have the same interface of octet
  input channel of http://www.ocaml-programming.de/rec/IO-Classes.html
  All channels of Camomile having this interface must confirm the
  behaviour defined in the recommendation above.  In addition, all
  channels are assumed to be blocking.  If you supply a non-blocking
  channel to Camomile API, the outcome is undefined.
*)
class type char_input_channel =
  object
    method input : Bytes.t -> int -> int -> int
    method close_in : unit -> unit
  end

(** Character(byte) output channel.  Have the same interface of octet
  input channel of http://www.ocaml-programming.de/rec/IO-Classes.html
  All channels of Camomile having this interface must confirm the
  behaviour defined in the recommendation above.  In addition, all
  channels are assumed to be blocking.  If you supply a non-blocking
  channel to Camomile API, the outcome is undefined.
*)
class type char_output_channel =
  object
    method output : Bytes.t -> int -> int -> int
    method flush : unit -> unit
    method close_out : unit -> unit
  end

(** Convert a polymorphic input channel to a character input channel *)
class char_input_channel_of : char #obj_input_channel ->
  char_input_channel

(** Convert a character input channel to a polymorphic input channel*)
class char_obj_input_channel_of : char_input_channel ->
  [char] obj_input_channel

(** Convert a polymorphic output channel to a character output channel *)
class char_output_channel_of : char #obj_output_channel -> char_output_channel

(** Convert a character output channel to a polymorphic output channel *)
class char_obj_output_channel_of : char_output_channel ->
  [char] obj_output_channel

(** Convert an OCaml input channel to an OO-based character input channel *)
class of_in_channel : Pervasives.in_channel -> char_input_channel

(** Convert an OCaml output channel to an OO-based character output channel *)
class of_out_channel : Pervasives.out_channel -> char_output_channel
