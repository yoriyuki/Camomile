(* $Id: oOChannel.mli,v 1.7 2004/11/03 11:21:53 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

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
    (** If close_oout cannot output all buffered objects, flush raises
      Failure *)
    method close_out : unit -> unit
    (** If flush cannot output all buffered objects, flush raises
      Failure *)
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
    method input : string -> int -> int -> int
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
    method output : string -> int -> int -> int
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
