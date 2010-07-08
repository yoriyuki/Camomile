(* $Id: uLine.mli,v 1.4 2004/04/14 18:11:07 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** Line I/O, conversion of line separators. *)
open OOChannel

(** Line separators.  
   - [`CR] specifies carriage return.
   - [`LF] specifies linefeed.
   - [`CRLF] specifies the sequence of carriage return and linefeed.
   - [`NEL] specifies next line (\u0085).
   - [`LS] specifies Unicode line separator (\u2028).
   - [`PS] specifies Unicode paragraph separator (\u2029). *)
type separator =
  [ `CR
  | `LF
  | `CRLF
  | `NEL
  | `LS
  | `PS ]

(** [new input separator input_obj] creates the new input channel object
   {!OOChannel.obj_input_channel} which reads from [input_obj] and
   converts line separators (all of CR, LF, CRLF, NEL, LS, PS) to
   [separator]. *)
class input : separator -> 
  UChar.t #obj_input_channel -> [UChar.t] obj_input_channel

(** [new output separator output_obj] creates the new output channel
   object {!OOChannel.obj_output_channel} which receives Unicode characters 
   and converts line separators (all of CR, LF, CRLF, NEL, LS, PS) to
   [separator]. *)
class output : separator ->
  UChar.t #obj_output_channel -> [UChar.t] obj_output_channel

module type Type = sig

  type text

(** [new input_line input_obj] creates the new input channel object
   {!OOChannel.obj_input_channel} which reads Unicode characters 
   from [input_obj] and output lines.  All of CR, LF, CRLF, NEL, LS, PS, 
   as well as FF (formfeed) are recognised as a line separator. *)
  class input_line : UChar.t #obj_input_channel -> [text] obj_input_channel

(** [new output_line ~sp output_obj] create the new output channel object
   {!OOChannel.obj_output_channel} which output each line to [output_obj]
   using [sp] as a line separator.  
   If [sp] is omitted, linefeed (LF) is used. *)
  class output_line : ?sp:separator ->
      UChar.t #obj_output_channel -> [text] obj_output_channel

end

module Make : functor (Text : UnicodeString.Type) ->
  (Type with type text = Text.t)
