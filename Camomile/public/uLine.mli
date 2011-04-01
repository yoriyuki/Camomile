(** Line IO *) 
(* Copyright (C) 2003 Yamagata Yoriyuki. distributed with LGPL *)

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
