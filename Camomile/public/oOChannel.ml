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


class type ['a] obj_input_channel =
  object
    method get : unit -> 'a
    method close_in : unit -> unit
  end

class type ['a] obj_output_channel =
  object
    method put : 'a -> unit
    method flush : unit -> unit
    method close_out : unit -> unit
  end

class ['a] channel_of_stream s  =
  object
    val s = s
    method get () :'a = 
      try Stream.next s with Stream.Failure -> raise End_of_file
    method close_in () = ()
  end

let stream_of_channel inchan =
  Stream.from (fun _ ->
    try Some (inchan#get()) with End_of_file -> None)

class type char_input_channel =
  object
    method input : Bytes.t -> int -> int -> int
    method close_in : unit -> unit
  end

class type char_output_channel =
  object
    method output : Bytes.t -> int -> int -> int
    method flush : unit -> unit
    method close_out : unit -> unit
  end

class char_input_channel_of (oc : char #obj_input_channel) =
  object
    method close_in () = oc#close_in ()
    method input b pos len =
      let p = ref pos in
	(try while !p < pos + len do
	  Bytes.set b !p (oc#get());
	  incr p;
	 done; () with End_of_file -> ());
	let len = !p - pos in
	  if len <= 0 then raise End_of_file else len
  end

class char_obj_input_channel_of (ic : char_input_channel) =
  let b = Bytes.make 1024 '\000' in
  let pos = ref 0 in
  let len = ref 0 in
  object (self)
    method get () =
      if !pos >= !len then begin
	len := ic#input b 0 1024;
	pos := 0;
	self#get ()
       end else
	 let c = Bytes.get b !pos in
	   incr pos;
	   c
    method close_in () = ic#close_in ()
  end
    
class char_output_channel_of (oc : char #obj_output_channel) =
  object
    method flush = oc#flush
    method close_out = oc#close_out
    method output b p len =
      for i = p to p+len-1 do oc#put (Bytes.get b i) done;
      len
  end

class char_obj_output_channel_of (out : char_output_channel) =
  let b = Bytes.make 1024 '\000' in
  let pos = ref 0 in
  object
    method put c =
      Bytes.set b !pos c;
      incr pos;
      if !pos >= 1024 then 
	let n = out#output b 0 1024 in
	  Bytes.blit b n b 0 (1024 - n);
	  pos := 1024 - n
    method flush () =    
      let n = out#output b 0 !pos in
	if n < !pos then begin
	  Bytes.blit b n b 0 (!pos - n);
	  pos := !pos -n;
	  failwith 
	    "OOChannel.char_output_channel_of#flush: \
             Cannot flush the entire buffer";
	end else
	  pos := 0
    method close_out () = 
      out#flush ();
      out#close_out () 
  end

class of_in_channel p_in =
  object
    method close_in () = close_in p_in
    method input b p len = 
      let len = input p_in b p len in
      if len = 0 then raise End_of_file else len
  end

class of_out_channel p_out =
  object
    method close_out () = close_out p_out
    method output b p len = output p_out b p len; len
    method flush () = flush p_out
  end
