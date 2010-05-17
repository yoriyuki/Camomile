(* $Id: oOChannel.ml,v 1.7 2004/11/03 11:21:53 yori Exp $ *)
(* Copyright 2002, 2003, 2010 Yamagata Yoriyuki. distributed with LGPL *)

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
    method input : string -> int -> int -> int
    method close_in : unit -> unit
  end

class type char_output_channel =
  object
    method output : string -> int -> int -> int
    method flush : unit -> unit
    method close_out : unit -> unit
  end

class char_input_channel_of (oc : char #obj_input_channel) =
  object (self)
    method close_in () = oc#close_in ()
    method input b pos len =
      let p = ref pos in
	(try while !p < pos + len do
	  b.[!p] <- oc#get();
	  incr p;
	 done; () with End_of_file -> ());
	let len = !p - pos in
	  if len <= 0 then raise End_of_file else len
  end

class char_obj_input_channel_of (ic : char_input_channel) =
  let b = String.make 1024 '\000' in
  let pos = ref 0 in
  let len = ref 0 in
  object (self)
    method get () =
      if !pos >= !len then begin
	len := ic#input b 0 1024;
	pos := 0;
	self#get ()
       end else
	 let c = b.[!pos] in
	   incr pos;
	   c
    method close_in () = ic#close_in ()
  end
    
class char_output_channel_of (oc : char #obj_output_channel) =
  object
    method flush = oc#flush
    method close_out = oc#close_out
    method output b p len =
      for i = p to p+len-1 do oc#put b.[i] done;
      len
  end

class char_obj_output_channel_of (out : char_output_channel) =
  let b = String.make 1024 '\000' in
  let pos = ref 0 in
  object
    method put c =
      b.[!pos] <- c;
      incr pos;
      if !pos >= 1024 then 
	let n = out#output b 0 1024 in
	  String.blit b n b 0 (1024 - n);
	  pos := 1024 - n
    method flush () =    
      let n = out#output b 0 !pos in
	if n < !pos then begin
	  String.blit b n b 0 (!pos - n);
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
