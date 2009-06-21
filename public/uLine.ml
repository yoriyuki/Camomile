(* $Id: uLine.ml,v 1.7 2004/11/03 11:21:53 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

open OOChannel

type separator = 
  [ `CR
  | `LF
  | `CRLF
  | `NEL
  | `LS
  | `PS ]

class input (op : separator) (inchan : UChar.t #obj_input_channel) =
  let sp = 
    match op with
      `CR -> [UChar.chr_of_uint 0x000d]
    | `LF -> [UChar.chr_of_uint 0x000a]
    | `CRLF -> [UChar.chr_of_uint 0x000d; UChar.chr_of_uint 0x000a]
    | `NEL -> [UChar.chr_of_uint 0x0085]
    | `LS -> [UChar.chr_of_uint 0x2028]
    | `PS -> [UChar.chr_of_uint 0x2029] in
  let sp_hd = List.hd sp in
  let sp_tl = List.tl sp in
  object (self)
    val mutable wait = false		(*whether the last char is CR*)
    val mutable out_buf = []
    method get() =
      match out_buf with
	u :: rest ->	    
	  out_buf <- rest;
	  u
      | [] ->
	  let u = inchan#get() in
	  if wait then begin
	    wait <- false;
	    match UChar.uint_code u with
	      0x000a ->
		out_buf <- sp_tl;
		sp_hd
	    | 0x000d ->
		wait <- true;
		out_buf <- sp_tl;
		sp_hd
	    | 0x0085 ->
		out_buf <- sp_tl @ sp;
		sp_hd
	    | _ ->
		out_buf <- sp_tl @ [u];
		sp_hd
	  end else
	    match UChar.uint_code u with
	      0x000d ->
		wait <- true;
		self#get()
	    | 0x000a | 0x0085 ->
		out_buf <- sp_tl;
		sp_hd
	    | _ -> u

    method close_in () =
      out_buf <- [];
      inchan#close_in ()
  end

class output (op : separator) (outchan : UChar.t #obj_output_channel) =
  let sp = 
    match op with
      `CR -> [UChar.chr_of_uint 0x000d]
    | `LF -> [UChar.chr_of_uint 0x000a]
    | `CRLF -> [UChar.chr_of_uint 0x000d; UChar.chr_of_uint 0x000a]
    | `NEL -> [UChar.chr_of_uint 0x0085]
    | `LS -> [UChar.chr_of_uint 0x2028]
    | `PS -> [UChar.chr_of_uint 0x2029] in
  object (self)
    val mutable wait = false

    method private output_newline =
      List.iter outchan#put sp

    method put u =
      if wait then begin
	wait <- false;
	match UChar.uint_code u with
	  0x000a -> ()
	| _ -> self#put u
      end else
	match UChar.uint_code u with
	  0x000d -> 
	    self#output_newline;
	    wait <- true
	| 0x000a | 0x0085 | 0x2028 | 0x2029 -> 
	    self#output_newline
	| _ -> outchan#put u

    method close_out () =
      wait <- false;
      outchan#close_out ()

    method flush : unit -> unit = outchan#flush
  end

module type Type = sig

  type text

  class input_line : UChar.t #obj_input_channel -> [text] obj_input_channel

  class output_line : ?sp:[`CR | `CRLF | `LF | `LS | `NEL | `PS] ->
    UChar.t #obj_output_channel -> [text] obj_output_channel

end

module Make (Text : UnicodeString.Type) = struct
  type text = Text.t

  class input_line inchan =
    object (self)
      val b = Text.Buf.create 0
      val mutable wait = false
	  
      method get() =
	Text.Buf.clear b;
	let rec loop () =
	  let x = wait in
	  wait <- false;
	  match UChar.uint_code (inchan#get()) with
	    0x0a ->
	      if x then loop () else ()
	  | 0x0d -> wait <- true
	  | 0x85 | 0x0c | 0x2028 | 0x2029 -> ()
	  | n -> 
	      Text.Buf.add_char b (UChar.chr_of_uint n); 
	      loop () in
	try
	  loop ();
	  Text.Buf.contents b
	with End_of_file ->
	  if Text.length (Text.Buf.contents b) > 0 then
	    Text.Buf.contents b
	  else
	    raise End_of_file

      method close_in () : unit=
	Text.Buf.reset b;
	inchan#close_in ()
    end

  class output_line ?(sp:separator=`LF) outchan =
    let sp = 
      match sp with
	`CR -> [UChar.chr_of_uint 0x000d]
      | `LF -> [UChar.chr_of_uint 0x000a]
      | `CRLF -> [UChar.chr_of_uint 0x000d; UChar.chr_of_uint 0x000a]
      | `NEL -> [UChar.chr_of_uint 0x0085]
      | `LS -> [UChar.chr_of_uint 0x2028]
      | `PS -> [UChar.chr_of_uint 0x2029] in
    object (self)
      method private output_newline =
	List.iter outchan#put sp

      method put t =
	Text.iter outchan#put t;
	self#output_newline

      method flush : unit -> unit = outchan#flush

      method close_out () : unit = outchan#close_out ()
    end

end
