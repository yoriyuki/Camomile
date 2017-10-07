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
(* yori@users.sourceforge.net *)


module type Interface = sig
(** Module for character encodings. *)
open OOChannel

exception Malformed_code		(**Failure of decoding*)
exception Out_of_range			(**Failure of encoding*)

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
val recode_string :
   in_enc:t -> out_enc:t -> string -> string

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
class out_channel : t -> Pervasives.out_channel -> [UChar.t] obj_output_channel

(** [new in_channel enc inchan] creates the intput channel object 
   {!OOChannel.obj_input_channel} which
   reads bytes from [inchan] and converts them to Unicode characters. *)
class in_channel : t -> Pervasives.in_channel -> [UChar.t] obj_input_channel

(** [ustream_of enc chars] converts the byte stream [chars] 
   to the Unicode character stream by the encoding [enc]. *)
val ustream_of : t -> char Stream.t -> UChar.t Stream.t

(** [char_stream_of enc uchars] converts the Unicode character stream 
   [uchars] to the byte stream by the encoding [enc] *)
val char_stream_of : t -> UChar.t Stream.t -> char Stream.t

module type Type =
  sig
    type text

   (** [decode enc s] converts the string [s] encoded 
      by the encoding [enc] to the Unicode text. *)
    val decode : t -> string -> text

   (** [encode enc t] converts the Unicode text [t] to the string
      by the encoding [enc].*)
    val encode : t -> text -> string
  end

module Make (Text : UnicodeString.Type) :  (Type with type text = Text.t)
end

module Configure (Config : ConfigInt.Type) = struct
module Unimap = Unimap.Make(Config)
module Charmap = Charmap.Configure(Config)
open OOChannel

let rec comp_sub_aux s1 i1 s2 i2 len =
  if len <= 0 then true else
  if s1.[i1] <> Bytes.get s2 i2 then false else
  comp_sub_aux s1 (i1 + 1) s2 (i2 + 1) (len - 1)

let comp_sub s1 i1 len1 s2 i2 len2 =
  if len1 <> len2 then false else
  comp_sub_aux s1 i1 s2 i2 len1

exception Malformed_code
exception Out_of_range

type 'a machine =
     {read : 'a -> unit;
     term : unit -> unit;}

let feed_string decoder s =
  for i = 0 to (String.length s) - 1 do
    decoder.read s.[i]
  done

type t =
    {name : string;
     make_decoder : UChar.t machine -> char machine;
     make_encoder : char machine -> UChar.t machine}

let null_umachine =
  {read = (fun _ -> ());
   term = (fun () -> ())}

type automatic_state =
    Detection of (t * (char machine)) list
  | Decoding of char machine

let accept_char m c =
  try m.read c; true with Malformed_code -> false

let automatic name encs def =
  let make_decoder um =
    let buf = Buffer.create 0 in
    let state = ref (Detection
	(List.map (fun enc -> (enc, enc.make_decoder null_umachine)) encs))
    in
    let read c =
      match !state with
	Detection candidates ->
	  Buffer.add_char buf c;
	  let cs = List.filter (fun (_, m) -> accept_char m c) candidates in
	  (match cs with
	    [] -> raise Malformed_code
	  | [(enc, _)] ->
	      let m = enc.make_decoder um in
	      feed_string m (Buffer.contents buf);
	      state := Decoding m
	  | _ -> state := Detection cs)
      | Decoding m -> m.read c
    in
    let term () =
      match !state with
	Detection ((enc, _) :: _) ->
	  let m = enc.make_decoder um in
	  feed_string m (Buffer.contents buf);
	  m.term ()
      | Decoding m -> m.term ()
      | _ -> raise Malformed_code
    in
    {read = read; term = term}
  in
  {name = name; make_decoder = make_decoder; make_encoder = def.make_encoder}

let table = Hashtbl.create 0
let holded = ref []

let install name f =
  if Hashtbl.mem table name then 
    failwith (name ^" has been already installed : CharEncoding.install")
  else begin
    Hashtbl.add table name (`P f);
  end

let new_enc name enc = install name (fun () -> enc)

let aliases : (string, string) Hashtbl.t = Hashtbl.create 0

let alias s1 s2 = Hashtbl.add aliases s1 s2

(* Charmap based encodings *)
module Charmap_enc =
  struct
    let of_charmap charmap =
      let make_decoder um =
	let no_char = Charmap.no_char_of charmap.Charmap.enc_to_ucs in
	let start = Charmap.start_probe charmap.Charmap.enc_to_ucs in
	let state = ref start in
	let read c =
	  let i = Char.code c in
	  let n = Charmap.look_probe !state i in
	  if n <> no_char then begin
	    state := start;
	    um.read (UChar.chr_of_uint n)
	  end else			    
	    match Charmap.next_probe !state i with
	      None -> raise Malformed_code
	    | Some next -> state := next in
	let term () = um.term () in
	{read = read; term = term}
      in
      let make_encoder cm =
	let read u =
	  let n = UChar.uint_code u in
	  let enc = Tbl31.get charmap.Charmap.ucs_to_enc n in
	  if enc = "" then raise Out_of_range else
	  feed_string cm enc
	in
	let term () = cm.term () in
	{read = read; term = term}
      in
      {name = charmap.Charmap.name; 
       make_decoder = make_decoder; 
       make_encoder = make_encoder}
  end

let of_charmap = Charmap_enc.of_charmap

let of_name alias = 
  let name = try Hashtbl.find aliases alias with Not_found -> alias in
  let rec look name =
    match Hashtbl.find table name with
      `P f ->
	let enc = f () in
	let b = Weak.create 1 in
	Weak.set b 0 (Some enc);
	Hashtbl.add table name (`W b);
	enc
    | `W b ->
	match Weak.get b 0 with
	  None ->
	    Hashtbl.remove table name;
	    look name
	| Some x -> x in
  try look name with Not_found ->
    let charmap = Charmap.of_name name in
    let enc = of_charmap charmap in
    let b = Weak.create 1 in
    Weak.set b 0 (Some enc);
    Hashtbl.add table name (`W b);
    enc

let name_of enc = enc.name

let buf_machine buf =
  {read = (fun c -> Buffer.add_char buf c); term = fun () -> ()}

let recode_string ~in_enc ~out_enc s =
  let buf = Buffer.create (String.length s) in
  let encoder = out_enc.make_encoder (buf_machine buf) in
  let reader = in_enc.make_decoder encoder in
  feed_string reader s;
  reader.term ();
  Buffer.contents buf

let char_machine_of outchan =
  let b = Bytes.make 1024 '\000' in
  let pos = ref 0 in
  let read c =
    Bytes.set b !pos c;
    incr pos;
    if !pos >= 1024 then 
       let n = outchan#output b 0 1024 in
	 Bytes.blit b n b 0 (1024 - n);
	 pos := 1024 - n
  in
  let flush () =    
    let n = outchan#output b 0 !pos in
      if n < !pos then begin
	pos := !pos - n;
	failwith 
	  "CharEncoding.char_machine_of: \
             Cannot flush the entire buffer";
      end;
      outchan#flush ();
      pos := 0
  in
  let term () = 
    flush ();
    outchan#close_out () 
  in
    {read = read; term = term}, flush

class uchar_output_channel_of 
    enc
    (output : char_output_channel) 
    : [UChar.t] obj_output_channel =
  let c, flush = char_machine_of output in
  let m = enc.make_encoder c in
  object
    method put u = m.read u
    method close_out = m.term
    method flush = flush
  end

class out_channel enc outchan =
  uchar_output_channel_of enc (new OOChannel.of_out_channel outchan)

let queueing q =
  let read u = Queue.add u q in
  let term () = () in
  {read = read; term = term}

class uchar_input_channel_of enc 
    (input : char_input_channel)
    : [UChar.t] obj_input_channel =
  let q = Queue.create () in
  let b = Bytes.make 1024 '\000' in
  object(self : 'a)
    val m = enc.make_decoder (queueing q)
    val mutable term = false
    method get() =
      try Queue.take q with Queue.Empty ->
	if term then raise End_of_file else
	(try 
	   let len = input#input b 0 1024 in
	     for i = 0 to len - 1 do m.read (Bytes.get b i) done
	 with End_of_file ->
	  m.term (); term <- true);
	self#get()
    method close_in =
      Queue.clear q; term <- true; input#close_in
  end

class in_channel enc inchan =
  uchar_input_channel_of enc (new OOChannel.of_in_channel inchan)
  
class in_channel_from_stream enc s =
  uchar_input_channel_of enc 
    (new char_input_channel_of    
       (new channel_of_stream s))

let fill_string s pos len q =
  begin
    try while !pos < Bytes.length s do
      Bytes.set s !pos (Queue.take q);
      incr pos;
    done with Queue.Empty -> ()
  end;
  let read c =
    if !pos < Bytes.length s then begin
      Bytes.set s !pos c;
      incr pos;
    end else
      Queue.add c q
  in
  let term () = () in
    {read = read; term = term}

class convert_uchar_input enc (uinput : UChar.t obj_input_channel)
  : char_input_channel =
  let q = Queue.create () in
  object (self)
    val eof = false
    method input s pos len =
      let p = ref pos in
      let m = enc.make_encoder (fill_string s p len q) in
	try while !p < pos + len do
	  m.read (uinput#get())
	done;
	  !p - pos
	with End_of_file ->
	  if eof then raise End_of_file else !p - pos
    method close_in () =
      uinput#close_in ();
      Queue.clear q;
  end

class convert_uchar_output enc (uoutput : UChar.t obj_output_channel) 
  : char_output_channel =
  let m = enc.make_decoder {read = uoutput#put; term = uoutput#close_out} in
  object (self)
    method output s pos len =
      for i = pos to pos + len - 1 do m.read (Bytes.get s i) done;
      len;
    method flush = uoutput#flush
    method close_out = m.term
  end

class convert_input ~in_enc ~out_enc input =
  convert_uchar_input out_enc (new uchar_input_channel_of in_enc input)

class convert_output ~in_enc ~out_enc output =
  convert_uchar_output in_enc (new uchar_output_channel_of out_enc output)
    
let ustream_of enc s = stream_of_channel (new in_channel_from_stream enc s)

class in_char_channel enc input : [char] obj_input_channel =
  let q = Queue.create () in
  object(self : 'a)
    val m = enc.make_encoder (queueing q)
    val q = q
    val mutable term = false
    method get() =
      try Queue.take q with Queue.Empty ->
	if term then raise End_of_file else
	(try let u = input#get() in m.read u with End_of_file ->
	  m.term (); term <- true);
	self#get()
    method close_in () =
      Queue.clear q; term <- true; input#close_in ()
  end

class in_char_channel_from_ustream enc us =
  in_char_channel enc (new channel_of_stream us)

let char_stream_of enc us = 
  stream_of_channel (new in_char_channel_from_ustream enc us)

module type Type =
  sig
    type text
    val decode : t -> string -> text
    val encode : t -> text -> string
  end

module Make (Text : UnicodeString.Type) =
struct
  type text = Text.t

  let ubuf_machine ubuf =
    {read = (fun u -> Text.Buf.add_char ubuf u); term = fun () -> ()}

  let feed_text encoder s =
    Text.iter encoder.read s

  let decode enc s =
    let buf = Text.Buf.create 0 in
    let m = enc.make_decoder (ubuf_machine buf) in
    feed_string m s;
    m.term ();
    Text.Buf.contents buf

  let encode enc s =
    let buf = Buffer.create (Text.length s) in
    let m = enc.make_encoder (buf_machine buf) in
    feed_text m s ;
    m.term ();
    Buffer.contents buf
end

(* Ascii *)
let _ =
  let make_decoder output_machine =
    let output = output_machine.read in
    let close = output_machine.term in
    let reader c =
      let code = Char.code c in
      if code >= 0x80 then raise Malformed_code else
      let u = UChar.chr_of_uint code in output u
    in 
    let term () = close () in
    {read = reader; term = term}
  in    
  let make_encoder output_machine =
    let output = output_machine.read in
    let close = output_machine.term in
    let reader u =
      let code = UChar.uint_code u in
      if code < 0 || code >= 0x80 then raise Out_of_range else
      output (Char.chr code)
    in
    let term () = close () in
    {read = reader; term = term}
  in
  let enc = fun () ->
    {name = "US-ASCII"; 
     make_decoder = make_decoder;
     make_encoder = make_encoder;}
  in begin
    install "US-ASCII" enc;
    install "USASCII" enc;
    install "ASCII" enc;
    install "ISO646US" enc;
    install "IBM367" enc;
    install "CP367" enc;
    install "ANSI_X3.4-1968" enc;
  end

(* ISO-8859-1, Latin-1 *)
let _ =
  let make_decoder output_machine =
    let output = output_machine.read in
    let close = output_machine.term in
    let reader c =
      let code = Char.code c in
      let u = UChar.chr_of_uint code in output u
    in 
    let term () = close () in
    {read = reader; term = term}
  in    
  let make_encoder output_machine =
    let output = output_machine.read in
    let close = output_machine.term in
    let reader u =
      let c = 
	try (UChar.char_of u) 
	with Invalid_argument _ -> raise Out_of_range
      in
      output c
    in
    let term () = close () in
    {read = reader; term = term}
  in
  let enc = fun () ->
    {name = "Latin-1"; 
     make_decoder = make_decoder;
     make_encoder = make_encoder;}
  in begin
    install "ISO-8859-1" enc;
    install "Latin-1" enc;
  end

(* UTF-8 *)

type utf8_decoder_state =
    {mutable remain : int; mutable cur : int}

let _ =
  let masq = 0b111111 in
  let make_decoder output_machine =
    let output = output_machine.read in
    let close = output_machine.term in
    let state = {remain = 0; cur = 0} in
    let reader c =
      let i = Char.code c in
      if state.remain = 0
      then				(*beginning of char*)
	if i <= 0x7f then output (UChar.chr_of_uint i) else
	if i >= 0xc2 && i <= 0xdf then
	  begin state.remain <- 1; state.cur <- (i - 0xc0) end
	else if i >= 0xe0 && i <= 0xef then
	  begin state.remain <- 2; state.cur <- (i - 0xe0) end
	else if i >= 0xf0 && i <= 0xf7 then
	  begin state.remain <- 3; state.cur <- (i - 0xf0) end 
	else if i >= 0xf8 && i <= 0xfb then
	  begin state.remain <- 4; state.cur <- (i - 0xf8) end
	else if i >= 0xfc && i <= 0xfd then
	  begin state.remain <- 5; state.cur <- (i - 0xfc) end
	else raise Malformed_code
      else
	if i < 0x80 then raise Malformed_code else
	let i' = i - 0x80 in
	let a = (state.cur lsl 6) + i' in
	(* reject unnecessary long encoding *)
	if a = 0 then raise Malformed_code else
	state.remain <- state.remain - 1;
	if state.remain = 0 then output (UChar.chr_of_uint a) else
	state.cur <- a
    in
    let term () = close () in
    {read = reader; term = term}
  in    
  let make_encoder output_machine =
    let output = output_machine.read in
    let close = output_machine.term in
    let reader u =
      let k = UChar.uint_code u in
      if k >= 0 && k <= 0x7f then output (Char.chr k) else
      if k >= 0x80 && k <= 0x7ff then 
	let c0 = Char.chr (0xc0 + (k lsr 6)) in
	let c1 = Char.chr (0x80 + (k land masq)) in
	begin output c0; output c1; end
      else if k >= 0x800 && k <= 0xffff then
	let c0 = Char.chr (0xe0 + (k lsr 12)) in
	let c1 = Char.chr (0x80 + ((k lsr 6) land masq)) in
	let c2 = Char.chr (0x80 + (k land masq)) in
	begin output c0; output c1; output c2 end
      else if k >= 0x10000 && k <= 0x1fffff then
	let c0 = Char.chr (0xf0 + (k lsr 18)) in
	let c1 = Char.chr (0x80 + ((k lsr 12) land masq)) in
	let c2 = Char.chr (0x80 + ((k lsr 6) land masq)) in
	let c3 = Char.chr (0x80 + (k land masq)) in
	begin output c0; output c1; output c2; output c3 end
      else if k >= 0x200000 && k <= 0x3ffffff then
	let c0 = Char.chr (0xf8 + (k lsr 24)) in
	let c1 = Char.chr (0x80 + ((k lsr 18) land masq)) in
	let c2 = Char.chr (0x80 + ((k lsr 12) land masq)) in
	let c3 = Char.chr (0x80 + ((k lsr 6) land masq)) in
	let c4 = Char.chr (0x80 + (k land masq)) in
	begin output c0; output c1; output c2; output c3; output c4 end
      else if k >= 0x4000000 || k < 0 then
	let c0 = Char.chr (0xfc + (k lsr 30)) in
	let c1 = Char.chr (0x80 + ((k lsr 24) land masq)) in
	let c2 = Char.chr (0x80 + ((k lsr 18) land masq)) in
	let c3 = Char.chr (0x80 + ((k lsr 12) land masq)) in
	let c4 = Char.chr (0x80 + ((k lsr 6) land masq)) in
	let c5 = Char.chr (0x80 + (k land masq)) in
	begin 
	  output c0; output c1; output c2; output c3; output c4; output c5
	end
      else raise Out_of_range
    in
    let term () = close () in
    {read = reader; term = term}
  in
  let enc = fun () ->
    {name = "UTF-8"; 
     make_decoder = make_decoder;
     make_encoder = make_encoder;}
  in begin
    install "UTF-8" enc;
  end

(* UTF-16 *)

type endian = BE | LE | Unknown

type ucs2_machine =
    {read2 : int -> unit;
     term2 : unit -> unit}

type ucs2_buf =
    {mutable first_char : bool;
     mutable buf : int}

let char_machine_be m =
  let state = {first_char = true; buf = 0} in
  let read c =
    if state.first_char 
    then begin state.buf <- (Char.code c); state.first_char <- false; end
    else begin
      m.read2 ((state.buf lsl 8) lor (Char.code c));
      state.first_char <- true;
    end
  in
  let term () = m.term2 () in
  {read = read; term = term}

let char_machine_le m =
  let state = {first_char = true; buf = 0} in
  let read c =
    if state.first_char 
    then begin state.buf <- (Char.code c); state.first_char <- false; end
    else begin
      m.read2 (((Char.code c) lsl 8) lor state.buf);
      state.first_char <- true;
    end
  in
  let term () = m.term2 () in
  {read = read; term = term}

let be_outmachine m =
  let read i =
    m.read (Char.chr (i lsr 8));
    m.read (Char.chr (i land 255))
  in {read2 = read; term2 = m.term}

let le_outmachine m =
  let read i =
    m.read (Char.chr (i land 255));
    m.read (Char.chr (i lsr 8))
  in {read2 = read; term2 = m.term}

type utf16_decoder_state =
    {mutable surrogated : bool;
     mutable buf : int}

let _ =
  let make_decoder m =
    let output = m.read in
    let close = m.term in
    let state = {surrogated = false; buf = 0} in
    let read i =
      if state.surrogated 
      then
	if i >= 0xdc00 && i <= 0xdfff then
	  let i' = (state.buf - 0xd800) lsl 10 + (i - 0xdc00) + 0x10000 in
	  begin output (UChar.chr_of_uint i'); state.surrogated <- false end
	else raise Malformed_code
      else
	if i = 0xfeff then () else	(*BOM is ignored*)
	if i <= 0xd7ff || (i >= 0xe000 && i <= 0xfffd) then 
	  output (UChar.chr_of_uint i)
	else if i >= 0xd800 && i <= 0xdbff then
	  begin state.surrogated <- true; state.buf <- i end
	else raise Malformed_code
    in
    let term = close in
    {read2 = read; term2 = term}
  in
  let make_encoder m =
    let output = m.read2 in
    let close = m.term2 in
    output 0xfeff;
    let read u =
      let i = UChar.uint_code u in
      if i >= 0x0 && i < 0xd800 || i >= 0xe000 && i < 0x10000
      then output i
      else if i >= 0x10000 && i <= 0x10ffff then
	let i1 = (i - 0x10000) lsr 10 + 0xd800 in
	let i2 = (i - 0x10000) land 0x3ff + 0xdc00 in
	output i1; output i2
      else raise Out_of_range
    in
    let term = close in
    {read = read; term = term}
  in
  let make_decoder_be m = char_machine_be (make_decoder m) in
  let make_encoder_be m = make_encoder (be_outmachine m) in
  let make_decoder_le m = char_machine_le (make_decoder m) in
  let make_encoder_le m = make_encoder (le_outmachine m) in
  
  let enc_be =
    {name = "UTF-16BE";
     make_decoder = make_decoder_be;
     make_encoder = make_encoder_be;}
  in
  let enc_le =
    {name = "UTF-16LE";
     make_decoder = make_decoder_le;
     make_encoder = make_encoder_le;}
  in

  let enc_unknown = 
    fun () -> automatic "UTF-16" [enc_be; enc_le] enc_be in

  begin
    install "UTF-16BE" (fun () -> enc_be);
    install "UTF-16LE" (fun () -> enc_le);
    install "UTF-16" enc_unknown
  end

(* UCS-4, UTF-32, UTF-32BE, UTF-32LE*)

module UTF32 =
  struct

    type ucs4_machine =
	{read4 : int -> unit;
	  term4 : unit -> unit}

    type ucs4_buf =
	{mutable count : int;
	 mutable buf : int}

    let char_machine_be m =
      let state = {count = 0; buf = 0} in
      let read c =
	let i = Char.code c in
	if state.count = 0 && i > 0x7f then raise Malformed_code else begin
	  state.buf <- (state.buf lsl 8) lor i;
	  state.count <- state.count + 1;
	  if state.count = 4 then begin
	    m.read4 state.buf;
	    state.count <- 0; state.buf <- 0 end
	  else ()
	end
      in
      let term () = m.term4 () in
      {read = read; term = term}

    let char_machine_le m =
      let state = {count = 0; buf = 0} in
      let read c =
	let i = Char.code c in
	if state.count = 3 && i > 0x7f then raise Malformed_code else begin
       	  state.buf <- (i lsl (8 * state.count)) lor state.buf;
	  state.count <- state.count + 1;
	  if state.count = 4 then begin
	    m.read4 state.buf;
	    state.count <- 0; state.buf <- 0 end
	  else ()
	end
      in
      let term () = m.term4 () in
      {read = read; term = term}

    let be_outmachine m =
      let read i =
	m.read (Char.chr (i lsr 24));
	m.read (Char.chr ((i lsr 16) land 255));
	m.read (Char.chr ((i lsr 8) land 255));
	m.read (Char.chr (i land 255))
      in {read4 = read; term4 = m.term}

    let le_outmachine m =
      let read i =
	m.read (Char.chr (i land 255));
	m.read (Char.chr ((i lsr 8) land 255));
	m.read (Char.chr ((i lsr 16) land 255));
	m.read (Char.chr (i lsr 24))
      in {read4 = read; term4 = m.term}

    let make_ucs4_decoder m =
      let read n =
	m.read (UChar.chr_of_uint n)
      in
      let term () = m.term () in
      {read4 = read; term4 = term}

    let make_ucs4_encoder m =
      let read u =
	let n = UChar.uint_code u in
	m.read4 n
      in
      let term () = m.term4 () in
      {read = read; term = term}

    let make_utf32_decoder m =
      let read n =
	if n = 0xfeff then () else	(*BOM is ignored*)
	if n > 0x10ffff || n < 0 || n = 0xfffe then raise Malformed_code else
	m.read (UChar.chr_of_uint n)
      in
      let term () = m.term () in
      {read4 = read; term4 = term}

    let make_utf32_encoder m =
      m.read4 0xfeff;
      let read u =
	let n = UChar.uint_code u in
	if n > 0x10ffff || n < 0 || n = 0xfffe then raise Out_of_range else
	m.read4 n
      in
      let term () = m.term4 () in
      {read = read; term = term}

    let make_utf32_be_decoder m =
      char_machine_be (make_utf32_decoder m)

    let make_utf32_le_decoder m =
      char_machine_le (make_utf32_decoder m)

    let make_utf32_be_encoder m =
      make_utf32_encoder (be_outmachine m)

    let make_utf32_le_encoder m =
      make_utf32_encoder (le_outmachine m)

(* 4-bytes encoding for unicode.  All 31-bits code points are allowed.
 * ISO 10646-1 doesn't specify endianess.  We use big endian ordering
 * without BOM.
 *)
    let ucs4 =
      let make_decoder m = char_machine_be (make_ucs4_decoder m) in
      let make_encoder m = make_ucs4_encoder (be_outmachine m) in
      {name = "UCS-4";
       make_decoder = make_decoder;
       make_encoder = make_encoder}

(* The same as UCS-4, but range is limited to 21-bits code points. *)
    let utf32_be =
      let make_decoder m = make_utf32_be_decoder m in
      let make_encoder m = make_utf32_be_encoder m in
      {name = "UTF-32BE";
       make_decoder = make_decoder;
       make_encoder = make_encoder}

(* Little endian order. *)
    let utf32_le =
      let make_decoder m = make_utf32_le_decoder m in
      let make_encoder m = make_utf32_le_encoder m in
      {name = "UTF-32LE";
       make_decoder = make_decoder;
       make_encoder = make_encoder}

(* UTF-32 with unknown endianness. *)
    let utf32 = fun () ->
      automatic "UTF-32" [utf32_be; utf32_le] utf32_be

    let init () =
      install "UCS-4" (fun () -> ucs4);
      install "UTF-32BE" (fun () -> utf32_be);
      install "UTF-32LE" (fun () -> utf32_le);
      install "UTF-32" utf32

  end

let _ = UTF32.init ()

(* iso-2022-jp *)

let enc_to_ucs map enc =
  let no_char = Unimap.no_char_ucs map in
  let ucs = Unimap.enc_to_ucs map enc in
  if ucs = no_char then raise Malformed_code else ucs

let ucs_to_enc map enc =
  let no_char = Unimap.no_char_enc map in
  let enc = Unimap.ucs_to_enc map enc in
  if enc = no_char then raise Out_of_range else enc

module Iso2022jp =
  struct

    type charset = Ascii | Jisx0208_1978 | Jisx0208_1983 | Jisx0201_roman
  | Jisx0201_kana | Gb2312_1980 | Ksc5601_1987 | Jisx0212_1990 
  | Iso88591 | Iso88597 | Unknown

    let unibyte charset =
      match charset with
	Ascii -> true | Jisx0201_roman -> true | Jisx0201_kana -> true
      | Iso88591 -> true | Iso88597 -> true 
      | Jisx0208_1978 -> false | Jisx0208_1983 -> false |Jisx0212_1990 -> false
      | Gb2312_1980 -> false | Ksc5601_1987 -> false | Unknown -> assert false

    let set94 charset =
      match charset with
	Ascii -> true | Jisx0201_roman -> true | Jisx0201_kana -> true
      | Iso88591 -> false | Iso88597 -> false 
      | Jisx0208_1978 -> true | Jisx0208_1983 -> true |Jisx0212_1990 -> true
      | Gb2312_1980 -> true | Ksc5601_1987 -> true | Unknown -> assert false
	    
    let esc = Char.chr 0x1b			(* \027 *)

    type state_type = 
	{mutable remain : int;		(* <0 means escape sequence*)
	 mutable g0 : charset;
	 mutable g2 : charset;
	 mutable single_shift : bool;
	 buf : Bytes.t}

    let make_enc_2 () =
      let gb2312 = Unimap.of_name "gb2312" in
      let jisx0201 = Unimap.of_name "jisx0201" in
      let jisx0208 = Unimap.of_name "jisx0208" in
      let jisx0212 = Unimap.of_name "jisx0212" in
      let ksc5601 = Unimap.of_name "ksc5601" in
      let iso88597 = Unimap.of_name "iso88597" in

      let make_decoder_2 m =
	let to_uchar_2 charset code =
	  let ucode =
	    match charset with
	      Ascii -> code
	    | Jisx0208_1978 -> enc_to_ucs jisx0208 code
	    | Jisx0208_1983 -> enc_to_ucs jisx0208 code
	    | Jisx0201_roman -> enc_to_ucs jisx0201 code
	    | Jisx0201_kana ->  enc_to_ucs jisx0201 (code + 0x80)
	    | Gb2312_1980 -> enc_to_ucs gb2312 code
	    | Ksc5601_1987 -> enc_to_ucs ksc5601 code
	    | Jisx0212_1990 -> enc_to_ucs jisx0212 code
	    | Iso88591 -> code + 0x80
	    | Iso88597 -> enc_to_ucs iso88597 (code + 0x80)
	    | Unknown -> raise Malformed_code
	  in UChar.chr_of_uint ucode
	in
	let state = 
	  {remain = 0; 
	   g0 = Ascii; 
	   g2 = Unknown; 
	   single_shift = false;
	   buf = Bytes.create 3}
	in
	let reader c =
	  let i = Char.code c in
	  if i >= 0x80 then raise Malformed_code else
	  if state.single_shift 
	  then 
	    if i >= 0x20 && i <= 0x7f
	    then
	      begin
		m.read (to_uchar_2 state.g2 i); 
		state.single_shift <- false 
	      end
	    else raise Malformed_code
	  else 
	    match state.remain with
	      0 ->
		(* escape sequence?. *)
		if c = esc then state.remain <- -1 else
		(* control code or white space, backspace *)
		if i <= 0x20 || i = 0x7f then m.read (UChar.chr_of_uint i) else
		if unibyte state.g0
		then m.read (to_uchar_2 state.g0 i)
		else begin state.remain <- 1; Bytes.set state.buf 0 c end
	    | 1 ->
		let i1 = Char.code (Bytes.get state.buf 0) in
		let i2 = Char.code c in
		if i2 >= 0x21 && i2 <= 0x7e 
		then 
		  let u = to_uchar_2 state.g0 ((i1 lsl 8) + i2) in
		  m.read u
		else raise Malformed_code;
		state.remain <- 0
	    | _ ->			(*escape seq.*)
		Bytes.set state.buf (~-(state.remain) - 1) c;
		state.remain <- state.remain - 1;
		let i = Char.code c in
		let len = ~- (state.remain) - 1 in
		if i >= 0x20 && i <= 0x2f then
		  if len >= 3 then raise Malformed_code else ()
		else if i >= 0x30 && i <= 0x7e then begin
		  state.remain <- 0;
		  if comp_sub "(B" 0 2 state.buf 0 len then
		    state.g0 <- Ascii
		  else if comp_sub "(I" 0 2 state.buf 0 len then
		    state.g0 <- Jisx0201_kana
		  else if comp_sub "(J" 0 2 state.buf 0 len then
		    state.g0 <- Jisx0201_roman
		  else if comp_sub "$@" 0 2 state.buf 0 len then
		    state.g0 <- Jisx0208_1978
		  else if comp_sub "$A" 0 2 state.buf 0 len then
		    state.g0 <- Gb2312_1980
		  else if comp_sub "$B" 0 2 state.buf 0 len then
		    state.g0 <- Jisx0208_1983
		  else if comp_sub "$(C" 0 3 state.buf 0 len then
		    state.g0 <- Ksc5601_1987
		  else if comp_sub "$(D" 0 3 state.buf 0 len then
		    state.g0 <- Jisx0212_1990
		  else if comp_sub ".A" 0 2 state.buf 0 len then
		    state.g2 <- Iso88591
		  else if comp_sub ".F" 0 2 state.buf 0 len then
		    state.g2 <- Iso88597
		  else if comp_sub "N" 0 1 state.buf 0 len then
		    state.single_shift <- true
		  else raise Malformed_code;
		end else raise Malformed_code
	in
	{read = reader; term = m.term} in
      
      let make_encoder_2 m =
	let from_uchar_2 u =
	  let n = UChar.code u in
	  (* the order of tests is *significant* *)
	  if n >= 0x00 && n <= 0x7f then Ascii, n else
	  if n >= 0xa0 && n <= 0xff then Iso88591, (n - 0x80) else
	  let enc = Unimap.ucs_to_enc jisx0208 n in
	  if enc <> Unimap.no_char_enc jisx0208 then (Jisx0208_1983, enc) else
	  let enc = Unimap.ucs_to_enc jisx0212 n in
	  if enc <> Unimap.no_char_enc jisx0212 then (Jisx0212_1990, enc) else
	  let enc = Unimap.ucs_to_enc gb2312 n in
	  if enc <> Unimap.no_char_enc gb2312 then (Gb2312_1980, enc) else
	  let enc = Unimap.ucs_to_enc ksc5601 n in
	  if enc <> Unimap.no_char_enc ksc5601 then (Ksc5601_1987, enc) else
	  let enc = Unimap.ucs_to_enc iso88597 n in
	  if enc <> Unimap.no_char_enc iso88597 then (Iso88597, enc - 0x80) else
	  let n' = Unimap.ucs_to_enc jisx0201 n in
	  if n' = Unimap.no_char_enc jisx0201 then raise Out_of_range else
	  if n' >= 0x80 then Jisx0201_kana, (n' - 0x80) else Jisx0201_roman, n'
	in	  
	(*fields other than g0, g2 are not used*)
	let state =
	  {remain = 0; 
	   g0 = Ascii; 
	   g2 = Unknown; 
	   single_shift = false;
	   buf = Bytes.create 1}
	in
	let reader u =
	  let cs, i = from_uchar_2 u in
	  let c1 = Char.chr (i lsr 8) in
	  let c2 = Char.chr (i land 255) in
	  if c1 = esc || c2 = esc then raise Out_of_range else
	  if set94 cs
	  then
	    if state.g0 = cs
	    then
	      if unibyte cs
	      then m.read c2
	      else begin m.read c1; m.read c2 end
	    else
	      (match cs with
		Ascii ->
		  state.g0 <- Ascii;
		  m.read esc; m.read '('; m.read 'B'; m.read c2
	      |	Jisx0201_kana ->
		  state.g0 <- Jisx0201_kana;
		  m.read esc; m.read '('; m.read 'I'; m.read c2
	      |	Jisx0201_roman ->
		  state.g0 <- Jisx0201_roman;
		  m.read esc; m.read '('; m.read 'J'; m.read c2
	      |	Gb2312_1980 ->
		  state.g0 <- Gb2312_1980;
		  m.read esc; m.read '$'; m.read 'A'; m.read c1; m.read c2
	      |	Jisx0208_1983 ->
		  state.g0 <- Jisx0208_1983;
		  m.read esc; m.read '$'; m.read 'B'; m.read c1; m.read c2
	      |	Ksc5601_1987 ->
		  state.g0 <- Ksc5601_1987;
		  m.read esc; m.read '$'; m.read '('; m.read 'C';
		  m.read c1; m.read c2
	      |	Jisx0212_1990 ->
		  state.g0 <- Jisx0212_1990;
		  m.read esc; m.read '$'; m.read '('; m.read 'D';
		  m.read c1; m.read c2
	      |	_ -> assert false)
	  else				(*set96*)
	    (* since RFC allows G2 is cleared in the begining of lines, 
	     * we set G2 every times. *)
	    (match cs with
	      Iso88591 ->
		m.read esc; m.read '.'; m.read 'A'; 
		m.read esc; m.read 'N'; m.read c2
	    | Iso88597 ->
		m.read esc; m.read '.'; m.read 'F'; 
		m.read esc; m.read 'N'; m.read c2
	    | _ -> assert false)
	in
	let term () =
	  if state.g0 = Ascii then () else
	  begin m.read esc; m.read '('; m.read 'B' end;
	  m.term ()
	in
	{read = reader; term = term} in	

      {name = "ISO-2022-JP-2";
       make_decoder = make_decoder_2;
       make_encoder = make_encoder_2}

    let make_enc () =
      let jisx0201 = Unimap.of_name "jisx0201" in
      let jisx0208 = Unimap.of_name "jisx0208" in

      let make_decoder m =
	let to_uchar charset code =
	  let ucode =
	    match charset with
	      Ascii -> code
	    | Jisx0208_1978 -> enc_to_ucs jisx0208 code
	    | Jisx0208_1983 -> enc_to_ucs jisx0208 code
	    | Jisx0201_roman -> enc_to_ucs jisx0201 code
	    | _ -> raise Malformed_code
	  in UChar.chr_of_uint ucode
	in
	let state = 
	  {remain = 0; 
	   g0 = Ascii; 
	   g2 = Unknown; 
	   single_shift = false;
	   buf = Bytes.create 3}
	in
	let reader c =
	  let i = Char.code c in
	  if i >= 0x80 then raise Malformed_code else
	  match state.remain with
	    0 ->
	      (* escape sequence?. *)
	      if c = esc then state.remain <- -1 else
	      (* control code or white space, backspace *)
	      if i <= 0x20 || i = 0x7f then m.read (UChar.chr_of_uint i) else
	      if unibyte state.g0
	      then m.read (to_uchar state.g0 i)
	      else begin state.remain <- 1; Bytes.set state.buf 0 c end
	  | 1 ->
	      let i1 = Char.code (Bytes.get state.buf 0) in
	      let i2 = Char.code c in
	      if i2 >= 0x21 && i2 <= 0x7e 
	      then 
		let u = to_uchar state.g0 ((i1 lsl 8) + i2) in
		m.read u
	      else raise Malformed_code;
	      state.remain <- 0
	  | _ ->			(*escape seq.*)
	      Bytes.set state.buf (~-(state.remain) - 1) c;
	      state.remain <- state.remain - 1;
	      let len = ~- (state.remain) - 1 in
	      if i >= 0x20 && i <= 0x2f 
	      then
		begin if len >= 3 then raise Malformed_code else () end
	      else if i >= 0x30 && i <= 0x7e then 
		begin
		  state.remain <- 0;
		  if comp_sub "(B" 0 2 state.buf 0 len then
		    state.g0 <- Ascii
		  else if comp_sub "(J" 0 2 state.buf 0 len then
		    state.g0 <- Jisx0201_roman
		  else if comp_sub "$@" 0 2 state.buf 0 len then
		    state.g0 <- Jisx0208_1978
		  else if comp_sub "$B" 0 2 state.buf 0 len then
		    state.g0 <- Jisx0208_1983
		  else raise Malformed_code
		end
	      else raise Malformed_code;
	in
	{read = reader; term = m.term} in

      let make_encoder m =
	let from_uchar u =
	  let n = UChar.code u in
	  (* the order of tests is *significant* *)
	  if n >= 0x00 && n <= 0x7f then Ascii, n else
	  let enc = Unimap.ucs_to_enc jisx0208 n in
	  if enc <> Unimap.no_char_enc jisx0208 then (Jisx0208_1983, enc) else
	  let n' = Unimap.ucs_to_enc jisx0201 n in
	  if n' = Unimap.no_char_enc jisx0201 then raise Out_of_range else
	  if n' >= 0x80 then raise Out_of_range else Jisx0201_roman, n'
	in	  
	(*fields other than g0, g2 are not used*)
	let state =
	  {remain = 0; 
	   g0 = Ascii; 
	   g2 = Unknown; 
	   single_shift = false;
	   buf = Bytes.create 1}
	in
	let reader u =
	  if UChar.uint_code u = 27 then raise Out_of_range else
	  let cs, i = from_uchar u in
	  let c1 = Char.chr (i lsr 8) in
	  let c2 = Char.chr (i land 255) in
	  if state.g0 = cs
	  then
	    if unibyte cs
	    then m.read c2
	    else begin m.read c1; m.read c2 end
	  else
	    (match cs with
	      Ascii ->
		state.g0 <- Ascii;
		m.read esc; m.read '('; m.read 'B'; m.read c2
	    | Jisx0201_roman ->
		state.g0 <- Jisx0201_roman;
		m.read esc; m.read '('; m.read 'J'; m.read c2
	    | Jisx0208_1983 ->
		state.g0 <- Jisx0208_1983;
		m.read esc; m.read '$'; m.read 'B'; m.read c1; m.read c2
	    |	_ -> assert false)
	in
	let term () =
	  if state.g0 = Ascii then () else
	  begin m.read esc; m.read '('; m.read 'B' end;
	  m.term ()
	in
	{read = reader; term = term} in

      {name = "ISO-2022-JP";
       make_decoder = make_decoder;
       make_encoder = make_encoder}

let init () = 
  install "ISO-2022-JP-2" make_enc_2;
  install "csISO2022JP2" make_enc_2;
  install "ISO-2022-JP" make_enc;
  install "csISO2022JP" make_enc
end

let _ = Iso2022jp.init ()

(* Japanese auto detect *)

module J_auto =
  struct
	
    let jauto =
      fun () ->
	let euc_jp = of_name "EUC-JP" in
	let sjis = of_name "SHIFT_JIS" in
	let iso2022jp = of_name "ISO-2022-JP-2" in
	automatic 
	  "japanese_auto_detection" 
	  [iso2022jp; euc_jp; sjis] 
	  euc_jp

    let init () =
      let _ = install "japanese_auto_detection" jauto in
      let _ = install "jauto" jauto in ()
	
  end

let _ = J_auto.init ()

(* iso-2022-kr *)

module Iso2022kr =
  struct
    
    let esc = Char.chr 0x1b			(* \027 *)
    let si = Char.chr 0x0f
    let so = Char.chr 0x0e

    type charset = Ascii | Ksc5601

    type state_type = 
	{mutable remain : int;		(* <0 means escape sequence*)
	 mutable gl : charset;
	 buf : Bytes.t}

    let make_enc () =
      let ksc5601 = Unimap.of_name "ksc5601" in

      let make_decoder m =
	let state = 
	  {remain = 0; 
	   gl = Ascii; 
	   buf = Bytes.create 3}
	in
	let reader c =
	  let i = Char.code c in
	  if i >= 0x80 then raise Malformed_code else
	  match state.remain with
	    0 ->
	      if c = esc then state.remain <- -1 else
	      if c = si then state.gl <- Ascii else
	      if c = so then state.gl <- Ksc5601 else
	      (* control code or white space, backspace *)
	      if i <= 0x20 || i = 0x7f then m.read (UChar.chr_of_uint i) else
	      (match state.gl with
		Ascii -> m.read (UChar.chr_of_uint i)
	      | Ksc5601 -> state.remain <- 1; Bytes.set state.buf 0 c)
	  | 1 ->
	      let i1 = Char.code (Bytes.get state.buf 0) in
	      let i2 = Char.code c in
	      if i2 >= 0x21 && i2 <= 0x7e 
	      then 
		let n = enc_to_ucs ksc5601 ((i1 lsl 8) + i2) in
		m.read (UChar.chr_of_uint n)
	      else raise Malformed_code;
	      state.remain <- 0
	  | _ ->				(*escape seq.*)
	      Bytes.set state.buf (~-(state.remain) - 1) c;
	      state.remain <- state.remain - 1;
	      let i = Char.code c in
	      let len = ~- (state.remain) - 1 in
	      if i >= 0x20 && i <= 0x2f then
		if len >= 3 then raise Malformed_code else ()
	      else if i >= 0x30 && i <= 0x7e then 
		begin
		  state.remain <- 0;
		  (* designator *)
		  if comp_sub "$)C" 0 3 state.buf 0 len then () 
		  else raise Malformed_code
		end
	      else raise Malformed_code
	in
	{read = reader; term = m.term} in
      
      let make_encoder m =
	(*fields other than gl are not used*)
	let state =
	  {remain = 0; 
	   gl = Ascii;
	   buf = Bytes.create 1}
	in
	feed_string m "\027$)C";		(*designator*)
	let reader u =
	  let n = UChar.code u in
	  if n <= 0x7f then
	    if n = 0x0e || n = 0x0f || n = 0x1b then raise Out_of_range else
	    match state.gl with
	      Ascii -> m.read (Char.chr n)
	    | Ksc5601 -> 
		state.gl <- Ascii;
		m.read si; m.read (Char.chr n)
	  else
	    let i = Unimap.ucs_to_enc ksc5601 n in
	    if i = Unimap.no_char_enc ksc5601 then raise Out_of_range else
	    let c1 = Char.chr (i lsr 8) in
	    let c2 = Char.chr (i land 255) in
	    match state.gl with
	      Ascii ->
		state.gl <- Ksc5601;
		m.read so; m.read c1; m.read c2
	    |	Ksc5601 ->
		m.read c1; m.read c2
	in
	let term () =
	  (if state.gl = Ksc5601 then m.read si else ());
	  m.term ()
	in
	{read = reader; term = term} in
      
      {name = "ISO-2022-KR";
       make_decoder = make_decoder;
       make_encoder = make_encoder}

let init () = 
let _ = install "ISO-2022-KR" make_enc in 
let _ = install "csISO2022KR" make_enc  in ()
end

let _ = Iso2022kr.init ()

(* iso-2022-cn *)

module Iso2022cn =
  struct
    
    let esc = Char.chr 0x1b			(* \027 *)
    let si = Char.chr 0x0f
    let so = Char.chr 0x0e
    let uLF = UChar.chr_of_uint (Char.code '\n')

    let cns_p1 = 0x10000
    let cns_p2 = 0x20000

    type charset = Ascii | GB_2312 | CNS_11643_p1 | CNS_11643_p2 | Unknown

    type assign = G0 | G1 | G2

    type state_type = 
	{mutable remain : int;		(* <0 means escape sequence*)
	 mutable gl : assign;
	 mutable g1 : charset;
	 mutable g2 : charset;
	 mutable single_shift : bool;
	 buf : Bytes.t}

    let make_enc () =
      let gb2312 = Unimap.of_name "gb2312" in
      let cns11643 = Unimap.of_name "cns11643" in

      let make_decoder m =
	let to_ucs4 cs n =
	  match cs with
	    Ascii -> if n <= 0x7f then n else raise Malformed_code
	  | GB_2312 -> enc_to_ucs gb2312 n
	  | CNS_11643_p1 -> enc_to_ucs cns11643 (cns_p1 lor n)
	  | CNS_11643_p2 -> enc_to_ucs cns11643 (cns_p2 lor n)
	  | Unknown -> raise Malformed_code
	in
	let state = 
	  {remain = 0; 
	   gl = G0;
	   g1 = Unknown;
	   g2 = Unknown;
	   single_shift = false;
	   buf = Bytes.create 3}
	in
	let reader c =
	  let i = Char.code c in
	  if i >= 0x80 then raise Malformed_code else
	  match state.remain with
	    0 ->
	      if not state.single_shift then
		match c with
		  '\027' -> state.remain <- ~-1
		|	'\015' -> state.gl <- G0
		|	'\014' -> state.gl <- G1
		|	_  ->
		    (* control code *)
		    if i <= 0x20 then m.read (UChar.chr_of_uint i) else
		    (match state.gl with
		      G0 -> m.read (UChar.chr_of_uint i)
		    | _ -> state.remain <- 1; Bytes.set state.buf 0 c)
	      else
		state.remain <- 1; Bytes.set state.buf 0 c
	  | 1 ->
	      let i1 = Char.code (Bytes.get state.buf 0) in
	      let i2 = Char.code c in
	      let n = (i1 lsl 8) + i2 in
	      let cs =
		if state.single_shift then state.g2 else
		match state.gl with
		  G0 -> Ascii
		|	G1 -> state.g1
		|	G2 -> state.g2
	      in
	      m.read (UChar.chr_of_uint (to_ucs4 cs n));
	      state.remain <- 0;
	      if state.single_shift then state.single_shift <- false else ()
	  | 2 -> state.remain <- 1; Bytes.set state.buf 0 c
	  | _ ->				(*escape seq.*)
     Bytes.set state.buf (~-(state.remain) - 1) c;
	      state.remain <- state.remain - 1;
	      let i = Char.code c in
	      let len = ~- (state.remain) - 1 in
	      if i >= 0x20 && i <= 0x2f then
		if len >= 3 then raise Malformed_code else ()
	      else if i >= 0x30 && i <= 0x7e then
		begin
		  state.remain <- 0;
		  if comp_sub "$)A" 0 3 state.buf 0 len then
		    state.g1 <- GB_2312
		  else if comp_sub "$)G" 0 3 state.buf 0 len then
		    state.g1 <- CNS_11643_p1
		  else if comp_sub "$*H" 0 3 state.buf 0 len then
		    state.g2 <- CNS_11643_p2
		  else if comp_sub "N"  0 1 state.buf 0 len then
		    begin state.single_shift <- true; state.remain <- 2 end
		  else raise Malformed_code
		end
	      else raise Malformed_code
	in
	{read = reader; term = m.term} in
      
      let make_encoder m =
	let from_ucs4 n =
	  if n <= 0x7f then Ascii, n else
	  let n' = Unimap.ucs_to_enc gb2312 n in
	  if n' <> Unimap.no_char_enc gb2312 then (GB_2312, n') else
	  let n' = Unimap.ucs_to_enc cns11643 n in
	  if n' = Unimap.no_char_enc cns11643 then raise Out_of_range else
	  match n' lsr 16 with
	    1 -> CNS_11643_p1, (n' land 0xffff)
	  | 2 -> CNS_11643_p2, (n' land 0xffff)
	  | _ -> raise Out_of_range
	in
	(*fields other than gl, g1, g2 are not used*)
	let state =
	  {remain = 0; 
	   gl = G0;
	   g1 = Unknown;
	   g2 = Unknown;
	   buf = Bytes.create 1;
	   single_shift = false;
	 }
	in
	let reader u =
	  let n = UChar.code u in
	  if n = 0x0e || n = 0x0f || n = 0x1b then raise Out_of_range else begin
	    if n = 0x0d || n = 0x0a || n = 0x85 then begin
	      state.g1 <- Unknown;
	      state.g2 <- Unknown;
	    end else ();
	    let cs, n' = from_ucs4 n in
	    let i1 = n' lsr 8 in
	    let i2 = n' land 255 in
	    let cur_cs =
	      match state.gl with
		G0 -> Ascii
	      | G1 -> state.g1
	      | G2 -> state.g2
	    in
	    if cs = cur_cs then
	      match cs with
		Ascii -> m.read (Char.chr i2)
	      | _ -> m.read (Char.chr i1); m.read (Char.chr i2)
	    else
	      match cs with
		Ascii ->
		  m.read si;
		  m.read (Char.chr i2);
		  state.gl <- G0
	      | GB_2312 ->
		  if not (state.g1 = GB_2312) then begin
		    m.read esc; feed_string m "$)A";
		    state.g1 <- GB_2312;
		  end else ();
		  if not (state.gl = G1) then begin
		    m.read so;
		    state.gl <- G1
		  end else ();
		  m.read (Char.chr i1); m.read (Char.chr i2)
	      |  CNS_11643_p1 ->
		  if not (state.g1 = CNS_11643_p1) then begin
		    m.read esc; feed_string m "$)G";
		    state.g1 <- CNS_11643_p1;
		  end else ();
		  if not (state.gl = G1) then begin
		    m.read so;		state.gl <- G1
		  end else ();
		  m.read (Char.chr i1); m.read (Char.chr i2)
	      | CNS_11643_p2 ->
		  if not (state.g2 = CNS_11643_p2) then begin
		    m.read esc; feed_string m "$*H";
		    state.g2 <- CNS_11643_p2;
		  end else ();
		  m.read esc; m.read 'N';
		  m.read (Char.chr i1); m.read (Char.chr i2)
	      | _ -> assert false
	  end
	in
	let term () =
	  if state.gl = G0 then () else m.read si;
	  m.term ();
	in {read = reader; term = term} in
      
      {name = "ISO-2022-CN";
       make_decoder = make_decoder;
       make_encoder = make_encoder}

    let init () = 
      let _ = install "ISO-2022-CN" make_enc in ()
	
  end

let _ = Iso2022cn.init ()

(* shortcuts *)
let ascii = of_name "US-ASCII"
let latin1 = of_name "Latin-1"
let utf8 = of_name "UTF-8"
let utf16 = of_name "UTF-16"
let utf16be = of_name "UTF-16BE"
let utf16le = of_name "UTF-16LE"
let utf32 = of_name "UTF-32"
let utf32be = of_name "UTF-32BE"
let utf32le = of_name "UTF-32LE"
let ucs4 = of_name "UCS-4"

(* IANA *)
(* Name: ANSI_X3.4-1968                                   [RFC1345,KXS2] *)
(* Alias: iso-ir-6 *)
(* Alias: ANSI_X3.4-1986 *)
(* Alias: ISO_646.irv:1991 *)
(* Alias: ASCII *)
(* Alias: ISO646-US *)
(* Alias: US-ASCII (preferred MIME name) *)
(* Alias: us *)
(* Alias: IBM367 *)
(* Alias: cp367 *)
(* Alias: csASCII *)
let () = alias "IANA/csASCII" "ANSI_X3.4-1968"
let () = alias "IANA/cp367" "ANSI_X3.4-1968"
let () = alias "IANA/IBM367" "ANSI_X3.4-1968"
let () = alias "IANA/us" "ANSI_X3.4-1968"
let () = alias "IANA/US-ASCII" "ANSI_X3.4-1968"
let () = alias "IANA/ISO646-US" "ANSI_X3.4-1968"
let () = alias "IANA/ASCII" "ANSI_X3.4-1968"
let () = alias "IANA/ISO_646.irv:1991" "ANSI_X3.4-1968"
let () = alias "IANA/ANSI_X3.4-1986" "ANSI_X3.4-1968"
let () = alias "IANA/iso-ir-6" "ANSI_X3.4-1968"
let () = alias "IANA/ANSI_X3.4-1968" "ANSI_X3.4-1968"
(* Name: ISO-10646-UTF-1 *)
(* Alias: csISO10646UTF1 *)
(* Name: ISO_646.basic:1983                                [RFC1345,KXS2] *)
(* Alias: ref *)
(* Alias: csISO646basic1983 *)
let () = alias "IANA/csISO646basic1983" "ISO_646.BASIC"
let () = alias "IANA/ref" "ISO_646.BASIC"
let () = alias "IANA/ISO_646.basic:1983" "ISO_646.BASIC"
(* Name: INVARIANT                                         [RFC1345,KXS2] *)
(* Alias: csINVARIANT *)
let () = alias "IANA/csINVARIANT" "INVARIANT"
let () = alias "IANA/INVARIANT" "INVARIANT"
(* Name: ISO_646.irv:1983                                  [RFC1345,KXS2] *)
(* Alias: iso-ir-2 *)
(* Alias: irv *)
(* Alias: csISO2IntlRefVersion *)
let () = alias "IANA/csISO2IntlRefVersion" "ISO_646.IRV"
let () = alias "IANA/irv" "ISO_646.IRV"
let () = alias "IANA/iso-ir-2" "ISO_646.IRV"
let () = alias "IANA/ISO_646.irv:1983" "ISO_646.IRV"
(* Name: BS_4730                                           [RFC1345,KXS2] *)
(* Alias: iso-ir-4 *)
(* Alias: ISO646-GB *)
(* Alias: gb *)
(* Alias: uk *)
(* Alias: csISO4UnitedKingdom *)
let () = alias "IANA/csISO4UnitedKingdom" "BS_4730"
let () = alias "IANA/uk" "BS_4730"
let () = alias "IANA/gb" "BS_4730"
let () = alias "IANA/ISO646-GB" "BS_4730"
let () = alias "IANA/iso-ir-4" "BS_4730"
let () = alias "IANA/BS_4730" "BS_4730"
(* Name: NATS-SEFI                                         [RFC1345,KXS2] *)
(* Alias: iso-ir-8-1 *)
(* Alias: csNATSSEFI *)
let () = alias "IANA/csNATSSEFI" "NATS-SEFI"
let () = alias "IANA/iso-ir-8-1" "NATS-SEFI"
let () = alias "IANA/NATS-SEFI" "NATS-SEFI"
(* Name: NATS-SEFI-ADD                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-8-2 *)
(* Alias: csNATSSEFIADD *)
(* Name: NATS-DANO                                         [RFC1345,KXS2] *)
(* Alias: iso-ir-9-1 *)
(* Alias: csNATSDANO *)
let () = alias "IANA/csNATSDANO" "NATS-DANO"
let () = alias "IANA/iso-ir-9-1" "NATS-DANO"
let () = alias "IANA/NATS-DANO" "NATS-DANO"
(* Name: NATS-DANO-ADD                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-9-2 *)
(* Alias: csNATSDANOADD *)
(* Name: SEN_850200_B                                      [RFC1345,KXS2] *)
(* Alias: iso-ir-10 *)
(* Alias: FI *)
(* Alias: ISO646-FI *)
(* Alias: ISO646-SE *)
(* Alias: se *)
(* Alias: csISO10Swedish *)
let () = alias "IANA/csISO10Swedish" "SEN_850200_B"
let () = alias "IANA/se" "SEN_850200_B"
let () = alias "IANA/ISO646-SE" "SEN_850200_B"
let () = alias "IANA/ISO646-FI" "SEN_850200_B"
let () = alias "IANA/FI" "SEN_850200_B"
let () = alias "IANA/iso-ir-10" "SEN_850200_B"
let () = alias "IANA/SEN_850200_B" "SEN_850200_B"
(* Name: SEN_850200_C                                      [RFC1345,KXS2] *)
(* Alias: iso-ir-11 *)
(* Alias: ISO646-SE2 *)
(* Alias: se2 *)
(* Alias: csISO11SwedishForNames *)
let () = alias "IANA/csISO11SwedishForNames" "SEN_850200_C"
let () = alias "IANA/se2" "SEN_850200_C"
let () = alias "IANA/ISO646-SE2" "SEN_850200_C"
let () = alias "IANA/iso-ir-11" "SEN_850200_C"
let () = alias "IANA/SEN_850200_C" "SEN_850200_C"
(* Name: KS_C_5601-1987                                    [RFC1345,KXS2] *)
(* Alias: iso-ir-149 *)
(* Alias: KS_C_5601-1989 *)
(* Alias: KSC_5601 *)
(* Alias: korean *)
(* Alias: csKSC56011987 *)
(* Name: ISO-2022-KR  (preferred MIME name)                [RFC1557,Choi] *)
(* Alias: csISO2022KR *)
let () = alias "IANA/csISO2022KR" "ISO-2022-KR"
let () = alias "IANA/ISO-2022-KR" "ISO-2022-KR"
(* Name: EUC-KR  (preferred MIME name)                     [RFC1557,Choi] *)
(* Alias: csEUCKR *)
let () = alias "IANA/csEUCKR" "EUC-KR"
let () = alias "IANA/EUC-KR" "EUC-KR"
(* Name: ISO-2022-JP  (preferred MIME name)               [RFC1468,Murai] *)
(* Alias: csISO2022JP *)
let () = alias "IANA/csISO2022JP" "ISO-2022-JP"
let () = alias "IANA/ISO-2022-JP" "ISO-2022-JP"
(* Name: ISO-2022-JP-2  (preferred MIME name)              [RFC1554,Ohta] *)
(* Alias: csISO2022JP2 *)
let () = alias "IANA/csISO2022JP2" "ISO-2022-JP-2"
let () = alias "IANA/ISO-2022-JP-2" "ISO-2022-JP-2"
(* Name: ISO-2022-CN                                            [RFC1922] *)
let () = alias "IANA/ISO-2022-CN" "ISO-2022-CN"
(* Name: ISO-2022-CN-EXT                                        [RFC1922] *)
(* Name: JIS_C6220-1969-jp                                 [RFC1345,KXS2] *)
(* Alias: JIS_C6220-1969 *)
(* Alias: iso-ir-13 *)
(* Alias: katakana *)
(* Alias: x0201-7 *)
(* Alias: csISO13JISC6220jp *)
(* Name: JIS_C6220-1969-ro                                 [RFC1345,KXS2] *)
(* Alias: iso-ir-14 *)
(* Alias: jp *)
(* Alias: ISO646-JP *)
(* Alias: csISO14JISC6220ro *)
let () = alias "IANA/csISO14JISC6220ro" "JIS_C6220-1969-RO"
let () = alias "IANA/ISO646-JP" "JIS_C6220-1969-RO"
let () = alias "IANA/jp" "JIS_C6220-1969-RO"
let () = alias "IANA/iso-ir-14" "JIS_C6220-1969-RO"
let () = alias "IANA/JIS_C6220-1969-ro" "JIS_C6220-1969-RO"
(* Name: IT                                                [RFC1345,KXS2] *)
(* Alias: iso-ir-15 *)
(* Alias: ISO646-IT *)
(* Alias: csISO15Italian *)
let () = alias "IANA/csISO15Italian" "IT"
let () = alias "IANA/ISO646-IT" "IT"
let () = alias "IANA/iso-ir-15" "IT"
let () = alias "IANA/IT" "IT"
(* Name: PT                                                [RFC1345,KXS2] *)
(* Alias: iso-ir-16 *)
(* Alias: ISO646-PT *)
(* Alias: csISO16Portuguese *)
let () = alias "IANA/csISO16Portuguese" "PT"
let () = alias "IANA/ISO646-PT" "PT"
let () = alias "IANA/iso-ir-16" "PT"
let () = alias "IANA/PT" "PT"
(* Name: ES                                                [RFC1345,KXS2] *)
(* Alias: iso-ir-17 *)
(* Alias: ISO646-ES *)
(* Alias: csISO17Spanish *)
let () = alias "IANA/csISO17Spanish" "ES"
let () = alias "IANA/ISO646-ES" "ES"
let () = alias "IANA/iso-ir-17" "ES"
let () = alias "IANA/ES" "ES"
(* Name: greek7-old                                        [RFC1345,KXS2] *)
(* Alias: iso-ir-18 *)
(* Alias: csISO18Greek7Old *)
let () = alias "IANA/csISO18Greek7Old" "GREEK7-OLD"
let () = alias "IANA/iso-ir-18" "GREEK7-OLD"
let () = alias "IANA/greek7-old" "GREEK7-OLD"
(* Name: latin-greek                                       [RFC1345,KXS2] *)
(* Alias: iso-ir-19 *)
(* Alias: csISO19LatinGreek *)
let () = alias "IANA/csISO19LatinGreek" "LATIN-GREEK"
let () = alias "IANA/iso-ir-19" "LATIN-GREEK"
let () = alias "IANA/latin-greek" "LATIN-GREEK"
(* Name: DIN_66003                                         [RFC1345,KXS2] *)
(* Alias: iso-ir-21 *)
(* Alias: de *)
(* Alias: ISO646-DE *)
(* Alias: csISO21German *)
let () = alias "IANA/csISO21German" "DIN_66003"
let () = alias "IANA/ISO646-DE" "DIN_66003"
let () = alias "IANA/de" "DIN_66003"
let () = alias "IANA/iso-ir-21" "DIN_66003"
let () = alias "IANA/DIN_66003" "DIN_66003"
(* Name: NF_Z_62-010_(1973)                                [RFC1345,KXS2] *)
(* Alias: iso-ir-25 *)
(* Alias: ISO646-FR1 *)
(* Alias: csISO25French *)
let () = alias "IANA/csISO25French" "NF_Z_62-010_(1973)"
let () = alias "IANA/ISO646-FR1" "NF_Z_62-010_(1973)"
let () = alias "IANA/iso-ir-25" "NF_Z_62-010_(1973)"
let () = alias "IANA/NF_Z_62-010_(1973)" "NF_Z_62-010_(1973)"
(* Name: Latin-greek-1                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-27 *)
(* Alias: csISO27LatinGreek1 *)
let () = alias "IANA/csISO27LatinGreek1" "LATIN-GREEK-1"
let () = alias "IANA/iso-ir-27" "LATIN-GREEK-1"
let () = alias "IANA/Latin-greek-1" "LATIN-GREEK-1"
(* Name: ISO_5427                                          [RFC1345,KXS2] *)
(* Alias: iso-ir-37 *)
(* Alias: csISO5427Cyrillic *)
let () = alias "IANA/csISO5427Cyrillic" "ISO_5427"
let () = alias "IANA/iso-ir-37" "ISO_5427"
let () = alias "IANA/ISO_5427" "ISO_5427"
(* Name: JIS_C6226-1978                                    [RFC1345,KXS2] *)
(* Alias: iso-ir-42 *)
(* Alias: csISO42JISC62261978 *)
(* Name: BS_viewdata                                       [RFC1345,KXS2] *)
(* Alias: iso-ir-47 *)
(* Alias: csISO47BSViewdata *)
let () = alias "IANA/csISO47BSViewdata" "BS_VIEWDATA"
let () = alias "IANA/iso-ir-47" "BS_VIEWDATA"
let () = alias "IANA/BS_viewdata" "BS_VIEWDATA"
(* Name: INIS                                              [RFC1345,KXS2] *)
(* Alias: iso-ir-49 *)
(* Alias: csISO49INIS *)
let () = alias "IANA/csISO49INIS" "INIS"
let () = alias "IANA/iso-ir-49" "INIS"
let () = alias "IANA/INIS" "INIS"
(* Name: INIS-8                                            [RFC1345,KXS2] *)
(* Alias: iso-ir-50 *)
(* Alias: csISO50INIS8 *)
let () = alias "IANA/csISO50INIS8" "INIS-8"
let () = alias "IANA/iso-ir-50" "INIS-8"
let () = alias "IANA/INIS-8" "INIS-8"
(* Name: INIS-cyrillic                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-51 *)
(* Alias: csISO51INISCyrillic *)
let () = alias "IANA/csISO51INISCyrillic" "INIS-CYRILLIC"
let () = alias "IANA/iso-ir-51" "INIS-CYRILLIC"
let () = alias "IANA/INIS-cyrillic" "INIS-CYRILLIC"
(* Name: ISO_5427:1981                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-54 *)
(* Alias: ISO5427Cyrillic1981 *)
let () = alias "IANA/ISO5427Cyrillic1981" "ISO_5427-EXT"
let () = alias "IANA/iso-ir-54" "ISO_5427-EXT"
let () = alias "IANA/ISO_5427:1981" "ISO_5427-EXT"
(* Name: ISO_5428:1980                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-55 *)
(* Alias: csISO5428Greek *)
let () = alias "IANA/csISO5428Greek" "ISO_5428"
let () = alias "IANA/iso-ir-55" "ISO_5428"
let () = alias "IANA/ISO_5428:1980" "ISO_5428"
(* Name: GB_1988-80                                        [RFC1345,KXS2] *)
(* Alias: iso-ir-57 *)
(* Alias: cn *)
(* Alias: ISO646-CN *)
(* Alias: csISO57GB1988 *)
let () = alias "IANA/csISO57GB1988" "GB_1988-80"
let () = alias "IANA/ISO646-CN" "GB_1988-80"
let () = alias "IANA/cn" "GB_1988-80"
let () = alias "IANA/iso-ir-57" "GB_1988-80"
let () = alias "IANA/GB_1988-80" "GB_1988-80"
(* Name: GB_2312-80                                        [RFC1345,KXS2] *)
(* Alias: iso-ir-58 *)
(* Alias: chinese *)
(* Alias: csISO58GB231280 *)
(* Name: NS_4551-1                                         [RFC1345,KXS2] *)
(* Alias: iso-ir-60 *)
(* Alias: ISO646-NO *)
(* Alias: no *)
(* Alias: csISO60DanishNorwegian *)
(* Alias: csISO60Norwegian1 *)
let () = alias "IANA/csISO60Norwegian1" "NS_4551-1"
let () = alias "IANA/csISO60DanishNorwegian" "NS_4551-1"
let () = alias "IANA/no" "NS_4551-1"
let () = alias "IANA/ISO646-NO" "NS_4551-1"
let () = alias "IANA/iso-ir-60" "NS_4551-1"
let () = alias "IANA/NS_4551-1" "NS_4551-1"
(* Name: NS_4551-2                                          [RFC1345,KXS2] *)
(* Alias: ISO646-NO2 *)
(* Alias: iso-ir-61 *)
(* Alias: no2 *)
(* Alias: csISO61Norwegian2 *)
let () = alias "IANA/csISO61Norwegian2" "NS_4551-2"
let () = alias "IANA/no2" "NS_4551-2"
let () = alias "IANA/iso-ir-61" "NS_4551-2"
let () = alias "IANA/ISO646-NO2" "NS_4551-2"
let () = alias "IANA/NS_4551-2" "NS_4551-2"
(* Name: NF_Z_62-010                                        [RFC1345,KXS2] *)
(* Alias: iso-ir-69 *)
(* Alias: ISO646-FR *)
(* Alias: fr *)
(* Alias: csISO69French *)
let () = alias "IANA/csISO69French" "NF_Z_62-010"
let () = alias "IANA/fr" "NF_Z_62-010"
let () = alias "IANA/ISO646-FR" "NF_Z_62-010"
let () = alias "IANA/iso-ir-69" "NF_Z_62-010"
let () = alias "IANA/NF_Z_62-010" "NF_Z_62-010"
(* Name: videotex-suppl                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-70 *)
(* Alias: csISO70VideotexSupp1 *)
let () = alias "IANA/csISO70VideotexSupp1" "VIDEOTEX-SUPPL"
let () = alias "IANA/iso-ir-70" "VIDEOTEX-SUPPL"
let () = alias "IANA/videotex-suppl" "VIDEOTEX-SUPPL"
(* Name: PT2                                                [RFC1345,KXS2] *)
(* Alias: iso-ir-84 *)
(* Alias: ISO646-PT2 *)
(* Alias: csISO84Portuguese2 *)
let () = alias "IANA/csISO84Portuguese2" "PT2"
let () = alias "IANA/ISO646-PT2" "PT2"
let () = alias "IANA/iso-ir-84" "PT2"
let () = alias "IANA/PT2" "PT2"
(* Name: ES2                                                [RFC1345,KXS2] *)
(* Alias: iso-ir-85 *)
(* Alias: ISO646-ES2 *)
(* Alias: csISO85Spanish2 *)
let () = alias "IANA/csISO85Spanish2" "ES2"
let () = alias "IANA/ISO646-ES2" "ES2"
let () = alias "IANA/iso-ir-85" "ES2"
let () = alias "IANA/ES2" "ES2"
(* Name: MSZ_7795.3                                         [RFC1345,KXS2] *)
(* Alias: iso-ir-86 *)
(* Alias: ISO646-HU *)
(* Alias: hu *)
(* Alias: csISO86Hungarian *)
let () = alias "IANA/csISO86Hungarian" "MSZ_7795.3"
let () = alias "IANA/hu" "MSZ_7795.3"
let () = alias "IANA/ISO646-HU" "MSZ_7795.3"
let () = alias "IANA/iso-ir-86" "MSZ_7795.3"
let () = alias "IANA/MSZ_7795.3" "MSZ_7795.3"
(* Name: JIS_C6226-1983                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-87 *)
(* Alias: x0208 *)
(* Alias: JIS_X0208-1983 *)
(* Alias: csISO87JISX0208 *)
(* Name: greek7                                             [RFC1345,KXS2] *)
(* Alias: iso-ir-88 *)
(* Alias: csISO88Greek7 *)
let () = alias "IANA/csISO88Greek7" "GREEK7"
let () = alias "IANA/iso-ir-88" "GREEK7"
let () = alias "IANA/greek7" "GREEK7"
(* Name: ASMO_449                                           [RFC1345,KXS2] *)
(* Alias: ISO_9036 *)
(* Alias: arabic7 *)
(* Alias: iso-ir-89 *)
(* Alias: csISO89ASMO449 *)
let () = alias "IANA/csISO89ASMO449" "ASMO_449"
let () = alias "IANA/iso-ir-89" "ASMO_449"
let () = alias "IANA/arabic7" "ASMO_449"
let () = alias "IANA/ISO_9036" "ASMO_449"
let () = alias "IANA/ASMO_449" "ASMO_449"
(* Name: iso-ir-90                                          [RFC1345,KXS2] *)
(* Alias: csISO90 *)
let () = alias "IANA/csISO90" "ISO-IR-90"
let () = alias "IANA/iso-ir-90" "ISO-IR-90"
(* Name: JIS_C6229-1984-a                                   [RFC1345,KXS2] *)
(* Alias: iso-ir-91 *)
(* Alias: jp-ocr-a *)
(* Alias: csISO91JISC62291984a *)
(* Name: JIS_C6229-1984-b                                   [RFC1345,KXS2] *)
(* Alias: iso-ir-92 *)
(* Alias: ISO646-JP-OCR-B *)
(* Alias: jp-ocr-b *)
(* Alias: csISO92JISC62991984b *)
let () = alias "IANA/csISO92JISC62991984b" "JIS_C6229-1984-B"
let () = alias "IANA/jp-ocr-b" "JIS_C6229-1984-B"
let () = alias "IANA/ISO646-JP-OCR-B" "JIS_C6229-1984-B"
let () = alias "IANA/iso-ir-92" "JIS_C6229-1984-B"
let () = alias "IANA/JIS_C6229-1984-b" "JIS_C6229-1984-B"
(* Name: JIS_C6229-1984-b-add                               [RFC1345,KXS2] *)
(* Alias: iso-ir-93 *)
(* Alias: jp-ocr-b-add *)
(* Alias: csISO93JIS62291984badd *)
(* Name: JIS_C6229-1984-hand                                [RFC1345,KXS2] *)
(* Alias: iso-ir-94 *)
(* Alias: jp-ocr-hand *)
(* Alias: csISO94JIS62291984hand *)
(* Name: JIS_C6229-1984-hand-add                            [RFC1345,KXS2] *)
(* Alias: iso-ir-95 *)
(* Alias: jp-ocr-hand-add *)
(* Alias: csISO95JIS62291984handadd *)
(* Name: JIS_C6229-1984-kana                                [RFC1345,KXS2] *)
(* Alias: iso-ir-96 *)
(* Alias: csISO96JISC62291984kana *)
(* Name: ISO_2033-1983                                      [RFC1345,KXS2] *)
(* Alias: iso-ir-98 *)
(* Alias: e13b *)
(* Alias: csISO2033 *)
let () = alias "IANA/csISO2033" "ISO_2033-1983"
let () = alias "IANA/e13b" "ISO_2033-1983"
let () = alias "IANA/iso-ir-98" "ISO_2033-1983"
let () = alias "IANA/ISO_2033-1983" "ISO_2033-1983"
(* Name: ANSI_X3.110-1983                                   [RFC1345,KXS2] *)
(* Alias: iso-ir-99 *)
(* Alias: CSA_T500-1983 *)
(* Alias: NAPLPS *)
(* Alias: csISO99NAPLPS *)
let () = alias "IANA/csISO99NAPLPS" "ANSI_X3.110-1983"
let () = alias "IANA/NAPLPS" "ANSI_X3.110-1983"
let () = alias "IANA/CSA_T500-1983" "ANSI_X3.110-1983"
let () = alias "IANA/iso-ir-99" "ANSI_X3.110-1983"
let () = alias "IANA/ANSI_X3.110-1983" "ANSI_X3.110-1983"
(* Name: ISO_8859-1:1987                                    [RFC1345,KXS2] *)
(* Alias: iso-ir-100 *)
(* Alias: ISO_8859-1 *)
(* Alias: ISO-8859-1 (preferred MIME name) *)
(* Alias: latin1 *)
(* Alias: l1 *)
(* Alias: IBM819 *)
(* Alias: CP819 *)
(* Alias: csISOLatin1 *)
let () = alias "IANA/csISOLatin1" "ISO-8859-1"
let () = alias "IANA/CP819" "ISO-8859-1"
let () = alias "IANA/IBM819" "ISO-8859-1"
let () = alias "IANA/l1" "ISO-8859-1"
let () = alias "IANA/latin1" "ISO-8859-1"
let () = alias "IANA/ISO-8859-1" "ISO-8859-1"
let () = alias "IANA/ISO_8859-1" "ISO-8859-1"
let () = alias "IANA/iso-ir-100" "ISO-8859-1"
let () = alias "IANA/ISO_8859-1:1987" "ISO-8859-1"
(* Name: ISO_8859-2:1987                                    [RFC1345,KXS2] *)
(* Alias: iso-ir-101 *)
(* Alias: ISO_8859-2 *)
(* Alias: ISO-8859-2 (preferred MIME name) *)
(* Alias: latin2 *)
(* Alias: l2 *)
(* Alias: csISOLatin2 *)
let () = alias "IANA/csISOLatin2" "ISO-8859-2"
let () = alias "IANA/l2" "ISO-8859-2"
let () = alias "IANA/latin2" "ISO-8859-2"
let () = alias "IANA/ISO-8859-2" "ISO-8859-2"
let () = alias "IANA/ISO_8859-2" "ISO-8859-2"
let () = alias "IANA/iso-ir-101" "ISO-8859-2"
let () = alias "IANA/ISO_8859-2:1987" "ISO-8859-2"
(* Name: T.61-7bit                                          [RFC1345,KXS2] *)
(* Alias: iso-ir-102 *)
(* Alias: csISO102T617bit *)
let () = alias "IANA/csISO102T617bit" "T.61-7BIT"
let () = alias "IANA/iso-ir-102" "T.61-7BIT"
let () = alias "IANA/T.61-7bit" "T.61-7BIT"
(* Name: T.61-8bit                                          [RFC1345,KXS2] *)
(* Alias: T.61 *)
(* Alias: iso-ir-103 *)
(* Alias: csISO103T618bit *)
let () = alias "IANA/csISO103T618bit" "T.61-8BIT"
let () = alias "IANA/iso-ir-103" "T.61-8BIT"
let () = alias "IANA/T.61" "T.61-8BIT"
let () = alias "IANA/T.61-8bit" "T.61-8BIT"
(* Name: ISO_8859-3:1988                                    [RFC1345,KXS2] *)
(* Alias: iso-ir-109 *)
(* Alias: ISO_8859-3 *)
(* Alias: ISO-8859-3 (preferred MIME name) *)
(* Alias: latin3 *)
(* Alias: l3 *)
(* Alias: csISOLatin3 *)
let () = alias "IANA/csISOLatin3" "ISO-8859-3"
let () = alias "IANA/l3" "ISO-8859-3"
let () = alias "IANA/latin3" "ISO-8859-3"
let () = alias "IANA/ISO-8859-3" "ISO-8859-3"
let () = alias "IANA/ISO_8859-3" "ISO-8859-3"
let () = alias "IANA/iso-ir-109" "ISO-8859-3"
let () = alias "IANA/ISO_8859-3:1988" "ISO-8859-3"
(* Name: ISO_8859-4:1988                                    [RFC1345,KXS2] *)
(* Alias: iso-ir-110 *)
(* Alias: ISO_8859-4 *)
(* Alias: ISO-8859-4 (preferred MIME name) *)
(* Alias: latin4 *)
(* Alias: l4 *)
(* Alias: csISOLatin4 *)
let () = alias "IANA/csISOLatin4" "ISO-8859-4"
let () = alias "IANA/l4" "ISO-8859-4"
let () = alias "IANA/latin4" "ISO-8859-4"
let () = alias "IANA/ISO-8859-4" "ISO-8859-4"
let () = alias "IANA/ISO_8859-4" "ISO-8859-4"
let () = alias "IANA/iso-ir-110" "ISO-8859-4"
let () = alias "IANA/ISO_8859-4:1988" "ISO-8859-4"
(* Name: ECMA-cyrillic                                      [RFC1345,KXS2] *)
(* Alias: iso-ir-111 *)
(* Alias: csISO111ECMACyrillic *)
let () = alias "IANA/csISO111ECMACyrillic" "ECMA-CYRILLIC"
let () = alias "IANA/iso-ir-111" "ECMA-CYRILLIC"
let () = alias "IANA/ECMA-cyrillic" "ECMA-CYRILLIC"
(* Name: CSA_Z243.4-1985-1                                  [RFC1345,KXS2] *)
(* Alias: iso-ir-121 *)
(* Alias: ISO646-CA *)
(* Alias: csa7-1 *)
(* Alias: ca *)
(* Alias: csISO121Canadian1 *)
let () = alias "IANA/csISO121Canadian1" "CSA_Z243.4-1985-1"
let () = alias "IANA/ca" "CSA_Z243.4-1985-1"
let () = alias "IANA/csa7-1" "CSA_Z243.4-1985-1"
let () = alias "IANA/ISO646-CA" "CSA_Z243.4-1985-1"
let () = alias "IANA/iso-ir-121" "CSA_Z243.4-1985-1"
let () = alias "IANA/CSA_Z243.4-1985-1" "CSA_Z243.4-1985-1"
(* Name: CSA_Z243.4-1985-2                                  [RFC1345,KXS2] *)
(* Alias: iso-ir-122 *)
(* Alias: ISO646-CA2 *)
(* Alias: csa7-2 *)
(* Alias: csISO122Canadian2 *)
let () = alias "IANA/csISO122Canadian2" "CSA_Z243.4-1985-2"
let () = alias "IANA/csa7-2" "CSA_Z243.4-1985-2"
let () = alias "IANA/ISO646-CA2" "CSA_Z243.4-1985-2"
let () = alias "IANA/iso-ir-122" "CSA_Z243.4-1985-2"
let () = alias "IANA/CSA_Z243.4-1985-2" "CSA_Z243.4-1985-2"
(* Name: CSA_Z243.4-1985-gr                                 [RFC1345,KXS2] *)
(* Alias: iso-ir-123 *)
(* Alias: csISO123CSAZ24341985gr *)
let () = alias "IANA/csISO123CSAZ24341985gr" "CSA_Z243.4-1985-GR"
let () = alias "IANA/iso-ir-123" "CSA_Z243.4-1985-GR"
let () = alias "IANA/CSA_Z243.4-1985-gr" "CSA_Z243.4-1985-GR"
(* Name: ISO_8859-6:1987                                    [RFC1345,KXS2] *)
(* Alias: iso-ir-127 *)
(* Alias: ISO_8859-6 *)
(* Alias: ISO-8859-6 (preferred MIME name) *)
(* Alias: ECMA-114 *)
(* Alias: ASMO-708 *)
(* Alias: arabic *)
(* Alias: csISOLatinArabic *)
let () = alias "IANA/csISOLatinArabic" "ISO-8859-6"
let () = alias "IANA/arabic" "ISO-8859-6"
let () = alias "IANA/ASMO-708" "ISO-8859-6"
let () = alias "IANA/ECMA-114" "ISO-8859-6"
let () = alias "IANA/ISO-8859-6" "ISO-8859-6"
let () = alias "IANA/ISO_8859-6" "ISO-8859-6"
let () = alias "IANA/iso-ir-127" "ISO-8859-6"
let () = alias "IANA/ISO_8859-6:1987" "ISO-8859-6"
(* Name: ISO_8859-6-E                                       [RFC1556,IANA] *)
(* Alias: csISO88596E *)
(* Alias: ISO-8859-6-E (preferred MIME name) *)
(* Name: ISO_8859-6-I                                       [RFC1556,IANA] *)
(* Alias: csISO88596I *)
(* Alias: ISO-8859-6-I (preferred MIME name) *)
(* Name: ISO_8859-7:1987                            [RFC1947,RFC1345,KXS2] *)
(* Alias: iso-ir-126 *)
(* Alias: ISO_8859-7 *)
(* Alias: ISO-8859-7 (preferred MIME name) *)
(* Alias: ELOT_928 *)
(* Alias: ECMA-118 *)
(* Alias: greek *)
(* Alias: greek8 *)
(* Alias: csISOLatinGreek *)
let () = alias "IANA/csISOLatinGreek" "ISO-8859-7"
let () = alias "IANA/greek8" "ISO-8859-7"
let () = alias "IANA/greek" "ISO-8859-7"
let () = alias "IANA/ECMA-118" "ISO-8859-7"
let () = alias "IANA/ELOT_928" "ISO-8859-7"
let () = alias "IANA/ISO-8859-7" "ISO-8859-7"
let () = alias "IANA/ISO_8859-7" "ISO-8859-7"
let () = alias "IANA/iso-ir-126" "ISO-8859-7"
let () = alias "IANA/ISO_8859-7:1987" "ISO-8859-7"
(* Name: T.101-G2                                            [RFC1345,KXS2] *)
(* Alias: iso-ir-128 *)
(* Alias: csISO128T101G2 *)
let () = alias "IANA/csISO128T101G2" "T.101-G2"
let () = alias "IANA/iso-ir-128" "T.101-G2"
let () = alias "IANA/T.101-G2" "T.101-G2"
(* Name: ISO_8859-8:1988                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-138 *)
(* Alias: ISO_8859-8 *)
(* Alias: ISO-8859-8 (preferred MIME name) *)
(* Alias: hebrew *)
(* Alias: csISOLatinHebrew *)
let () = alias "IANA/csISOLatinHebrew" "ISO-8859-8"
let () = alias "IANA/hebrew" "ISO-8859-8"
let () = alias "IANA/ISO-8859-8" "ISO-8859-8"
let () = alias "IANA/ISO_8859-8" "ISO-8859-8"
let () = alias "IANA/iso-ir-138" "ISO-8859-8"
let () = alias "IANA/ISO_8859-8:1988" "ISO-8859-8"
(* Name: ISO_8859-8-E                                  [RFC1556,Nussbacher] *)
(* Alias: csISO88598E *)
(* Alias: ISO-8859-8-E (preferred MIME name) *)
(* Name: ISO_8859-8-I                                  [RFC1556,Nussbacher] *)
(* Alias: csISO88598I *)
(* Alias: ISO-8859-8-I (preferred MIME name) *)
(* Name: CSN_369103                                          [RFC1345,KXS2] *)
(* Alias: iso-ir-139 *)
(* Alias: csISO139CSN369103 *)
let () = alias "IANA/csISO139CSN369103" "CSN_369103"
let () = alias "IANA/iso-ir-139" "CSN_369103"
let () = alias "IANA/CSN_369103" "CSN_369103"
(* Name: JUS_I.B1.002                                        [RFC1345,KXS2] *)
(* Alias: iso-ir-141 *)
(* Alias: ISO646-YU *)
(* Alias: js *)
(* Alias: yu *)
(* Alias: csISO141JUSIB1002 *)
let () = alias "IANA/csISO141JUSIB1002" "JUS_I.B1.002"
let () = alias "IANA/yu" "JUS_I.B1.002"
let () = alias "IANA/js" "JUS_I.B1.002"
let () = alias "IANA/ISO646-YU" "JUS_I.B1.002"
let () = alias "IANA/iso-ir-141" "JUS_I.B1.002"
let () = alias "IANA/JUS_I.B1.002" "JUS_I.B1.002"
(* Name: ISO_6937-2-add                                      [RFC1345,KXS2] *)
(* Alias: iso-ir-142 *)
(* Alias: csISOTextComm *)
let () = alias "IANA/csISOTextComm" "ISO_6937-2-ADD"
let () = alias "IANA/iso-ir-142" "ISO_6937-2-ADD"
let () = alias "IANA/ISO_6937-2-add" "ISO_6937-2-ADD"
(* Name: IEC_P27-1                                           [RFC1345,KXS2] *)
(* Alias: iso-ir-143 *)
(* Alias: csISO143IECP271 *)
let () = alias "IANA/csISO143IECP271" "IEC_P27-1"
let () = alias "IANA/iso-ir-143" "IEC_P27-1"
let () = alias "IANA/IEC_P27-1" "IEC_P27-1"
(* Name: ISO_8859-5:1988                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-144 *)
(* Alias: ISO_8859-5 *)
(* Alias: ISO-8859-5 (preferred MIME name) *)
(* Alias: cyrillic *)
(* Alias: csISOLatinCyrillic *)
let () = alias "IANA/csISOLatinCyrillic" "ISO-8859-5"
let () = alias "IANA/cyrillic" "ISO-8859-5"
let () = alias "IANA/ISO-8859-5" "ISO-8859-5"
let () = alias "IANA/ISO_8859-5" "ISO-8859-5"
let () = alias "IANA/iso-ir-144" "ISO-8859-5"
let () = alias "IANA/ISO_8859-5:1988" "ISO-8859-5"
(* Name: JUS_I.B1.003-serb                                   [RFC1345,KXS2] *)
(* Alias: iso-ir-146 *)
(* Alias: serbian *)
(* Alias: csISO146Serbian *)
let () = alias "IANA/csISO146Serbian" "JUS_I.B1.003-SERB"
let () = alias "IANA/serbian" "JUS_I.B1.003-SERB"
let () = alias "IANA/iso-ir-146" "JUS_I.B1.003-SERB"
let () = alias "IANA/JUS_I.B1.003-serb" "JUS_I.B1.003-SERB"
(* Name: JUS_I.B1.003-mac                                    [RFC1345,KXS2] *)
(* Alias: macedonian *)
(* Alias: iso-ir-147 *)
(* Alias: csISO147Macedonian *)
let () = alias "IANA/csISO147Macedonian" "JUS_I.B1.003-MAC"
let () = alias "IANA/iso-ir-147" "JUS_I.B1.003-MAC"
let () = alias "IANA/macedonian" "JUS_I.B1.003-MAC"
let () = alias "IANA/JUS_I.B1.003-mac" "JUS_I.B1.003-MAC"
(* Name: ISO_8859-9:1989                                     [RFC1345,KXS2] *)
(* Alias: iso-ir-148 *)
(* Alias: ISO_8859-9 *)
(* Alias: ISO-8859-9 (preferred MIME name) *)
(* Alias: latin5 *)
(* Alias: l5 *)
(* Alias: csISOLatin5 *)
let () = alias "IANA/csISOLatin5" "ISO-8859-9"
let () = alias "IANA/l5" "ISO-8859-9"
let () = alias "IANA/latin5" "ISO-8859-9"
let () = alias "IANA/ISO-8859-9" "ISO-8859-9"
let () = alias "IANA/ISO_8859-9" "ISO-8859-9"
let () = alias "IANA/iso-ir-148" "ISO-8859-9"
let () = alias "IANA/ISO_8859-9:1989" "ISO-8859-9"
(* Name: greek-ccitt                                         [RFC1345,KXS2] *)
(* Alias: iso-ir-150 *)
(* Alias: csISO150 *)
(* Alias: csISO150GreekCCITT *)
let () = alias "IANA/csISO150GreekCCITT" "GREEK-CCITT"
let () = alias "IANA/csISO150" "GREEK-CCITT"
let () = alias "IANA/iso-ir-150" "GREEK-CCITT"
let () = alias "IANA/greek-ccitt" "GREEK-CCITT"
(* Name: NC_NC00-10:81                                       [RFC1345,KXS2] *)
(* Alias: cuba *)
(* Alias: iso-ir-151 *)
(* Alias: ISO646-CU *)
(* Alias: csISO151Cuba *)
let () = alias "IANA/csISO151Cuba" "NC_NC00-10"
let () = alias "IANA/ISO646-CU" "NC_NC00-10"
let () = alias "IANA/iso-ir-151" "NC_NC00-10"
let () = alias "IANA/cuba" "NC_NC00-10"
let () = alias "IANA/NC_NC00-10:81" "NC_NC00-10"
(* Name: ISO_6937-2-25                                       [RFC1345,KXS2] *)
(* Alias: iso-ir-152 *)
(* Alias: csISO6937Add *)
let () = alias "IANA/csISO6937Add" "ISO_6937-2-25"
let () = alias "IANA/iso-ir-152" "ISO_6937-2-25"
let () = alias "IANA/ISO_6937-2-25" "ISO_6937-2-25"
(* Name: GOST_19768-74                                       [RFC1345,KXS2] *)
(* Alias: ST_SEV_358-88 *)
(* Alias: iso-ir-153 *)
(* Alias: csISO153GOST1976874 *)
let () = alias "IANA/csISO153GOST1976874" "GOST_19768-74"
let () = alias "IANA/iso-ir-153" "GOST_19768-74"
let () = alias "IANA/ST_SEV_358-88" "GOST_19768-74"
let () = alias "IANA/GOST_19768-74" "GOST_19768-74"
(* Name: ISO_8859-supp                                       [RFC1345,KXS2] *)
(* Alias: iso-ir-154 *)
(* Alias: latin1-2-5 *)
(* Alias: csISO8859Supp *)
let () = alias "IANA/csISO8859Supp" "ISO_8859-SUPP"
let () = alias "IANA/latin1-2-5" "ISO_8859-SUPP"
let () = alias "IANA/iso-ir-154" "ISO_8859-SUPP"
let () = alias "IANA/ISO_8859-supp" "ISO_8859-SUPP"
(* Name: ISO_10367-box                                       [RFC1345,KXS2] *)
(* Alias: iso-ir-155 *)
(* Alias: csISO10367Box *)
let () = alias "IANA/csISO10367Box" "ISO_10367-BOX"
let () = alias "IANA/iso-ir-155" "ISO_10367-BOX"
let () = alias "IANA/ISO_10367-box" "ISO_10367-BOX"
(* Name: ISO-8859-10 (preferred MIME name)			  [RFC1345,KXS2] *)
(* Alias: iso-ir-157 *)
(* Alias: l6 *)
(* Alias: ISO_8859-10:1992 *)
(* Alias: csISOLatin6 *)
(* Alias: latin6 *)
let () = alias "IANA/latin6" "ISO-8859-10"
let () = alias "IANA/csISOLatin6" "ISO-8859-10"
let () = alias "IANA/ISO_8859-10:1992" "ISO-8859-10"
let () = alias "IANA/l6" "ISO-8859-10"
let () = alias "IANA/iso-ir-157" "ISO-8859-10"
let () = alias "IANA/ISO-8859-10" "ISO-8859-10"
(* Name: latin-lap                                           [RFC1345,KXS2] *)
(* Alias: lap *)
(* Alias: iso-ir-158 *)
(* Alias: csISO158Lap *)
let () = alias "IANA/csISO158Lap" "SAMI"
let () = alias "IANA/iso-ir-158" "SAMI"
let () = alias "IANA/lap" "SAMI"
let () = alias "IANA/latin-lap" "SAMI"
(* Name: JIS_X0212-1990                                      [RFC1345,KXS2] *)
(* Alias: x0212 *)
(* Alias: iso-ir-159 *)
(* Alias: csISO159JISX02121990 *)
(* Name: DS_2089                                             [RFC1345,KXS2] *)
(* Alias: DS2089 *)
(* Alias: ISO646-DK *)
(* Alias: dk *)
(* Alias: csISO646Danish *)
let () = alias "IANA/csISO646Danish" "DS_2089"
let () = alias "IANA/dk" "DS_2089"
let () = alias "IANA/ISO646-DK" "DS_2089"
let () = alias "IANA/DS2089" "DS_2089"
let () = alias "IANA/DS_2089" "DS_2089"
(* Name: us-dk                                               [RFC1345,KXS2] *)
(* Alias: csUSDK *)
(* Name: dk-us                                               [RFC1345,KXS2] *)
(* Alias: csDKUS *)
(* Name: JIS_X0201                                           [RFC1345,KXS2] *)
(* Alias: X0201 *)
(* Alias: csHalfWidthKatakana *)
let () = alias "IANA/csHalfWidthKatakana" "JIS_X0201"
let () = alias "IANA/X0201" "JIS_X0201"
let () = alias "IANA/JIS_X0201" "JIS_X0201"
(* Name: KSC5636                                             [RFC1345,KXS2] *)
(* Alias: ISO646-KR *)
(* Alias: csKSC5636 *)
let () = alias "IANA/csKSC5636" "KSC5636"
let () = alias "IANA/ISO646-KR" "KSC5636"
let () = alias "IANA/KSC5636" "KSC5636"
(* Name: ISO-10646-UCS-2 *)
(* Alias: csUnicode *)
let () = alias "IANA/ISO-10646-UCS-2" "UTF-16"
let () = alias "IANA/csUnicode" "UTF-16"
(* Name: ISO-10646-UCS-4 *)
(* Alias: csUCS4 *)
let () = alias "IANA/ISO-10646-UCS-4" "UCS4"
let () = alias "IANA/csUCS4" "UCS4"
(* Name: DEC-MCS                                             [RFC1345,KXS2] *)
(* Alias: dec *)
(* Alias: csDECMCS *)
let () = alias "IANA/csDECMCS" "DEC-MCS"
let () = alias "IANA/dec" "DEC-MCS"
let () = alias "IANA/DEC-MCS" "DEC-MCS"
(* Name: hp-roman8                                  [HP-PCL5,RFC1345,KXS2] *)
(* Alias: roman8 *)
(* Alias: r8 *)
(* Alias: csHPRoman8 *)
let () = alias "IANA/csHPRoman8" "HP-ROMAN8"
let () = alias "IANA/r8" "HP-ROMAN8"
let () = alias "IANA/roman8" "HP-ROMAN8"
let () = alias "IANA/hp-roman8" "HP-ROMAN8"
(* Name: macintosh                                           [RFC1345,KXS2] *)
(* Alias: mac *)
(* Alias: csMacintosh *)
let () = alias "IANA/csMacintosh" "MACINTOSH"
let () = alias "IANA/mac" "MACINTOSH"
let () = alias "IANA/macintosh" "MACINTOSH"
(* Name: IBM037                                              [RFC1345,KXS2] *)
(* Alias: cp037 *)
(* Alias: ebcdic-cp-us *)
(* Alias: ebcdic-cp-ca *)
(* Alias: ebcdic-cp-wt *)
(* Alias: ebcdic-cp-nl *)
(* Alias: csIBM037 *)
let () = alias "IANA/csIBM037" "IBM037"
let () = alias "IANA/ebcdic-cp-nl" "IBM037"
let () = alias "IANA/ebcdic-cp-wt" "IBM037"
let () = alias "IANA/ebcdic-cp-ca" "IBM037"
let () = alias "IANA/ebcdic-cp-us" "IBM037"
let () = alias "IANA/cp037" "IBM037"
let () = alias "IANA/IBM037" "IBM037"
(* Name: IBM038                                              [RFC1345,KXS2] *)
(* Alias: EBCDIC-INT *)
(* Alias: cp038 *)
(* Alias: csIBM038 *)
let () = alias "IANA/csIBM038" "IBM038"
let () = alias "IANA/cp038" "IBM038"
let () = alias "IANA/EBCDIC-INT" "IBM038"
let () = alias "IANA/IBM038" "IBM038"
(* Name: IBM273                                              [RFC1345,KXS2] *)
(* Alias: CP273 *)
(* Alias: csIBM273 *)
let () = alias "IANA/csIBM273" "IBM273"
let () = alias "IANA/CP273" "IBM273"
let () = alias "IANA/IBM273" "IBM273"
(* Name: IBM274                                              [RFC1345,KXS2] *)
(* Alias: EBCDIC-BE *)
(* Alias: CP274 *)
(* Alias: csIBM274 *)
let () = alias "IANA/csIBM274" "IBM274"
let () = alias "IANA/CP274" "IBM274"
let () = alias "IANA/EBCDIC-BE" "IBM274"
let () = alias "IANA/IBM274" "IBM274"
(* Name: IBM275                                              [RFC1345,KXS2] *)
(* Alias: EBCDIC-BR *)
(* Alias: cp275 *)
(* Alias: csIBM275 *)
let () = alias "IANA/csIBM275" "IBM275"
let () = alias "IANA/cp275" "IBM275"
let () = alias "IANA/EBCDIC-BR" "IBM275"
let () = alias "IANA/IBM275" "IBM275"
(* Name: IBM277                                              [RFC1345,KXS2] *)
(* Alias: EBCDIC-CP-DK *)
(* Alias: EBCDIC-CP-NO *)
(* Alias: csIBM277 *)
let () = alias "IANA/csIBM277" "IBM277"
let () = alias "IANA/EBCDIC-CP-NO" "IBM277"
let () = alias "IANA/EBCDIC-CP-DK" "IBM277"
let () = alias "IANA/IBM277" "IBM277"
(* Name: IBM278                                              [RFC1345,KXS2] *)
(* Alias: CP278 *)
(* Alias: ebcdic-cp-fi *)
(* Alias: ebcdic-cp-se *)
(* Alias: csIBM278 *)
let () = alias "IANA/csIBM278" "IBM278"
let () = alias "IANA/ebcdic-cp-se" "IBM278"
let () = alias "IANA/ebcdic-cp-fi" "IBM278"
let () = alias "IANA/CP278" "IBM278"
let () = alias "IANA/IBM278" "IBM278"
(* Name: IBM280                                              [RFC1345,KXS2] *)
(* Alias: CP280 *)
(* Alias: ebcdic-cp-it *)
(* Alias: csIBM280 *)
let () = alias "IANA/csIBM280" "IBM280"
let () = alias "IANA/ebcdic-cp-it" "IBM280"
let () = alias "IANA/CP280" "IBM280"
let () = alias "IANA/IBM280" "IBM280"
(* Name: IBM281                                              [RFC1345,KXS2] *)
(* Alias: EBCDIC-JP-E *)
(* Alias: cp281 *)
(* Alias: csIBM281 *)
let () = alias "IANA/csIBM281" "IBM281"
let () = alias "IANA/cp281" "IBM281"
let () = alias "IANA/EBCDIC-JP-E" "IBM281"
let () = alias "IANA/IBM281" "IBM281"
(* Name: IBM284                                              [RFC1345,KXS2] *)
(* Alias: CP284 *)
(* Alias: ebcdic-cp-es *)
(* Alias: csIBM284 *)
let () = alias "IANA/csIBM284" "IBM284"
let () = alias "IANA/ebcdic-cp-es" "IBM284"
let () = alias "IANA/CP284" "IBM284"
let () = alias "IANA/IBM284" "IBM284"
(* Name: IBM285                                              [RFC1345,KXS2] *)
(* Alias: CP285 *)
(* Alias: ebcdic-cp-gb *)
(* Alias: csIBM285 *)
let () = alias "IANA/csIBM285" "IBM285"
let () = alias "IANA/ebcdic-cp-gb" "IBM285"
let () = alias "IANA/CP285" "IBM285"
let () = alias "IANA/IBM285" "IBM285"
(* Name: IBM290                                              [RFC1345,KXS2] *)
(* Alias: cp290 *)
(* Alias: EBCDIC-JP-kana *)
(* Alias: csIBM290 *)
let () = alias "IANA/csIBM290" "IBM290"
let () = alias "IANA/EBCDIC-JP-kana" "IBM290"
let () = alias "IANA/cp290" "IBM290"
let () = alias "IANA/IBM290" "IBM290"
(* Name: IBM297                                              [RFC1345,KXS2] *)
(* Alias: cp297 *)
(* Alias: ebcdic-cp-fr *)
(* Alias: csIBM297 *)
let () = alias "IANA/csIBM297" "IBM297"
let () = alias "IANA/ebcdic-cp-fr" "IBM297"
let () = alias "IANA/cp297" "IBM297"
let () = alias "IANA/IBM297" "IBM297"
(* Name: IBM420                                              [RFC1345,KXS2] *)
(* Alias: cp420 *)
(* Alias: ebcdic-cp-ar1 *)
(* Alias: csIBM420 *)
let () = alias "IANA/csIBM420" "IBM420"
let () = alias "IANA/ebcdic-cp-ar1" "IBM420"
let () = alias "IANA/cp420" "IBM420"
let () = alias "IANA/IBM420" "IBM420"
(* Name: IBM423                                              [RFC1345,KXS2] *)
(* Alias: cp423 *)
(* Alias: ebcdic-cp-gr *)
(* Alias: csIBM423 *)
let () = alias "IANA/csIBM423" "IBM423"
let () = alias "IANA/ebcdic-cp-gr" "IBM423"
let () = alias "IANA/cp423" "IBM423"
let () = alias "IANA/IBM423" "IBM423"
(* Name: IBM424                                              [RFC1345,KXS2] *)
(* Alias: cp424 *)
(* Alias: ebcdic-cp-he *)
(* Alias: csIBM424 *)
let () = alias "IANA/csIBM424" "IBM424"
let () = alias "IANA/ebcdic-cp-he" "IBM424"
let () = alias "IANA/cp424" "IBM424"
let () = alias "IANA/IBM424" "IBM424"
(* Name: IBM437                                              [RFC1345,KXS2] *)
(* Alias: cp437 *)
(* Alias: 437 *)
(* Alias: csPC8CodePage437 *)
let () = alias "IANA/csPC8CodePage437" "IBM437"
let () = alias "IANA/437" "IBM437"
let () = alias "IANA/cp437" "IBM437"
let () = alias "IANA/IBM437" "IBM437"
(* Name: IBM500                                              [RFC1345,KXS2] *)
(* Alias: CP500 *)
(* Alias: ebcdic-cp-be *)
(* Alias: ebcdic-cp-ch *)
(* Alias: csIBM500 *)
let () = alias "IANA/csIBM500" "IBM500"
let () = alias "IANA/ebcdic-cp-ch" "IBM500"
let () = alias "IANA/ebcdic-cp-be" "IBM500"
let () = alias "IANA/CP500" "IBM500"
let () = alias "IANA/IBM500" "IBM500"
(* Name: IBM775                                                   [HP-PCL5] *)
(* Alias: cp775 *)
(* Alias: csPC775Baltic *)
(* Name: IBM850                                              [RFC1345,KXS2] *)
(* Alias: cp850 *)
(* Alias: 850 *)
(* Alias: csPC850Multilingual *)
let () = alias "IANA/csPC850Multilingual" "IBM850"
let () = alias "IANA/850" "IBM850"
let () = alias "IANA/cp850" "IBM850"
let () = alias "IANA/IBM850" "IBM850"
(* Name: IBM851                                              [RFC1345,KXS2] *)
(* Alias: cp851 *)
(* Alias: 851 *)
(* Alias: csIBM851 *)
let () = alias "IANA/csIBM851" "IBM851"
let () = alias "IANA/851" "IBM851"
let () = alias "IANA/cp851" "IBM851"
let () = alias "IANA/IBM851" "IBM851"
(* Name: IBM852                                              [RFC1345,KXS2] *)
(* Alias: cp852 *)
(* Alias: 852 *)
(* Alias: csPCp852 *)
let () = alias "IANA/csPCp852" "IBM852"
let () = alias "IANA/852" "IBM852"
let () = alias "IANA/cp852" "IBM852"
let () = alias "IANA/IBM852" "IBM852"
(* Name: IBM855                                              [RFC1345,KXS2] *)
(* Alias: cp855 *)
(* Alias: 855 *)
(* Alias: csIBM855 *)
let () = alias "IANA/csIBM855" "IBM855"
let () = alias "IANA/855" "IBM855"
let () = alias "IANA/cp855" "IBM855"
let () = alias "IANA/IBM855" "IBM855"
(* Name: IBM857                                              [RFC1345,KXS2] *)
(* Alias: cp857 *)
(* Alias: 857 *)
(* Alias: csIBM857 *)
let () = alias "IANA/csIBM857" "IBM857"
let () = alias "IANA/857" "IBM857"
let () = alias "IANA/cp857" "IBM857"
let () = alias "IANA/IBM857" "IBM857"
(* Name: IBM860                                              [RFC1345,KXS2] *)
(* Alias: cp860 *)
(* Alias: 860 *)
(* Alias: csIBM860 *)
let () = alias "IANA/csIBM860" "IBM860"
let () = alias "IANA/860" "IBM860"
let () = alias "IANA/cp860" "IBM860"
let () = alias "IANA/IBM860" "IBM860"
(* Name: IBM861                                              [RFC1345,KXS2] *)
(* Alias: cp861 *)
(* Alias: 861 *)
(* Alias: cp-is *)
(* Alias: csIBM861 *)
let () = alias "IANA/csIBM861" "IBM861"
let () = alias "IANA/cp-is" "IBM861"
let () = alias "IANA/861" "IBM861"
let () = alias "IANA/cp861" "IBM861"
let () = alias "IANA/IBM861" "IBM861"
(* Name: IBM862                                              [RFC1345,KXS2] *)
(* Alias: cp862 *)
(* Alias: 862 *)
(* Alias: csPC862LatinHebrew *)
let () = alias "IANA/csPC862LatinHebrew" "IBM862"
let () = alias "IANA/862" "IBM862"
let () = alias "IANA/cp862" "IBM862"
let () = alias "IANA/IBM862" "IBM862"
(* Name: IBM863                                              [RFC1345,KXS2] *)
(* Alias: cp863 *)
(* Alias: 863 *)
(* Alias: csIBM863 *)
let () = alias "IANA/csIBM863" "IBM863"
let () = alias "IANA/863" "IBM863"
let () = alias "IANA/cp863" "IBM863"
let () = alias "IANA/IBM863" "IBM863"
(* Name: IBM864                                              [RFC1345,KXS2] *)
(* Alias: cp864 *)
(* Alias: csIBM864 *)
let () = alias "IANA/csIBM864" "IBM864"
let () = alias "IANA/cp864" "IBM864"
let () = alias "IANA/IBM864" "IBM864"
(* Name: IBM865                                              [RFC1345,KXS2] *)
(* Alias: cp865 *)
(* Alias: 865 *)
(* Alias: csIBM865 *)
let () = alias "IANA/csIBM865" "IBM865"
let () = alias "IANA/865" "IBM865"
let () = alias "IANA/cp865" "IBM865"
let () = alias "IANA/IBM865" "IBM865"
(* Name: IBM866                                                     [Pond] *)
(* Alias: cp866 *)
(* Alias: 866 *)
(* Alias: csIBM866 *)
let () = alias "IANA/csIBM866" "IBM866"
let () = alias "IANA/866" "IBM866"
let () = alias "IANA/cp866" "IBM866"
let () = alias "IANA/IBM866" "IBM866"
(* Name: IBM868                                              [RFC1345,KXS2] *)
(* Alias: CP868 *)
(* Alias: cp-ar *)
(* Alias: csIBM868 *)
let () = alias "IANA/csIBM868" "IBM868"
let () = alias "IANA/cp-ar" "IBM868"
let () = alias "IANA/CP868" "IBM868"
let () = alias "IANA/IBM868" "IBM868"
(* Name: IBM869                                              [RFC1345,KXS2] *)
(* Alias: cp869 *)
(* Alias: 869 *)
(* Alias: cp-gr *)
(* Alias: csIBM869 *)
let () = alias "IANA/csIBM869" "IBM869"
let () = alias "IANA/cp-gr" "IBM869"
let () = alias "IANA/869" "IBM869"
let () = alias "IANA/cp869" "IBM869"
let () = alias "IANA/IBM869" "IBM869"
(* Name: IBM870                                              [RFC1345,KXS2] *)
(* Alias: CP870 *)
(* Alias: ebcdic-cp-roece *)
(* Alias: ebcdic-cp-yu *)
(* Alias: csIBM870 *)
let () = alias "IANA/csIBM870" "IBM870"
let () = alias "IANA/ebcdic-cp-yu" "IBM870"
let () = alias "IANA/ebcdic-cp-roece" "IBM870"
let () = alias "IANA/CP870" "IBM870"
let () = alias "IANA/IBM870" "IBM870"
(* Name: IBM871                                              [RFC1345,KXS2] *)
(* Alias: CP871 *)
(* Alias: ebcdic-cp-is *)
(* Alias: csIBM871 *)
let () = alias "IANA/csIBM871" "IBM871"
let () = alias "IANA/ebcdic-cp-is" "IBM871"
let () = alias "IANA/CP871" "IBM871"
let () = alias "IANA/IBM871" "IBM871"
(* Name: IBM880                                              [RFC1345,KXS2] *)
(* Alias: cp880 *)
(* Alias: EBCDIC-Cyrillic *)
(* Alias: csIBM880 *)
let () = alias "IANA/csIBM880" "IBM880"
let () = alias "IANA/EBCDIC-Cyrillic" "IBM880"
let () = alias "IANA/cp880" "IBM880"
let () = alias "IANA/IBM880" "IBM880"
(* Name: IBM891                                              [RFC1345,KXS2] *)
(* Alias: cp891 *)
(* Alias: csIBM891 *)
let () = alias "IANA/csIBM891" "IBM891"
let () = alias "IANA/cp891" "IBM891"
let () = alias "IANA/IBM891" "IBM891"
(* Name: IBM903                                              [RFC1345,KXS2] *)
(* Alias: cp903 *)
(* Alias: csIBM903 *)
let () = alias "IANA/csIBM903" "IBM903"
let () = alias "IANA/cp903" "IBM903"
let () = alias "IANA/IBM903" "IBM903"
(* Name: IBM904                                              [RFC1345,KXS2] *)
(* Alias: cp904 *)
(* Alias: 904 *)
(* Alias: csIBBM904 *)
let () = alias "IANA/csIBBM904" "IBM904"
let () = alias "IANA/904" "IBM904"
let () = alias "IANA/cp904" "IBM904"
let () = alias "IANA/IBM904" "IBM904"
(* Name: IBM905                                              [RFC1345,KXS2] *)
(* Alias: CP905 *)
(* Alias: ebcdic-cp-tr *)
(* Alias: csIBM905 *)
let () = alias "IANA/csIBM905" "IBM905"
let () = alias "IANA/ebcdic-cp-tr" "IBM905"
let () = alias "IANA/CP905" "IBM905"
let () = alias "IANA/IBM905" "IBM905"
(* Name: IBM918                                              [RFC1345,KXS2] *)
(* Alias: CP918 *)
(* Alias: ebcdic-cp-ar2 *)
(* Alias: csIBM918 *)
let () = alias "IANA/csIBM918" "IBM918"
let () = alias "IANA/ebcdic-cp-ar2" "IBM918"
let () = alias "IANA/CP918" "IBM918"
let () = alias "IANA/IBM918" "IBM918"
(* Name: IBM1026                                             [RFC1345,KXS2] *)
(* Alias: CP1026 *)
(* Alias: csIBM1026 *)
let () = alias "IANA/csIBM1026" "IBM1026"
let () = alias "IANA/CP1026" "IBM1026"
let () = alias "IANA/IBM1026" "IBM1026"
(* Name: EBCDIC-AT-DE                                        [RFC1345,KXS2] *)
(* Alias: csIBMEBCDICATDE *)
let () = alias "IANA/csIBMEBCDICATDE" "EBCDIC-AT-DE"
let () = alias "IANA/EBCDIC-AT-DE" "EBCDIC-AT-DE"
(* Name: EBCDIC-AT-DE-A                                      [RFC1345,KXS2] *)
(* Alias: csEBCDICATDEA *)
let () = alias "IANA/csEBCDICATDEA" "EBCDIC-AT-DE-A"
let () = alias "IANA/EBCDIC-AT-DE-A" "EBCDIC-AT-DE-A"
(* Name: EBCDIC-CA-FR                                        [RFC1345,KXS2] *)
(* Alias: csEBCDICCAFR *)
let () = alias "IANA/csEBCDICCAFR" "EBCDIC-CA-FR"
let () = alias "IANA/EBCDIC-CA-FR" "EBCDIC-CA-FR"
(* Name: EBCDIC-DK-NO                                        [RFC1345,KXS2] *)
(* Alias: csEBCDICDKNO *)
let () = alias "IANA/csEBCDICDKNO" "EBCDIC-DK-NO"
let () = alias "IANA/EBCDIC-DK-NO" "EBCDIC-DK-NO"
(* Name: EBCDIC-DK-NO-A                                      [RFC1345,KXS2] *)
(* Alias: csEBCDICDKNOA *)
let () = alias "IANA/csEBCDICDKNOA" "EBCDIC-DK-NO-A"
let () = alias "IANA/EBCDIC-DK-NO-A" "EBCDIC-DK-NO-A"
(* Name: EBCDIC-FI-SE                                        [RFC1345,KXS2] *)
(* Alias: csEBCDICFISE *)
let () = alias "IANA/csEBCDICFISE" "EBCDIC-FI-SE"
let () = alias "IANA/EBCDIC-FI-SE" "EBCDIC-FI-SE"
(* Name: EBCDIC-FI-SE-A                                      [RFC1345,KXS2] *)
(* Alias: csEBCDICFISEA *)
let () = alias "IANA/csEBCDICFISEA" "EBCDIC-FI-SE-A"
let () = alias "IANA/EBCDIC-FI-SE-A" "EBCDIC-FI-SE-A"
(* Name: EBCDIC-FR                                           [RFC1345,KXS2] *)
(* Alias: csEBCDICFR *)
let () = alias "IANA/csEBCDICFR" "EBCDIC-FR"
let () = alias "IANA/EBCDIC-FR" "EBCDIC-FR"
(* Name: EBCDIC-IT                                           [RFC1345,KXS2] *)
(* Alias: csEBCDICIT *)
let () = alias "IANA/csEBCDICIT" "EBCDIC-IT"
let () = alias "IANA/EBCDIC-IT" "EBCDIC-IT"
(* Name: EBCDIC-PT                                           [RFC1345,KXS2] *)
(* Alias: csEBCDICPT *)
let () = alias "IANA/csEBCDICPT" "EBCDIC-PT"
let () = alias "IANA/EBCDIC-PT" "EBCDIC-PT"
(* Name: EBCDIC-ES                                           [RFC1345,KXS2] *)
(* Alias: csEBCDICES *)
let () = alias "IANA/csEBCDICES" "EBCDIC-ES"
let () = alias "IANA/EBCDIC-ES" "EBCDIC-ES"
(* Name: EBCDIC-ES-A                                         [RFC1345,KXS2] *)
(* Alias: csEBCDICESA *)
let () = alias "IANA/csEBCDICESA" "EBCDIC-ES-A"
let () = alias "IANA/EBCDIC-ES-A" "EBCDIC-ES-A"
(* Name: EBCDIC-ES-S                                         [RFC1345,KXS2] *)
(* Alias: csEBCDICESS *)
let () = alias "IANA/csEBCDICESS" "EBCDIC-ES-S"
let () = alias "IANA/EBCDIC-ES-S" "EBCDIC-ES-S"
(* Name: EBCDIC-UK                                           [RFC1345,KXS2] *)
(* Alias: csEBCDICUK *)
let () = alias "IANA/csEBCDICUK" "EBCDIC-UK"
let () = alias "IANA/EBCDIC-UK" "EBCDIC-UK"
(* Name: EBCDIC-US                                           [RFC1345,KXS2] *)
(* Alias: csEBCDICUS *)
let () = alias "IANA/csEBCDICUS" "EBCDIC-US"
let () = alias "IANA/EBCDIC-US" "EBCDIC-US"
(* Name: UNKNOWN-8BIT                                             [RFC1428] *)
(* Alias: csUnknown8BiT *)
(* Name: MNEMONIC                                            [RFC1345,KXS2] *)
(* Alias: csMnemonic *)
(* Name: MNEM                                                [RFC1345,KXS2] *)
(* Alias: csMnem *)
(* Name: VISCII                                                   [RFC1456] *)
(* Alias: csVISCII *)
let () = alias "IANA/csVISCII" "VISCII"
let () = alias "IANA/VISCII" "VISCII"
(* Name: VIQR                                                     [RFC1456] *)
(* Alias: csVIQR *)
(* Name: KOI8-R  (preferred MIME name)                            [RFC1489] *)
(* Alias: csKOI8R *)
let () = alias "IANA/csKOI8R" "KOI8-R"
let () = alias "IANA/KOI8-R" "KOI8-R"
(* Name: KOI8-U                                                   [RFC2319] *)
let () = alias "IANA/KOI8-U" "KOI8-U"
(* Name: IBM00858 *)
(* Alias: CCSID00858 *)
(* Alias: CP00858 *)
(* Alias: PC-Multilingual-850+euro *)
(* Name: IBM00924 *)
(* Alias: CCSID00924 *)
(* Alias: CP00924 *)
(* Alias: ebcdic-Latin9--euro *)
(* Name: IBM01140 *)
(* Alias: CCSID01140 *)
(* Alias: CP01140 *)
(* Alias: ebcdic-us-37+euro *)
(* Name: IBM01141 *)
(* Alias: CCSID01141 *)
(* Alias: CP01141 *)
(* Alias: ebcdic-de-273+euro *)
(* Name: IBM01142 *)
(* Alias: CCSID01142 *)
(* Alias: CP01142 *)
(* Alias: ebcdic-dk-277+euro *)
(* Alias: ebcdic-no-277+euro *)
(* Name: IBM01143 *)
(* Alias: CCSID01143 *)
(* Alias: CP01143 *)
(* Alias: ebcdic-fi-278+euro *)
(* Alias: ebcdic-se-278+euro *)
(* Name: IBM01144 *)
(* Alias: CCSID01144 *)
(* Alias: CP01144 *)
(* Alias: ebcdic-it-280+euro *)
(* Name: IBM01145 *)
(* Alias: CCSID01145 *)
(* Alias: CP01145 *)
(* Alias: ebcdic-es-284+euro *)
(* Name: IBM01146 *)
(* Alias: CCSID01146 *)
(* Alias: CP01146 *)
(* Alias: ebcdic-gb-285+euro *)
(* Name: IBM01147 *)
(* Alias: CCSID01147 *)
(* Alias: CP01147 *)
(* Alias: ebcdic-fr-297+euro *)
(* Name: IBM01148 *)
(* Alias: CCSID01148 *)
(* Alias: CP01148 *)
(* Alias: ebcdic-international-500+euro *)
(* Name: IBM01149 *)
(* Alias: CCSID01149 *)
(* Alias: CP01149 *)
(* Alias: ebcdic-is-871+euro *)
(* Name: Big5-HKSCS *)
let () = alias "IANA/Big5-HKSCS" "BIG5-HKSCS"
(* Name: UNICODE-1-1                                              [RFC1641] *)
(* Alias: csUnicode11 *)
(* Name: SCSU *)
(* Name: UTF-7                                                    [RFC2152] *)
(* Name: UTF-16BE                                                 [RFC2781] *)
let () = alias "IANA/UTF-16BE" "UTF-16BE"
(* Name: UTF-16LE                                                 [RFC2781] *)
let () = alias "IANA/UTF-16LE" "UTF-16LE"
(* Name: UTF-16                                                   [RFC2781] *)
let () = alias "IANA/UTF-16" "UTF-16"
(* Name: UNICODE-1-1-UTF-7                                        [RFC1642] *)
(* Alias: csUnicode11UTF7 *)
(* Name: UTF-8                                                    [RFC2279] *)
let () = alias "IANA/UTF-8" "UTF-8"
(* Name: ISO-8859-13 *)
let () = alias "IANA/ISO-8859-13" "ISO-8859-13"
(* Name: ISO-8859-14 *)
(* Alias: iso-ir-199 *)
(* Alias: ISO_8859-14:1998 *)
(* Alias: ISO_8859-14 *)
(* Alias: latin8 *)
(* Alias: iso-celtic *)
(* Alias: l8 *)
let () = alias "IANA/l8" "ISO-8859-14"
let () = alias "IANA/iso-celtic" "ISO-8859-14"
let () = alias "IANA/latin8" "ISO-8859-14"
let () = alias "IANA/ISO_8859-14" "ISO-8859-14"
let () = alias "IANA/ISO_8859-14:1998" "ISO-8859-14"
let () = alias "IANA/iso-ir-199" "ISO-8859-14"
let () = alias "IANA/ISO-8859-14" "ISO-8859-14"
(* Name: ISO-8859-15 *)
(* Alias: ISO_8859-15 *)
let () = alias "IANA/ISO_8859-15" "ISO-8859-15"
let () = alias "IANA/ISO-8859-15" "ISO-8859-15"
(* Name: ISO-8859-16 *)
let () = alias "IANA/ISO-8859-16" "ISO-8859-16"
(* Name: JIS_Encoding *)
(* Alias: csJISEncoding *)
(* Name: Shift_JIS  (preferred MIME name) *)
(* Alias: MS_Kanji  *)
(* Alias: csShiftJIS *)
let () = alias "IANA/csShiftJIS" "SHIFT_JIS"
let () = alias "IANA/MS_Kanji" "SHIFT_JIS"
let () = alias "IANA/Shift_JIS" "SHIFT_JIS"
(* Name: Extended_UNIX_Code_Packed_Format_for_Japanese *)
(* Alias: csEUCPkdFmtJapanese *)
(* Alias: EUC-JP  (preferred MIME name) *)
let () = alias "IANA/EUC-JP" "EUC-JP"
let () = alias "IANA/csEUCPkdFmtJapanese" "EUC-JP"
let () = alias "IANA/Extended_UNIX_Code_Packed_Format_for_Japanese" "EUC-JP"
(* Name: Extended_UNIX_Code_Fixed_Width_for_Japanese *)
(* Alias: csEUCFixWidJapanese *)
(* Name: ISO-10646-UCS-Basic *)
(* Alias: csUnicodeASCII *)
(* Name: ISO-10646-Unicode-Latin1 *)
(* Alias: csUnicodeLatin1 *)
(* Alias: ISO-10646 *)
(* Name: ISO-10646-J-1 *)
(* Name: ISO-Unicode-IBM-1261 *)
(* Alias: csUnicodeIBM1261 *)
(* Name: ISO-Unicode-IBM-1268 *)
(* Alias: csUnidoceIBM1268 *)
(* Name: ISO-Unicode-IBM-1276 *)
(* Alias: csUnicodeIBM1276 *)
(* Name: ISO-Unicode-IBM-1264 *)
(* Alias: csUnicodeIBM1264 *)
(* Name: ISO-Unicode-IBM-1265 *)
(* Alias: csUnicodeIBM1265 *)
(* Name: ISO-8859-1-Windows-3.0-Latin-1                           [HP-PCL5]  *)
(* Alias: csWindows30Latin1 *)
(* Name: ISO-8859-1-Windows-3.1-Latin-1                           [HP-PCL5]  *)
(* Alias: csWindows31Latin1 *)
(* Name: ISO-8859-2-Windows-Latin-2                               [HP-PCL5]  *)
(* Alias: csWindows31Latin2 *)
(* Name: ISO-8859-9-Windows-Latin-5                               [HP-PCL5]  *)
(* Alias: csWindows31Latin5 *)
(* Name: Adobe-Standard-Encoding                                    [Adobe] *)
(* Alias: csAdobeStandardEncoding *)
(* Name: Ventura-US                                               [HP-PCL5] *)
(* Alias: csVenturaUS   *)
(* Name: Ventura-International                                    [HP-PCL5] *)
(* Alias: csVenturaInternational *)
(* Name: PC8-Danish-Norwegian                                     [HP-PCL5] *)
(* Alias: csPC8DanishNorwegian *)
(* Name: PC8-Turkish                                              [HP-PCL5] *)
(* Alias: csPC8Turkish *)
(* Name: IBM-Symbols                                             [IBM-CIDT]  *)
(* Alias: csIBMSymbols *)
(* Name: IBM-Thai                                                [IBM-CIDT]  *)
(* Alias: csIBMThai *)
(* Name: HP-Legal                                                 [HP-PCL5] *)
(* Alias: csHPLegal *)
(* Name: HP-Pi-font                                               [HP-PCL5] *)
(* Alias: csHPPiFont *)
(* Name: HP-Math8                                                 [HP-PCL5] *)
(* Alias: csHPMath8 *)
(* Name: Adobe-Symbol-Encoding                                      [Adobe] *)
(* Alias: csHPPSMath *)
(* Name: HP-DeskTop                                               [HP-PCL5] *)
(* Alias: csHPDesktop *)
(* Name: Ventura-Math                                             [HP-PCL5] *)
(* Alias: csVenturaMath *)
(* Name: Microsoft-Publishing                                     [HP-PCL5] *)
(* Alias: csMicrosoftPublishing *)
(* Name: Windows-31J *)
(* Alias: csWindows31J *)
(* Name: GB2312  (preferred MIME name) *)
(* Alias: csGB2312 *)
let () = alias "IANA/csGB2312" "GB2312"
let () = alias "IANA/GB2312" "GB2312"
(* Name: Big5  (preferred MIME name) *)
(* Alias: csBig5 *)
let () = alias "IANA/csBig5" "BIG5"
let () = alias "IANA/Big5" "BIG5"
(* Name: windows-1250 *)
(* Name: windows-1251 *)
(* Name: windows-1252 *)
(* Name: windows-1253 *)
(* Name: windows-1254 *)
(* Name: windows-1255 *)
(* Name: windows-1256 *)
(* Name: windows-1257 *)
(* Name: windows-1258 *)
(* Name: TIS-620 *)
let () = alias "IANA/TIS-620" "TIS-620"
(* Name: HZ-GB-2312 *)
end
