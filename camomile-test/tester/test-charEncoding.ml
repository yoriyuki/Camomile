(* $Id: test-charEncoding.ml,v 1.8 2006/08/13 21:23:08 yori Exp $ *)
(* Copyright 2002, 2010 Yamagata Yoriyuki *)

open Printf
open Blender
open CamomileLibraryTest.Camomile
open UPervasives
open CharEncoding
open OOChannel
module UL = ULine.Make(UTF8)

let print_uchar u = sprintf "\\u%08x" (int_of_uchar u)

let enc_filename file = input_filename (Filename.concat "data/enc" file)

let test_decoding enc_name file utf8_file =
  let file = enc_filename file in
  let utf8_file = enc_filename utf8_file in
  let enc = of_name enc_name in
  let src = new in_channel enc (open_in_bin file) in
  let dst = new in_channel utf8 (open_in_bin utf8_file) in
  test  
    ~desc:("Decoding: "^ enc_name) 
    ~body:(fun () -> expect_pass ~body:(fun () ->
      let c = ref 0 in
      (try while true do
	incr c;
	let u = dst#get() in
	expect_equal_app
	  ~msg:(lazy (sprintf "location %d" !c))
	  ~printer:print_uchar
	  (fun () -> u) ()
	  (fun () -> src#get()) ()
      done with End_of_file -> ());
      expect_equal_app
	~msg:(lazy "EOF")
	~printer:print_uchar
	(fun () -> dst#get()) ()
	(fun () -> src#get()) ()))

let test_encoding enc_name utf8_file = 
  let enc = of_name enc_name in
  let utf8_file = enc_filename utf8_file in
  let us = stream_of_channel (new in_channel utf8 (open_in_bin utf8_file)) in
  let us0 = stream_of_channel (new in_channel utf8 (open_in_bin utf8_file)) in
  let cs = char_stream_of enc us0 in
  let us' = ustream_of enc cs in
  test
    ~desc:("Encoding: " ^ enc_name)
    ~body:(fun () -> expect_pass ~body:(fun () ->
      Stream.iter (fun u ->
	expect_equal_app
	  ~msg:(lazy (sprintf "location %d" (Stream.count us)))
	  ~printer:print_uchar
	  (fun () -> u) ()
	  Stream.next us')
	us;
      expect_equal_app
	~msg:(lazy "EOF")
	~printer:print_uchar
	Stream.next us
	Stream.next us'))

let test_enc file =
  let enc_name = 
    let s = Filename.basename file in
    try
      let i = String.index s '#' in
      String.sub s 0 i
    with Not_found -> s in
  try 
    let utf8_file = file ^ "..UTF8" in
    test_decoding enc_name file utf8_file;
    test_encoding enc_name utf8_file
  with
    Not_found ->
      prerr_string ("Warning: "^ enc_name ^ " is not supported");
      prerr_newline ()
  | Sys_error _ as exn -> 
      prerr_string ("Exception: " ^ (Printexc.to_string exn));
      prerr_newline ()
	
let filter name =
  not (Str.string_match (Str.regexp ".*\\.\\..*") name 0)

let _ = foreach_file (input_filename "data/enc") ~filter test_enc

(* test for Japanese auto detection *)

let _ = 
    test_decoding "jauto" "ISO-2022-JP" "ISO-2022-JP..UTF8"

let _ = 
    test_decoding "jauto" "ISO-2022-JP-2" "ISO-2022-JP-2..UTF8"

let _ = 
    test_decoding "jauto" "EUC-JP" "EUC-JP..UTF8"


class buffer_io b = 
object (self)
  method flush () = ()
  method close_out () = ()
  method output s pos len =
    Buffer.add_subbytes b s pos len; len
end

let () =
  test
    ~desc:"Output Test"
    ~body:(fun () -> 
	     expect_pass 
	       ~body:
	       (fun () ->
		 let b = Buffer.create 0 in
		 let oooch = new buffer_io b in
		 let ceuoc = 
		   new CharEncoding.uchar_output_channel_of 
		     CharEncoding.utf8 oooch in
		 let outln = new UL.output_line ceuoc in
		 outln#put "---------------";
		 outln#flush () ;
		 outln#put "1 first line  1" ;
		 outln#flush () ;
		 outln#put "2 second line 2" ;
		 outln#flush () ;
		 outln#put "3 third line  3" ;
		 outln#flush () ;
		 outln#flush () ;
		 outln#flush () ; 
		 expect_equal 
		   ~msg:(lazy (sprintf "output %s\n" (Buffer.contents b)))
		   (Buffer.contents b)
		   "---------------\n\
		   1 first line  1\n\
		   2 second line 2\n\
		   3 third line  3\n"))
  
