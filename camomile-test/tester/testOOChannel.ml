(* $Id: test-charEncoding.ml,v 1.8 2006/08/13 21:23:08 yori Exp $ *)
(* Copyright 2010 Yamagata Yoriyuki *)
(* Flush test *)

open Printf
open Blender
open CamomileLibraryTest.Camomile
open OOChannel

class buffer_io b = 
object
  method flush () = ()
  method close_out () = ()
  method output s pos len =
    Buffer.add_subbytes b s pos len; len
  method contents = Buffer.contents b
end

let () =
  test
    ~desc:"Output Test"
    ~body:(fun () -> 
	     expect_pass 
	       ~body:
	       (fun () ->
		  let b = Buffer.create 0 in
		  let bio = new buffer_io b in
		  let c = new char_obj_output_channel_of 
		      (bio :> char_output_channel) in
		  let c = new char_output_channel_of c in
		  let put c s = ignore (c#output (Bytes.of_string s) 0 (String.length s)) in
		    (put c "---------------\n");
		    c#flush ();
		    put c "1 first line  1\n";
		    c#flush () ;
		    put c "2 second line 2\n";
		    c#flush ();
		    put c "3 third line  3\n";
		    c#flush ();
		    c#flush ();
		    c#flush ();
		    c#flush ();
		    expect_equal 
		      ~msg:(lazy (sprintf "output %s\n" bio#contents))
		      (Buffer.contents b)
		      "---------------\n\
                       1 first line  1\n\
                       2 second line 2\n\
                       3 third line  3\n"
	       ))

let text = 	
  "---------------\n\
   1 first line  1\n\
   2 second line 2\n\
   3 third line  3\n"

let () =
  test
    ~desc:"channel_of_stream test"
    ~body:(fun () -> 
	     expect_pass 
	       ~body:
	       (fun () ->
		  let s = Stream.of_string text in
		  let c = new channel_of_stream s in
		  let b = Buffer.create 0 in
		    (try while true do Buffer.add_char b (c#get ()) done with
			End_of_file -> ());
		    expect_equal
		      ~msg:(lazy (sprintf "output differ"))
		      (Buffer.contents b)
		      text
	       ))

class otext =
object
  val mutable counter = 0
  method close_in () = ()
  method get () =
    if counter >= String.length text then raise End_of_file else
      begin
	let v = text.[counter] in
	  counter <- counter + 1;
	  v
      end
end

let () =
  test
    ~desc:"stream_of_channel test"
    ~body:(fun () -> 
	     expect_pass 
	       ~body:
	       (fun () ->
		  let s = stream_of_channel (new otext) in
		  let b = Buffer.create 0 in
		    Stream.iter (Buffer.add_char b) s;
		    expect_equal
		      ~msg:(lazy (sprintf "output differ"))
		      (Buffer.contents b)
		      text))

let () =
  test
    ~desc:"char_input_channel_of test"
    ~body:(fun () -> 
	     expect_pass 
	       ~body:
	       (fun () ->
		  let occ = new char_input_channel_of (new otext) in
		  let b = Bytes.create (String.length text) in
		  let n = occ#input b 0 (String.length text) in
		    expect_equal 
		      ~msg:(lazy (sprintf "length %d should be %d" 
				    n (String.length text)))
		      n
		      (String.length text);
		    expect_equal
		      ~msg:(lazy ("output differ"))
		      (Bytes.to_string b)
		      text;
		    expect_equal_app
		      ~msg:(lazy ("EOF"))
		      (fun () -> raise End_of_file) ()
		      (occ#input b 0) 1
	       ))

class otext_char =
object
  val mutable counter = 0
  method close_in () = ()
  method input s p len =
    if counter >= String.length text then raise End_of_file else
    let len' =  String.length text - counter  in
    let len = min len' len in
    String.blit text counter s p len;
    counter <- counter + len;
    len
end

let () =
  test
    ~desc:"char_obj_input_channel_of test"
    ~body:(fun () -> 
	     expect_pass 
	       ~body:
	       (fun () ->
		  let occ = new char_obj_input_channel_of (new otext_char) in
		  let b = Buffer.create 0 in
		  (try while true do Buffer.add_char b (occ#get ()) done with
		    End_of_file -> ());
		  expect_equal
		    ~msg:(lazy (sprintf "output differ"))
		    (Buffer.contents b)
		    text))
