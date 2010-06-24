(* Tests for UStorage *)

open CamomileLibraryDefault.Camomile
open UPervasives
open Blender
open Printf

module Make (Text : UnicodeString.Type) =
  struct

    let test
	?(char_gen = (fun _ -> uchar_of_int (Random.int 0x8000000)))
	~desc ~log ()
	= random_test ~desc ~log
	~data:(fun size -> Array.init size char_gen)
	~body:(fun a -> expect_pass (fun () ->

	  (* tests for ustring *)
	  let s = Text.init (Array.length a) (Array.get a) in
	  let len = Text.length s in
	  expect_equal ~msg:(lazy "init") len (Array.length a);

	  for c = 0 to 50 do
	    let i = Random.int len in
	    expect_equal 
	      ~msg:(lazy (sprintf "set/get: location %d" i))
	      (Text.get s i) a.(i)
	  done;

	  let r = ref 0 in
	  Text.iter (fun u ->
	    expect_equal
	      ~msg:(lazy (sprintf "iter: location %d" !r))
	      u a.(!r);
	    incr r) s;
	  expect_equal 
	    ~msg:(lazy (sprintf "iter: count %d" !r))
	    !r len;

	  let cur = ref (Text.nth s 0) in
	  let r = ref 0 in
	  while not (Text.out_of_range s !cur) do
	    expect_equal
	      ~msg:(lazy (sprintf "index look: location %d" !r))
	      (Text.look s !cur)
	      a.(!r);
	    cur := Text.next s !cur;
	    incr r
	  done;
	  expect_equal 
	    ~msg:(lazy (sprintf "index: count %d" !r))
	    !r len;

	  for i = 0 to 100 do
	    let pos = Random.int len in
	    let cur = Text.nth s pos in
	    expect_equal 
	      ~msg:(lazy (sprintf "nth: location %d" pos))
	      (Text.look s cur)
	      a.(pos);
	    if pos = 0 then () else
	    let cur' = Text.prev s cur in
	    expect_equal_app
	      ~msg:(lazy (sprintf "cursor prev : location %d" pos))
	      (fun () -> Text.look s cur') ()
	      (fun i -> a.(i)) (pos - 1);
	    if pos = len - 1 then () else
	    let cur' = Text.next s cur in
	    expect_equal_app
	      ~msg:(lazy (sprintf "cursor next : location %d" pos))
	      (fun () -> Text.look s cur') ()
	      (fun i -> a.(i)) (pos + 1);
(*	let m = Random.int 32 in
   if pos + m < 0 || pos + m >= len then () else
   let cur' = Text.move s cur m in
   expect_equal_app
   ~msg:(lazy (sprintf "cursor move : location %d, move %d" pos m))
   (fun () -> Text.look s cur') ()
   (fun i -> a.(i)) (pos + m); *)
	  done;

	  (* Buffer *)
	  let b = Text.Buf.create 0 in

	  let p = Random.int len in
	  let s1 = Text.init p (Array.get a) in
	  let s2 = Text.init (len - p) (fun x -> Array.get a (p + x)) in
	  
	  Text.Buf.add_string b s1;
	  Text.Buf.add_string b s2;
	  let s' = Text.Buf.contents b in
	  expect_true ~msg:(lazy "step 1") (Text.compare s s' = 0);

	  Text.Buf.clear b;
	  Text.iter (Text.Buf.add_char b) s;
	  let s' = Text.Buf.contents b in
	  expect_true ~msg:(lazy "step 2") (Text.compare s s' = 0);

	  Text.Buf.clear b;
	  let b' = Text.Buf.create 16 in
	  let pos = Random.int len in
	  for i = 0 to len - 1 do
	    if i < pos then
	      Text.Buf.add_char b a.(i)
	    else
	      Text.Buf.add_char b' a.(i)
	  done;
	  Text.Buf.add_buffer b b';
	  let s' = Text.Buf.contents b in
	  expect_true ~msg:(lazy "step 3") (Text.compare s s' = 0);

	  Text.Buf.reset b;
	  Text.Buf.add_string b s;
	  let s' = Text.Buf.contents b in
	  expect_true ~msg:(lazy "step 4") (Text.compare s s' = 0);))
	
  end
