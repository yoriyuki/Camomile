(* $Id: test-base.ml,v 1.18 2006/08/13 21:23:08 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

open Printf
open Blender
open CamomileLibraryDyn.Camomile
open UPervasives

let random_pair () = 
  let a0 = Random.int 0x8000000 in
  let b0 = Random.int 0x8000000 in
  (min a0 b0, max a0 b0)


(* Tests for UChar *)

let _ = random_test
    ~desc:"char <-> uchar"
    ~log:"char_uchar"
    ~data:(fun _ -> Char.chr (Random.int 0x100))
    ~body:(fun c ->
      expect_pass ~body:(fun () ->
	let u = UChar.of_char c in
	let c' = UChar.char_of u in
	expect_equal c c'))

let _ = random_test
    ~desc:"int <-> uchar"
    ~log:"int_uchar"
    ~data:(fun _ ->
      match Random.int 3 with
	0 -> Random.int 0xd800
      | 1 -> 0xe000 + Random.int 8190
      | 2 -> 0x10000 + Random.int 1048576
      | _ -> assert false)
    ~body:(fun n ->
      expect_pass ~body:(fun () ->
	let u = UChar.chr n in
	let n' = UChar.code u in
	expect_equal n n'))

let _ = random_test
    ~desc:"uint <-> uchar"
    ~log:"uint_uchar"
    ~data:(fun _ -> Random.int 0x8000000)
    ~body:(fun n ->
      expect_pass ~body:(fun () ->
	let u = UChar.chr_of_uint n in
	let n' = UChar.uint_code u in
	expect_equal n n'))

let _ = 
  test
    ~desc:"comparison"
    ~body:(fun () ->expect_pass ~body:(fun () ->
      let u1 = uchar_of_int 0x00 in
      let u2 = uchar_of_int 0x300 in
      let u3 = uchar_of_int 0x40000 in
      let u4 = uchar_of_int 0x3ffffff in
      let u5 = uchar_of_int 0x7ffffff in
      expect_true ~msg:(lazy "u1 >= u2") (UChar.compare u1 u2 < -1);
      expect_true ~msg:(lazy "u2 >= u3") (UChar.compare u2 u3 < -1);
      expect_true ~msg:(lazy "u3 >= u4") (UChar.compare u3 u4 < -1);
      expect_true ~msg:(lazy "u4 >= u5") (UChar.compare u4 u5 < -1)))

(* Tests for USet, UMap *)

module StdUSet = Set.Make (UChar)
module StdUMap = Map.Make (UChar)

let random_uchars size =
  let r = ref StdUSet.empty in
  for i = 1 to size / 2 do
    r := StdUSet.add (uchar_of_int (Random.int 0x10000)) !r
  done;
  for i = 1 to size - size / 2 do
    r := StdUSet.add (uchar_of_int (Random.int 0x8000000)) !r
  done;
  !r

let uset_of_stdset us = StdUSet.fold USet.add us USet.empty

let test_uset ~desc uset us =
  StdUSet.iter (fun u ->
    let n = int_of_uchar u in
    expect_true
      ~msg:(lazy (sprintf "\\u%08x is missing in %s" n desc))
      (USet.mem u uset);
    let u = uchar_of_int (n + 1) in
    expect_equal_app
      ~msg:(lazy (sprintf 
		    "u in us1 is %b but u in uset1 is %b for u = \\u%08x in %s"
		    (StdUSet.mem u us)
		    (USet.mem u uset)
		    (n + 1)
		    desc))
      (StdUSet.mem u) us
      (USet.mem u) uset;
    let u = uchar_of_int (n - 1) in
    expect_equal_app
      ~msg:(lazy (sprintf 
		    "u in us1 is %b but u in uset1 is %b for u = \\u%08x in %s"
		    (StdUSet.mem u us)
		    (USet.mem u uset)
		    (n - 1)
		    desc))
      (StdUSet.mem u) us
      (USet.mem u) uset)
    us

let test_range ~desc uset =
  let a, b = random_pair () in
  let uset' = USet.add_range (uchar_of_int a) (uchar_of_int b) uset in 
    for i = a to b do
      expect_true
	~msg:(lazy (sprintf "\\u%08x is missing in %s" i desc))
	(USet.mem (uchar_of_int i) uset')
    done;
    USet.iter 
      (fun u -> 
	 let i = int_of_uchar u in
	 expect_true
	   ~msg:(lazy (sprintf "\\u%08x is missing in %s" i desc))
	   (USet.mem u uset'))
      uset;
    USet.iter (fun u ->
		 let i = int_of_uchar u in
		 expect_true
		   ~msg:(lazy (sprintf "\\u%08x is in %s but souldn't" i desc))
		   (USet.mem u uset or a <= i && i <= b))
      uset'

let _ = 
  random_test
    ~desc:"USet"
    ~log:"uset"
    ~data:(fun size -> (random_uchars size, random_uchars size))
    ~body:(fun (us1, us2) -> expect_pass (fun () ->
      let uset1 = uset_of_stdset us1 in
      test_uset ~desc:"uset1" uset1 us1;
      let uset2 = uset_of_stdset us2 in
      test_uset ~desc:"uset2" uset2 us2;
      test_uset ~desc:"union" 
	(USet.union uset1 uset2) 
	(StdUSet.union us1 us2);
      test_uset ~desc:"inter"
	(USet.inter uset1 uset2)
	(StdUSet.inter us1 us2);
      test_uset ~desc:"diff"
	(USet.diff uset1 uset2)
	(StdUSet.diff us1 us2);
      test_range ~desc:"range" uset1))
      
let random_umap size =
  let r = ref StdUMap.empty in
  for i = 1 to size / 2 do
    let v = 1.0 /. (float_of_int (Random.int 10)) in
    r := StdUMap.add (uchar_of_int (Random.int 0x10000)) v !r
  done;
  for i = 1 to size - size / 2 do
    let v = 1.0 /. (float_of_int (Random.int 10)) in
    r := StdUMap.add (uchar_of_int (Random.int 0x8000000)) v !r
  done;
  !r

let umap_of_stdmap m = 
  StdUMap.fold UMap.add m UMap.empty

let _ = 
  random_test
    ~desc:"UMep"
    ~log:"umap"
    ~data:(fun size -> random_umap size)
    ~body:(fun m -> expect_pass (fun () ->
      let umap = umap_of_stdmap m in
      StdUMap.iter (fun u v ->
	let n = int_of_uchar u in
	expect_equal_app
	  (StdUMap.find u) m
	  (UMap.find u) umap;
	let u = uchar_of_int (n + 1) in
	expect_equal_app
	  (StdUMap.find u) m
	  (UMap.find u) umap;
	let u = uchar_of_int (n - 1) in
	expect_equal_app
	  (StdUMap.find u) m
	  (UMap.find u) umap)
	m))

(* Interval Association List *)
let rec assoc k al = 
  match al with
      [] ->  raise Not_found
    | (k1, k2, v) :: r -> if k1 <= k && k <= k2 then v else assoc k r


let rec random_assoc size =
  if size <= 0 then [] else
    let a, b = random_pair () in
      (a, b, Random.int 16) :: random_assoc (size - 1)

let umap_of_assoc al = 
  let al = List.rev al in
  let rec f al umap =
    match al with
	[] -> umap
      | (k1, k2, v) :: r ->
	    f r (UMap.add_range (UChar.chr k1) (UChar.chr k2) v umap) 
  in
    f al UMap.empty
	      
let _ = 
  random_test
    ~desc:"UMep interval"
    ~log:"umap interval"
    ~data:(fun size -> random_assoc size)
    ~body:(fun al -> expect_pass (fun () ->
      let umap = umap_of_assoc al in
      for i = 0 to 0x8000000 do
	expect_equal_app
	  (UMap.find (UChar.chr i)) umap
	  (assoc i) al
      done))

(* domain, set_to_map, map_to_set *)

let _ =
  random_test
    ~desc:"UMap <-> USet"
    ~log:"UMap <-> USet"
    ~data:(fun size -> random_assoc size)
    ~body:(fun al -> expect_pass (fun () ->
      let umap = umap_of_assoc al in
      let dom = UMap.domain umap in
      let umap' = UMap.set_to_map dom 1 in
      let umap'' = UMap.map (fun _ -> 1) umap in
      for i = 0 to 0x8000000 do
	expect_equal_app
	  (UMap.find (UChar.chr i)) umap'
	  (UMap.find (UChar.chr i)) umap''
      done))
      

(* Tests for UCharTbl *)

let test_tbl utbl uset exc =
  USet.iter 
    (fun u -> 
      expect_true 
	~msg:(lazy (sprintf "\\u%08x is missing" (int_of_uchar u)))
	(UCharTbl.Bool.get utbl u)) 
    uset;
  for i = 0 to 100 do
    let u = uchar_of_int (Random.int 0x8000000) in
    expect_equal_app
      ~msg:(lazy 
	     (sprintf 
		"%s for UCharTbl while %s for USet for \\u%0x08"
		(if UCharTbl.Bool.get utbl u then "true" else "false")
		(if USet.mem u uset then "true" else "false")
		(int_of_uchar u)))
      (UCharTbl.Bool.get utbl) u
      (fun u -> USet.mem u uset) u
  done;
  List.iter
    (fun u ->
      expect_true
	~msg:(lazy (sprintf "\\u%08x is not removed" (int_of_uchar u)))
	(not (UCharTbl.Bool.get utbl u)))
    exc

let _ = 
  random_test
    ~desc:"UCharTbl.Bool"
    ~log:"uchartbl_bool"
    ~data:(fun size -> uset_of_stdset (random_uchars size))
    ~body:(fun uset -> expect_pass (fun () ->
      let utbl = UCharTbl.Bool.of_set uset in
      test_tbl utbl uset []))

module FHash = struct 
  type t = float 
  let equal = (=) 
  let hash = Hashtbl.hash 
end

module Tbl = UCharTbl.Make (FHash)

let test_map tbl umap u =
  let v1 = try UMap.find u umap with Not_found -> 0.0 in
  let v2 = Tbl.get tbl u in
  expect_equal
    ~msg:(lazy (sprintf "%f in map while %f in tbl" v1 v2))
    v1 v2

let _= random_test
    ~desc:"UCharTbl"
    ~log:"uchartbl"
    ~data:(fun size -> umap_of_stdmap (random_umap size))
    ~body:(fun umap -> expect_pass (fun () ->
      let tbl = Tbl.of_map 0.0 umap in
      UMap.iter (fun u v ->
	test_map tbl umap u;
	let n = int_of_uchar u in
	let u = uchar_of_int (n + 1) in
	test_map tbl umap u;
	let u = uchar_of_int (n - 1) in
	test_map tbl umap u)
	umap))
      
(* Tests for UText *)

let _ = random_test
    ~desc:"UText.of_string"
    ~log:"base_utext_of_string"
    ~data:(fun size ->
      let s = String.create size in
      for i = 0 to size - 1 do
	s.[i] <- Char.chr (Random.int 0x100)
      done;
      s)
    ~body:(fun s -> expect_pass (fun () ->
      let text = UText.of_string s in
      expect_equal (String.length s) (UText.length text);
      for i = 0 to String.length s - 1 do
	let c1 = s.[i] in
	let c2 = UChar.char_of (UText.get text i) in
	expect_equal ~msg:(lazy (sprintf "location %d" i)) c1 c2;
      done))

let _ = random_test
    ~desc:"UText.make"
    ~log:"base_utext_make"
    ~data:(fun size -> Random.int 0x8000000, size)
    ~body:(fun (n, size) -> expect_pass (fun () ->
      let u = uchar_of_int n in
      let text = UText.make size u in
      for i = 0 to size - 1 do
	let n' = int_of_uchar (UText.get text i) in
	expect_equal ~msg:(lazy (sprintf "location %d: %04x %04x" i n n')) 
	  n n'
      done))

module UTextTest = UStorageTest.Make ((UText : UnicodeString.Type))

let _ = UTextTest.test ~desc:"UText test" ~log:"base_utext" ()

let _ = random_test
    ~desc:"UText.ustring"
    ~log:"base_UText_ustring"
    ~data:(fun size -> 
      Array.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
    ~body:(fun a -> expect_pass (fun () ->

      (* tests for ustring *)
      let s = UText.make (Array.length a) (uchar_of_int 0) in
      expect_equal ~msg:(lazy "create") (UText.length s) (Array.length a);

      for i = 0 to UText.length s - 1 do UText.set s i a.(i) done;
      for i = 0 to UText.length s - 1 do
	expect_equal 
	  ~msg:(lazy (sprintf "ustring set/get: location %d" i))
	  (UText.get s i) a.(i)
      done;

      let r = ref 0 in
      UText.iter (fun u ->
	expect_equal
	  ~msg:(lazy (sprintf "ustring iter: location %d" !r))
	  u a.(!r);
	incr r) s;
      expect_equal 
	~msg:(lazy (sprintf "ustring iter: count %d" !r))
	!r (UText.length s);

      let cur = ref (UText.nth s 0) in
      let r = ref 0 in
      while not (UText.out_of_range s !cur) do
	expect_equal
	  ~msg:(lazy (sprintf "ustring cursor get: location %d" !r))
	  (UText.look s !cur)
	  a.(!r);
	cur := UText.next s !cur;
	incr r
      done;
      expect_equal 
	~msg:(lazy (sprintf "ustring index: count %d" !r))
	!r (UText.length s);

      for i = 0 to 100 do
	let pos = Random.int (UText.length s) in
	let cur = UText.nth s pos in
	expect_equal 
	  ~msg:(lazy (sprintf "ustring nth: location %d" pos))
	  (UText.look s cur)
	  a.(pos);
	if pos = 0 then () else
	let cur' = UText.prev s cur in
	expect_equal_app
	  ~msg:(lazy (sprintf "ustring cursor next : location %d" pos))
	  (fun () -> UText.look s cur') ()
	  (fun i -> a.(i)) (pos - 1);
	if pos = UText.length s - 1 then () else
	let cur' = UText.next s cur in
	expect_equal_app
	  ~msg:(lazy (sprintf "ustring cursor next : location %d" pos))
	  (fun () -> UText.look s cur') ()
	  (fun i -> a.(i)) (pos + 1);
      done))

let _ = random_test
    ~desc:"UText.*"
    ~log:"base_utext"
    ~data:(fun size -> 
      Array.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
    ~body:(fun a -> expect_pass (fun () ->
      
      (* tests for ustring *)
      let s = UText.make (Array.length a) (uchar_of_int 0) in
      for i = 0 to UText.length s - 1 do UText.set s i a.(i) done;
      
      let s' = UText.copy s in
      for i = 0 to UText.length s - 1 do
	expect_equal
	  ~msg:(lazy (sprintf "copy: location %d" i))
	  (UText.get s i)
	  (UText.get s' i)
      done;

      let pos = Random.int (UText.length s) in
      let len = Random.int (UText.length s - pos + 1) in
      let s' = UText.sub s pos len in
      for i = 0 to len - 1 do
	expect_equal
	  ~msg:(lazy (sprintf 
		  "sub for ustring: location %d from %d of length %d" 
		  i pos len))
	  (UText.get s (pos + i))
	  (UText.get s' i)
      done;

      let s' = UText.copy s in
      let u = uchar_of_int (Random.int 0x8000000) in
      let pos = Random.int (UText.length s) in
      let len = Random.int (UText.length s - pos + 1) in
      UText.fill s' pos len u;
      for i = 0 to len - 1 do
	expect_equal
	  ~msg:(lazy (sprintf
		  "fill: location %d from %d of length %d"
		  i pos len))
	  (UText.get s' (pos + i))
	  u
      done;

      let len' = Random.int (UText.length s) + UText.length s in
      let s' = UText.make len' (uchar_of_int 0) in
      let pos = Random.int (UText.length s' - UText.length s + 1) in
      UText.blit s 0 s' pos (UText.length s);
      for i = 0 to UText.length s - 1 do
	expect_equal
	  ~msg:(lazy (sprintf "blit: location %d from %d" i pos))
	  (UText.get s' (pos + i))
	  (UText.get s i)
      done;

      let pos = Random.int (UText.length s) in
      let s1 = UText.sub s 0 pos in
      let s2 = UText.sub s pos (UText.length s - pos) in
      let s' = UText.append s1 s2 in
      for i = 0 to UText.length s - 1 do
	expect_equal
	  ~msg:(lazy (sprintf "append %d: location %d" pos i))
	  (UText.get s i)
	  (UText.get s' i)
      done;

      (* tests for utext *)
      
      let s = UText.utext_of_ustring s in

      let pos = Random.int (UText.length s) in
      let len = Random.int (UText.length s - pos + 1) in
      let s' = UText.sub s pos len in
      for i = 0 to len - 1 do
	expect_equal
	  ~msg:(lazy (sprintf 
		  "sub for ustring: location %d from %d of length %d" 
		  i pos len))
	  (UText.get s (pos + i))
	  (UText.get s' i)
      done;

      let pos = Random.int (UText.length s) in
      let s1 = UText.sub s 0 pos in
      let s2 = UText.sub s pos (UText.length s - pos) in
      let s' = UText.append s1 s2 in
      for i = 0 to UText.length s - 1 do
	expect_equal
	  ~msg:(lazy (sprintf "append %d: location %d" pos i))
	  (UText.get s i)
	  (UText.get s' i)
      done))

(* Tests for XString *)

module XStringTest = UStorageTest.Make ((XString : UnicodeString.Type))

let _ = XStringTest.test ~desc:"XString" ~log:"base_xstring" ()

let _ = random_test
    ~desc:"XString extra"
    ~log:"base_xstring_extra"
    ~data:(fun size -> 
      Array.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
    ~body:(fun a -> expect_pass (fun () ->
      
      let size = Array.length a in
      (* tests for ustring *)
      let s = XString.make 0 (uchar_of_int 0) in

      for i = 0 to size - 1 do XString.set s i a.(i) done;
      expect_equal
	~msg:(lazy (sprintf "xstring length: %d %d" (XString.length s) size))
	(XString.length s)
	size;
      for i = 0 to size - 1 do
	expect_equal 
	  ~msg:(lazy (sprintf "xstring set/get: location %d" i))
	  (XString.get s i) a.(i)
      done;

      let s' = XString.copy s in
      for i = 0 to size - 1 do
	expect_equal 
	  ~msg:(lazy (sprintf "xstring copy: location %d" i))
	  (XString.get s i) (XString.get s' i)
      done;

      let pos = Random.int size in
      let len = Random.int (size - pos + 1) in
      let s' = XString.sub s pos len in
      for i = 0 to len - 1 do
	expect_equal
	  ~msg:(lazy (sprintf 
		  "xstring sub: location %d from %d of length %d" 
		  i pos len))
	  (XString.get s (pos + i))
	  (XString.get s' i)
      done;

      let s'' = XString.append (XString.sub s 0 pos) s' in
      for i = 0 to pos + len - 1 do
	expect_equal
	  ~msg:(lazy (sprintf
		  "xstring append: appended in %d, location %d"
		  pos i))
	  (XString.get s'' i)
	  (XString.get s i)
      done;

      let r = ref 0 in
      XString.iter (fun u ->
	expect_equal
	  ~msg:(lazy (sprintf "xstring iter: location %d" !r))
	  u a.(!r);
	incr r) s;
      expect_equal 
	~msg:(lazy (sprintf "xstring iter: count %d" !r))
	!r size))

(* Tests for SubText *)

module SubTextTest (Text : UnicodeString.Type) = struct
  module SubText = SubText.Make (Text)
  module Test1 = UStorageTest.Make (SubText)

  let test ~desc ~log = 
    Test1.test ~desc:(desc ^ ": test1") ~log:(log ^ "_test1") ();
    random_test
      ~desc:(desc ^ ": test2") 
      ~log:(log ^ "_test2")
      ~data:(fun size -> 
	Text.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
      ~body:(fun t -> expect_pass (fun () ->

	let len = Text.length t in
	let pos = Random.int len in
	let len' = Random.int (len - pos) in
	let cur = Text.nth t pos in
	let cur' = Text.move t cur len' in
	let sub = SubText.refer t cur cur' in

	let r1 = ref cur in
	let r2 = ref (SubText.nth sub 0) in
	for i = 0 to len' - 1 do
	  expect_equal
	    ~msg:(lazy (sprintf "sub: location %d" i))
	    (Text.look t !r1)
	    (SubText.look sub !r2);
	  r1 := Text.next t !r1;
	  r2 := SubText.next sub !r2
	done;

	let t' = SubText.excerpt sub in
	let r1 = ref cur in
	let r2 = ref (Text.nth t' 0) in
	for i = 0 to len' - 1 do
	  expect_equal
	    ~msg:(lazy (sprintf "sub excerpt: location %d" i))
	    (Text.look t !r1)
	    (Text.look t' !r2);
	  r1 := Text.next t !r1;
	  r2 := Text.next t' !r2
	done;))
end

module SubUText = SubTextTest (UText)
let _ = SubUText.test ~desc:"subutext" ~log:"base_subutext"

module SubXString = SubTextTest (XString)
let _ = SubXString.test ~desc:"subxstring" ~log:"base_subxstring"

module SubUTF8 = SubTextTest (UTF8)
let _ = SubUTF8.test ~desc:"subutf8" ~log:"base_subutf8"
