(* $Id: test-caseMap.ml,v 1.4 2006/08/13 21:23:08 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki *)

open Printf
open Blender
open CamomileLibrary.Main.Camomile

module UTF8Casing = CaseMap.Make (UTF8)

let _ = random_test
    ~desc:"ASCII"
    ~log:"caseMap_ASCII"
    ~data:(fun size ->
      let s = String.create size in
      for i = 0 to size - 1 do
	s.[i] <- Char.chr (Random.int 0x80)
      done;
      s)
    ~body:(fun s -> expect_pass (fun () ->
      let s1 = UTF8Casing.lowercase s in
      let s2 = String.lowercase s in
      expect_equal
	~msg:(lazy (sprintf "lowercase: %s <> %s" s1 s2))
	s1
	s2;
      let s1 = UTF8Casing.uppercase s in
      let s2 = String.uppercase s in
      expect_equal
	~msg:(lazy (sprintf "uppercase: %s <> %s" s1 s2))
	s1
	s2))

let _ = test ~desc:"German" ~body:(fun () -> expect_pass ~body:(fun () ->
  let s = "daß mann" in
  let s1 = UTF8Casing.lowercase s in
  let s2 = UTF8Casing.uppercase s in
  let s3 = UTF8Casing.titlecase s in
  expect_equal 
    ~msg:(lazy (sprintf "lowercase of %s: %s" s s1))
    s1 
    "daß mann";
  expect_equal 
    ~msg:(lazy (sprintf "uppercase of %s: %s" s s2))
    s2
    "DASS MANN";
  expect_equal 
    ~msg:(lazy (sprintf "titlecase of %s: %s" s s3))
    s3
    "Daß Mann"))

let _ = test ~desc:"Greek" ~body:(fun () -> expect_pass ~body:(fun () ->
  let s = "ΜΙΣΕΙ ΓΑΡ Ο ΘΕΟΣ ΤΑΣ ΑΓΑΝ ΠΡΟΘΥΜΙΑΣ." in
  let s1 = UTF8Casing.lowercase s in
  let s2 = UTF8Casing.uppercase s in
  let s3 = UTF8Casing.titlecase s in
  expect_equal 
    ~msg:(lazy (sprintf "lowercase of %s: %s" s s1))
    s1 
    "μισει γαρ ο θεος τας αγαν προθυμιας.";
  expect_equal 
    ~msg:(lazy (sprintf "uppercase of %s: %s" s s2))
    s2
    "ΜΙΣΕΙ ΓΑΡ Ο ΘΕΟΣ ΤΑΣ ΑΓΑΝ ΠΡΟΘΥΜΙΑΣ.";
  expect_equal 
    ~msg:(lazy (sprintf "titlecase of %s: %s" s s3))
    s3
    "Μισει Γαρ Ο Θεος Τας Αγαν Προθυμιας."))
		
let _ = test ~desc:"Turkish" ~body:(fun () -> expect_pass ~body:(fun () ->
  let s = "Iİıi" in
  let s1 = UTF8Casing.lowercase ~locale:"tr" s in
  let s2 = UTF8Casing.uppercase ~locale:"tr" s in
  let s3 = UTF8Casing.titlecase ~locale:"tr" s in
  expect_equal 
    ~msg:(lazy (sprintf "lowercase of %s: %s" s s1))
    s1 
    "ıiıi";
  expect_equal 
    ~msg:(lazy (sprintf "uppercase of %s: %s" s s2))
    s2
    "IİIİ";
  expect_equal 
    ~msg:(lazy (sprintf "titlecase of %s: %s" s s3))
    s3
    "Iiıi"))

(* Fix me: Tests for Azeri,  Lithuanian. *)
