(* Tests for UTF16 *)
open CamomileLibrary.Default.Camomile
open UPervasives
open Blender
open Printf
open Bigarray

module UTF16Conv = CharEncoding.Make (UTF16)
module UTF16Test = UStorageTest.Make (UTF16)

let char_gen _ =
  let n = match Random.int 3 with
    0 -> Random.int 0xd800
  | 1 -> 0xe000 + Random.int (0xfffe - 0xe000)
  | 2 -> 0x10000 + Random.int (0x110000 - 0x10000)
  | _ -> assert false in
  uchar_of_int n

let _ = UTF16Test.test ~desc:"UTF16 test" ~log:"base_utf16" ~char_gen ()

let string_of_int16array a =
  let s = String.create (2 + 2 * Bigarray.Array1.dim a) in
  s.[0] <- Char.chr 0xfe; s.[1] <- Char.chr 0xff;
  for i = 0 to Bigarray.Array1.dim a - 1 do
    s.[2 * i + 2] <- Char.chr (a.{i} lsr 8);
    s.[2 * i + 3] <- Char.chr (a.{i} land 255)
  done;
  s

let _ = random_test
    ~desc:"UTF16 validate"
    ~log:"base_utf16_validate"
    ~data:(fun size -> Array.init size char_gen)
    ~body:(fun a -> expect_pass (fun () ->
      let s = UTF16.init (Array.length a) (fun i -> a.(i)) in
      let s' = UTF16Conv.encode CharEncoding.utf16be s in
      expect_true (s' = string_of_int16array s);
(*      UTF16.validate s *)
      expect_equal_app UTF16.validate s (fun () -> ()) ()))

let _ = test ~desc:"Malformed utf16 1" ~body:(fun () ->
  expect_pass ~body:(fun () ->
    let a = Array1.create int16_unsigned c_layout 2 in
    a.{0} <- 0xdc01; a.{1} <- 0xd801;
    expect_equal_app 
      UTF16.validate a (fun _ -> raise UTF16.Malformed_code) ()))
    
let _ = test ~desc:"Malformed utf16 2" ~body:(fun () ->
  expect_pass ~body:(fun () ->
    let a = Array1.create int16_unsigned c_layout 2 in
    a.{0} <- 0xd801; a.{1} <- 0x0300;
    expect_equal_app 
      UTF16.validate a (fun _ -> raise UTF16.Malformed_code) ()))

let _ = test ~desc:"Malformed utf16 3" ~body:(fun () ->
  expect_pass ~body:(fun () ->
    let a = Array1.create int16_unsigned c_layout 2 in
    a.{0} <- 0xfffe; a.{1} <- 0xfeff;
    expect_equal_app 
      UTF16.validate a (fun _ -> raise UTF16.Malformed_code) ()))

let _ = test ~desc:"Out_of_range  utf16 1" ~body:(fun () ->
  expect_pass ~body:(fun () ->
    expect_equal_app
      (UTF16.init 1) (fun _ -> uchar_of_int 0xd801)
      (fun _ -> raise UTF16.Out_of_range) ()))

let _ = test ~desc:"Out_of_range  utf16 2" ~body:(fun () ->
  expect_pass ~body:(fun () ->
    expect_equal_app
      (UTF16.init 1) (fun _ -> uchar_of_int 0xdfff)
      (fun _ -> raise UTF16.Out_of_range) ()))

let _ = test ~desc:"Out_of_range  utf16 3" ~body:(fun () ->
  expect_pass ~body:(fun () ->
    expect_equal_app
      (UTF16.init 1) (fun _ -> uchar_of_int 0xfffe)
      (fun _ -> raise UTF16.Out_of_range) ()))

let _ = test ~desc:"Out_of_range  utf16 1" ~body:(fun () ->
  expect_pass ~body:(fun () ->
    expect_equal_app
      (UTF16.init 1) (fun _ -> uchar_of_int 0x4fffffff)
      (fun _ -> raise UTF16.Out_of_range) ()))
