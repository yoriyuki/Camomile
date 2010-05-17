(* Tests for UTF8 *)
open CamomileLibrary.Default.Camomile
open UPervasives
open Blender
open Printf

module UTF8Conv = CharEncoding.Make (UTF8)
module UTF8Test = UStorageTest.Make (UTF8)

let _ = UTF8Test.test ~desc:"UTF8 test" ~log:"base_utf8" ()

let _ = random_test
    ~desc:"UTF8 validate"
    ~log:"base_utf8_validate"
    ~data:(fun size -> 
      Array.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
    ~body:(fun a -> expect_pass (fun () ->
      let s = UTF8.init (Array.length a) (fun i -> a.(i)) in
      let s' = UTF8Conv.encode CharEncoding.utf8 s in
      expect_true (UTF8.compare s s' = 0);
(*      UTF8.validate s *)
      expect_equal_app UTF8.validate s (fun () -> ()) ()))

let _ = test ~desc:"Malformed utf8" ~body:(fun () ->
  expect_pass ~body:(fun () -> 
    expect_equal_app 
      UTF8.validate "\192\128" 
      (fun _ -> raise UTF8.Malformed_code) ()))
