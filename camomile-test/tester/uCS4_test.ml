(* Tests for UCS4 *)
open Camomile
open UPervasives
open Blender
open Bigarray
module UCS4Test = UStorageTest.Make (UCS4)

let _ = UCS4Test.test ~desc:"UCS4 test" ~log:"base_ucs4" ()

let _ =
  random_test ~desc:"UCS4 validate" ~log:"base_utf16_validate"
    ~data:(fun size ->
      Array.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
    ~body:(fun a ->
      expect_pass ~body:(fun () ->
          let s = UCS4.init (Array.length a) (fun i -> a.(i)) in
          (*      UTF16.validate s *)
          expect_equal_app UCS4.validate s (fun () -> ()) ()))

let _ =
  test ~desc:"Malformed ucs4" ~body:(fun () ->
      expect_pass ~body:(fun () ->
          let a = Array1.create int32 c_layout 1 in
          a.{0} <- Int32.of_int ~-1;
          expect_equal_app UCS4.validate a
            (fun _ -> raise UCS4.Malformed_code)
            ()))
