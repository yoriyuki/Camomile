(* $Id: test-normalform.ml,v 1.7 2006/08/13 21:23:08 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

open Camomile
open UPervasives
open Blender
open Printf

let testfile = "unidata/NormalizationTest.txt"
let comment = Str.regexp "^#.*"
let part = Str.regexp "@Part[^1].*"
let part1 = Str.regexp "@Part1.*"
let blank = Str.regexp "[ \t]+"
let semicolon = Str.regexp ";"

let parse_column c =
  let ss = Str.split blank c in
  let ns = List.map (fun s -> int_of_string ("0x" ^ s)) ss in
  let us = List.map (fun n -> uchar_of_int n) ns in
  let s = UText.init (List.length us) (List.nth us) in
  s

let parse_line line =
  let cs = Str.split semicolon line in
  Array.init 5 (fun i -> parse_column (List.nth cs i))

module C = struct
  type t = uchar

  let compare u1 u2 = int_of_uchar u1 - int_of_uchar u2
end

module S = Set.Make (C)

let uset = ref S.empty

let eq t1 t2 =
  if UText.length t1 <> UText.length t2 then false
  else (
    let rec loop i =
      if i >= UText.length t1 then true
      else if UText.get t1 i = UText.get t2 i then loop (i + 1)
      else false
    in
    loop 0)

let print_char u = sprintf "%04X " (int_of_uchar u)

let print_text t =
  let buf = Buffer.create (5 * UText.length t) in
  UText.iter (fun u -> Buffer.add_string buf (print_char u)) t;
  Buffer.contents buf

module NF = UNF.Make ((UText : UnicodeString.Type with type t = UText.t))

let _ =
  read_file (input_filename testfile) (fun c ->
      let is_part1 = ref false in
      try
        while true do
          let line = input_line c in
          if Str.string_match comment line 0 then ()
          else if Str.string_match part line 0 then is_part1 := false
          else if Str.string_match part1 line 0 then is_part1 := true
          else (
            let c = parse_line line in
            if !is_part1 then uset := S.add (UText.get c.(0) 0) !uset;

            (* NFC test *)
            test ~desc:"NFC" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    let nfc = Array.map NF.nfc c in
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c2 %s <> NFC(c1) %s"
                             (print_text c.(1))
                             (print_text nfc.(0))))
                      (eq c.(1) nfc.(0));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c2 %s <> NFC(c2) %s"
                             (print_text c.(1))
                             (print_text nfc.(1))))
                      (eq c.(1) nfc.(1));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c2 %s <> NFC(c3) %s"
                             (print_text c.(1))
                             (print_text nfc.(2))))
                      (eq c.(1) nfc.(2));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c4 %s <> NFC(c4) %s"
                             (print_text c.(3))
                             (print_text nfc.(3))))
                      (eq c.(3) nfc.(3));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c4 %s <> NFC(c5) %s"
                             (print_text c.(3))
                             (print_text nfc.(4))))
                      (eq c.(3) nfc.(4))));

            (* NFD test *)
            test ~desc:"NFD" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    let nfd = Array.map NF.nfd c in
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c3 %s <> NFD(c1) %s"
                             (print_text c.(2))
                             (print_text nfd.(0))))
                      (eq c.(2) nfd.(0));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c3 %s <> NFD(c2) %s"
                             (print_text c.(2))
                             (print_text nfd.(1))))
                      (eq c.(2) nfd.(1));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c3 %s <> NFD(c3) %s"
                             (print_text c.(2))
                             (print_text nfd.(2))))
                      (eq c.(2) nfd.(2));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c5 %s <> NFD(c4) %s"
                             (print_text c.(4))
                             (print_text nfd.(3))))
                      (eq c.(4) nfd.(3));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c5 %s <> NDF(c5) %s"
                             (print_text c.(4))
                             (print_text nfd.(4))))
                      (eq c.(4) nfd.(4))));

            (* NFKC test *)
            test ~desc:"NFKC" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    let nfkc = Array.map NF.nfkc c in
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c4 %s <> NFKC(c1) %s"
                             (print_text c.(3))
                             (print_text nfkc.(0))))
                      (eq c.(3) nfkc.(0));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c4 %s <> NFKC(c2) %s"
                             (print_text c.(3))
                             (print_text nfkc.(1))))
                      (eq c.(3) nfkc.(1));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c4 %s <> NFKC(c3) %s"
                             (print_text c.(3))
                             (print_text nfkc.(2))))
                      (eq c.(3) nfkc.(2));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c4 %s <> NFKC(c4) %s"
                             (print_text c.(3))
                             (print_text nfkc.(3))))
                      (eq c.(3) nfkc.(3));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c4 %s <> NFKC(c5) %s"
                             (print_text c.(3))
                             (print_text nfkc.(4))))
                      (eq c.(3) nfkc.(4))));

            (* NFKD test *)
            test ~desc:"NFKD" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    let nfkd = Array.map NF.nfkd c in
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c5 %s <> NFKD(c1) %s"
                             (print_text c.(4))
                             (print_text nfkd.(0))))
                      (eq c.(4) nfkd.(0));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c5 %s <> NFKD(c2) %s"
                             (print_text c.(4))
                             (print_text nfkd.(1))))
                      (eq c.(4) nfkd.(1));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c5 %s <> NFKD(c3) %s"
                             (print_text c.(4))
                             (print_text nfkd.(2))))
                      (eq c.(4) nfkd.(2));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c5 %s <> NFKD(c4) %s"
                             (print_text c.(4))
                             (print_text nfkd.(2))))
                      (eq c.(4) nfkd.(3));
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "c5 %s NKKD(c5) %s"
                             (print_text c.(4))
                             (print_text nfkd.(4))))
                      (eq c.(4) nfkd.(4)))))
        done
      with End_of_file -> ())

open UCharInfo

let _ =
  let cat = general_category in
  for i = 0 to (1 lsl 21) - 1 do
    let u = uchar_of_int i in
    match cat u with
      | `Co | `Cn -> ()
      | _ ->
          if S.mem u !uset then ()
          else (
            let s = UText.init 1 (fun _ -> u) in
            let nfc = NF.nfc s in
            let nfd = NF.nfd s in
            let nfkc = NF.nfkc s in
            let nfkd = NF.nfkd s in

            test ~desc:"NFC" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "%s <> %s" (print_text s) (print_text nfc)))
                      (eq s nfc)));

            test ~desc:"NFD" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "%s <> %s" (print_text s) (print_text nfd)))
                      (eq s nfd)));

            test ~desc:"NFKC" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "%s <> %s" (print_text s) (print_text nfkc)))
                      (eq s nfkc)));

            test ~desc:"NFKD" ~body:(fun () ->
                expect_pass ~body:(fun () ->
                    expect_true
                      ~msg:
                        (lazy
                          (sprintf "%s <> %s" (print_text s) (print_text nfkd)))
                      (eq s nfkd))))
  done

let _ =
  random_test ~desc:"NFD ?= NFD(NFC)" ~log:"test-normalform-nfd"
    ~data:(fun size ->
      Array.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
    ~body:(fun a ->
      expect_pass ~body:(fun () ->
          let t = UText.init (Array.length a) (Array.get a) in
          expect_equal
            ~msg:(lazy "NFD <> NFD(NFC)")
            (NF.nfd t)
            (NF.nfd (NF.nfc t))))

let _ =
  random_test ~desc:"NFKD ?= NFKD(NFKC)" ~log:"test-normalform-nfd"
    ~data:(fun size ->
      Array.init size (fun _ -> uchar_of_int (Random.int 0x8000000)))
    ~body:(fun a ->
      expect_pass ~body:(fun () ->
          let t = UText.init (Array.length a) (Array.get a) in
          expect_equal
            ~msg:(lazy "NFKD <> NFKD(NFKC)")
            (NF.nfkd t)
            (NF.nfd (NF.nfkc t))))
