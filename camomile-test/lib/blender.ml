(* $Id: blender.ml,v 1.6 2003/06/28 06:10:31 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)
(* Copyright (c) 2001 Patrick Doane. *)

open Printf

(* global settings *)

let input_root  = ref "../"
let output_root = ref (".")
let verbose     = ref 2
let repetition = ref 10
let data_size = ref 5000
let handle_exception = ref true

(* utilities *)

let bracket
    (before : 'a -> 'b)
    (init : 'a) 
    (f : 'b -> 'c)
    (after : 'b -> unit) =
  let x = before init in
  let res =
    try f x with exn -> after x; raise exn
  in
  after x;
  res

(* directory management *)

let input_filename s  = Filename.concat !input_root s
let output_filename s = Filename.concat !output_root s

let read_file s f  = bracket open_in s f close_in
let write_file s f = bracket open_out s f close_out

let foreach_file dir ?(filter=(fun _ -> true)) f =
  bracket Unix.opendir dir (fun h ->
    try
      while true do
	let basename = Unix.readdir h in
	match basename with
	  | "." | ".." -> ()
	  | _ -> if filter basename then f basename
      done
    with End_of_file -> ()
  ) 
    Unix.closedir

(* test result codes *)

type result = 
  | Pass
  | UPass
  | Fail of string
  | XFail
  | Unresolved
  | Untested
  | Unsupported of string

let is_expected = function
  | Pass | XFail -> true
  | _ -> false

let to_string = function
  | Pass           -> "Pass"
  | UPass          -> "Unexpected Pass"
  | Fail ""        -> "Fail"
  | Fail m         -> "Fail - " ^ m
  | XFail          -> "Expected Fail"
  | Unresolved     -> "Unresolved"
  | Untested       -> "Untested"
  | Unsupported "" -> "Unsupported"
  | Unsupported m  -> "Unsupported - " ^ m

let count_pass         = ref 0
let count_upass        = ref 0
let count_fail         = ref 0
let count_xfail        = ref 0
let count_unresolved   = ref 0
let count_untested     = ref 0
let count_unsupported  = ref 0

let count_var = function
  | Pass          -> count_pass
  | UPass         -> count_upass
  | Fail _        -> count_fail
  | XFail         -> count_xfail
  | Unresolved    -> count_unresolved
  | Untested      -> count_untested
  | Unsupported _ -> count_unsupported

(* Tests *)

let do_test ~desc ~rep ~body =
  for _ = 1 to rep do
    let result = 
      if !handle_exception then
	try
	  body ()
	with exn ->
	  printf "  uncaught exception: %s\n" (Printexc.to_string exn);
	  Unresolved
      else
	body ()
    in
    incr (count_var result);
    if not (is_expected result) || !verbose > 2 then begin
      printf "  %s: %s\n" (to_string result) desc;
      flush stdout
    end
  done

let repeat_test ~desc ~body = do_test ~desc ~rep:!repetition ~body

let test ~desc ~body = do_test ~desc ~rep:1 ~body

let random_test ~desc ~log ~data ~body =
  let count = ref 0 in
  let result = ref Unresolved in
  repeat_test ~desc ~body:(fun () ->
    bracket 
      data
      !data_size
      (fun data -> result := body data; !result)
      (fun data ->
	match !result with
	  UPass | Fail _ | Unresolved ->
	    let logname = 
	      if !repetition = 1 then 
		log 
	      else 
		log ^ "." ^ (string_of_int !count)
	    in
	    let c = open_out_bin (output_filename logname) in
	    output_value c data;
	    close_out c
	| _ -> ()))
    

(* Expect based testing *)

exception EFail of string

let fail m = raise (EFail m)

let expect_equal ?msg ?printer x y =
  if x=y then ()
  else match printer with
    | None ->
        fail (match msg with
              | None -> "not equal"
              | Some msg -> Lazy.force msg)
    | Some p ->
        let test_msg = match msg with None -> "" 
	| Some msg -> (Lazy.force msg) ^ " " in
	fail (sprintf "expected %s\n          but got %s\n    in test %s"
	  (p x) (p y) test_msg)

let expect_equal_app ?msg ?printer f x g y =
  let test_msg = match msg with None -> "" 
  | Some msg -> (Lazy.force msg) ^ " " in
  try
    let x' = f x in
    (try
      let y' = g y in
      expect_equal ?msg ?printer x' y'
    with 
    | EFail _ as exn -> raise exn
    | exn2 ->
      let exn2 = "exception " ^ (Printexc.to_string exn2) in
      match printer with
      | None ->
          fail (sprintf "unexpected %s\n    in test %s"
                 exn2 test_msg)
      | Some p ->
          fail (sprintf
                "expected %s\n          but got %s\n    in test %s"
                  (p x') exn2 test_msg))
  with
    | EFail _ as exn -> raise exn
    | exn1 ->
        let exn1 = "exception " ^ (Printexc.to_string exn1) in
        (try
          let y' = g y in
          match printer with
          | None ->
              fail (sprintf
                 "expected %s\n          but got no exception\n    in test %s"
	         exn1 test_msg)
          | Some p ->
              fail (sprintf
                 "expected %s\n          but got %s\n    in test %s"
	         exn1 (p y') test_msg)
        with 
          | EFail _ as exn -> raise exn
          | exn2 ->
              let exn2 = "exception " ^ (Printexc.to_string exn2) in
              expect_equal ?msg ~printer:(fun s -> s) exn1 exn2)

let expect_true ?msg test =
  if test then ()
  else fail (match msg with None -> "" | Some m -> Lazy.force  m)

let expect_pass ~(body : unit -> unit) = 
  try body (); Pass with EFail m -> Fail m

let expect_fail ~(body : unit -> unit) = 
  try body (); UPass with EFail _ -> XFail

(* Front end *)

let day =   [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let month = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; 
	       "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let date_time () =
  let t = Unix.time () in
  let lt = Unix.localtime t in

  (* Generate date string *)
  Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d"
    day.(lt.Unix.tm_wday)
    lt.Unix.tm_mday
    month.(lt.Unix.tm_mon)
    (1900 + lt.Unix.tm_year)
    lt.Unix.tm_hour
    lt.Unix.tm_min
    lt.Unix.tm_sec

let user () =
  (* [getlogin] can be unreliable on some systems so try to access
   * the password database first. *)
  try
    (Unix.getpwuid (Unix.getuid ())).Unix.pw_name
  with Not_found ->
    try
      Unix.getlogin ()
    with _ -> "unknown"

let header () =
  if !verbose > 1 then
    printf "\n-------------------------------------------------------------\n";
  if !verbose > 0 then
    printf "%s\n" Sys.executable_name;
  if !verbose > 1 then begin
    printf "-------------------------------------------------------------\n";
    printf "        user:  %s\n" (user ());
    printf "        time:  %s\n" (date_time ());
    printf "          os:  %s\n" Sys.os_type;
    printf "  input root:  %s\n" !input_root;
    printf " output root:  %s\n" !output_root;
    printf "-------------------------------------------------------------\n\n";
  end;
  flush stdout

let t0 = ref 0.0

let summary () =
  let count_review =
    !count_upass + !count_fail + !count_unresolved + !count_untested
  in
  if !verbose > 0 then
    begin
      let result count desc =
	printf "%4d %s\n" count desc
      in
      let cond_result count desc =
	if count > 0 || !verbose > 2 then result count desc
      in
      if !verbose > 1 then
	printf
	  "\n-------------------------------------------------------------\n";
      printf "Summary:\n";
      result      !count_pass        "passed";
      cond_result !count_upass       "unexpectedly passed";
      cond_result !count_fail        "failed";
      cond_result !count_xfail       "failed as expected";
      cond_result !count_unresolved  "unresolved";
      cond_result !count_untested    "untested";
      cond_result !count_unsupported "unsupported";
      if !verbose > 1 then begin
	print_char '\n';
	result      count_review       "testcase(s) to review";
      end;
      if !verbose > 1 then begin
	print_char '\n';
	Printf.printf "The tests take %f seconds\n" ((Sys.time ()) -. !t0);
      end;
      printf
	"-------------------------------------------------------------\n\n";
      flush stdout;
    end;
  count_review

let _ = Arg.parse 
    [("--input",
      Arg.String ((:=) input_root),
      "<dir>\t\tspecify input root");

     ("--output",
      Arg.String ((:=) output_root),
      "<dir>\t\tspecify output root");

     ("--verbose",
      Arg.Int ((:=) verbose),
      "<level>\tset verbosity");

     ("--repetition", 
      Arg.Int ((:=) repetition),
      "<count>\trepeats tests");

     ("--datasize",
      Arg.Int ((:=) data_size),
      "<number>\tstandard size of random data");

     ("--handle-exception",
      Arg.Set handle_exception,
      "catch the uncaught exception");

     ("--throw-exception",
      Arg.Clear handle_exception,
      "throw the uncaught exception")

   ]
    (fun s -> raise (Arg.Bad (sprintf "Unexpected Argument %s" s)))
    ""

let _ = header ()

let main () =
  let count_review = summary () in
  exit (if count_review > 0 then 1 else 0)

let _ = t0 := Sys.time ()
