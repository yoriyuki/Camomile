(* $Id: uCS4.ml,v 1.8 2004/09/04 16:09:02 yori Exp $ *)
(* Copyright 2002, 2003, 2004 Yamagata Yoriyuki. distributed with LGPL *)

open Bigarray

(* UCS4 encoded string. the type is bigarray of 32-bit integers. *)
type t = (int32, int32_elt, c_layout) Array1.t

type index = int

exception Malformed_code

let rec validate_aux (a:t) i =
  if i >= Array1.dim a then () else
  match Int32.to_int (Int32.shift_right a.{i} 31) with
    0 -> validate_aux a (i + 1)
  | _ -> raise Malformed_code

let validate (a:t) = validate_aux a 0

let look (a:t) i : UChar.t = UChar.chr_of_uint (Int32.to_int a.{i})

let length (a:t) = Array1.dim a

let next _ i = i + 1

let prev _ i = i - 1

let move (a:t) i c = i + c

let first _ = 0

let last (a:t) = Array1.dim a - 1

let out_of_range (a:t) i = i < 0 || i >= Array1.dim a

let compare_index _ i j = i - j

let nth (a:t) c = c

let get (a:t) c = look a c

let rec iter_aux proc (a:t) i =
  if i >= Array1.dim a then () else begin
    proc (look a i);
    iter_aux proc a (i + 1)
  end

let iter proc (a:t) = iter_aux proc a 0

let init len f =
  let a = Array1.create int32 c_layout len in
  for i = 0 to len - 1 do 
    a.{i} <- Int32.of_int (UChar.uint_code (f i)) 
  done;
  a

module Buf = struct

  type buf = {init_size : int; mutable pos : index; mutable contents : t}

  let create n =
    let contents = Array1.create int32 c_layout n in
    {init_size = n; pos = 0; contents = contents}

  let clear buf = buf.pos <- 0

  let reset buf =
    buf.contents <- Array1.create int32 c_layout buf.init_size;
    buf.pos <- 0

  let contents buf =
    let a = Array1.create int32 c_layout buf.pos in
    let src = Array1.sub buf.contents 0 buf.pos in
    Array1.blit src a;
    a

  let resize buf n =
    if Array1.dim buf.contents >= n then () else
    let a = Array1.create int32 c_layout (2 * n) in
    let a' = Array1.sub a 0 (Array1.dim buf.contents) in
    Array1.blit buf.contents a';
    buf.contents <- a

  let add_char buf u =
    resize buf (buf.pos + 1);
    buf.contents.{buf.pos} <- Int32.of_int (UChar.uint_code u);
    buf.pos <- buf.pos + 1

  let add_string buf (a:t) =
    let len = buf.pos + Array1.dim a in
    resize buf len;
    let b = Array1.sub buf.contents buf.pos (Array1.dim a) in
    Array1.blit a b;
    buf.pos <- len

  let add_buffer buf1 buf2 =
    let len = buf1.pos + buf2.pos in
    resize buf1 len;
    let a = Array1.sub buf2.contents 0 buf2.pos in
    let b = Array1.sub buf1.contents buf1.pos buf2.pos in
    Array1.blit a b;
    buf1.pos <- len
end

let compare (a:t) (b:t) = 
  match Array1.dim a - Array1.dim b with
    0 -> Pervasives.compare a b
  | sgn -> sgn
