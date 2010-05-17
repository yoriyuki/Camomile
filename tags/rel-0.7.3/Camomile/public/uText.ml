(* $Id: uText.ml,v 1.2 2004/06/05 16:42:07 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

include Array
type mutability = [ `Mutable | `Immutable ]
      
type 'a text = UChar.t array
type utext = [`Imutable] text
type t = utext
type ustring = [`Mutable] text

let utext_of_ustring = Array.copy
let ustring_of_utext = Array.copy

type index = int

let look s i = get s i
let nth _ i = i
let first _ = 0
let last s = Array.length s - 1
let out_of_range s i = i < 0 || i >= Array.length s
let next _ i = i + 1
let prev _ i = i - 1
let move _ i n = i + n
let compare_index _ (i : int) (j : int) = i - j

let make len init = Array.make len init

let init_ustring = init

let of_string s = init (String.length s) (fun i -> UChar.of_char s.[i])

let rec compare_aux i t1 t2 =
  if i >= length t1 then
    if i >= length t2 then 0 else ~-1
  else if i >= length t2 then 1 else
  match UChar.compare (get t1 i) (get t2 i) with
    0 -> compare_aux (i + 1) t1 t2
  | sgn -> sgn

let compare t1 t2 = compare_aux 0 t1 t2

module Buf =
  struct
    include XArray

    type buf = UChar.t xarray

    let create bufsize = XArray.make ~bufsize 0 (UChar.chr_of_uint 0)
    let contents = array_of
    let contents_string = array_of
    let add_char = add_element
    let add_string = add_array
    let add_buffer = add_xarray
  end
