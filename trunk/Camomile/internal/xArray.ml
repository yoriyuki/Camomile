(** XArray : extensible arrays *)
(* Copyright (C) 2002, 2003 Yamagata Yoriyuki. *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yoriyuki.yamagata@aist.go.jp *)


type 'a xarray = 
    {mutable len : int; 
     mutable buf : 'a array; 
     default : 'a}

type 'a t = 'a xarray

let expand x len =
  if Array.length x.buf >= len then () else
  let buf' = Array.make (2 * len) x.default in
  Array.blit x.buf 0 buf' 0 x.len;
  x.buf <- buf'

let get x i =
  if 0 <= i && i < x.len then x.buf.(i) else
  invalid_arg "XArray.get"

let set x i e =
  if i < x.len then x.buf.(i) <- e else begin
    expand x (i + 1);
    if x.len < i then Array.fill x.buf x.len (i - x.len) x.default;
    x.buf.(i) <- e;
    x.len <- i + 1
  end

let length x = x.len
    
let init ?bufsize len def f =
  let buf = 
    Array.make 
      (match bufsize with None -> len | Some n -> n)
      def
  in
  for i = 0 to len - 1 do buf.(i) <- f i done;
  {len = len;
   buf = buf;
   default = def}

type index = int
let nth _ i = i
let first _ = 0
let last x = length x - 1
let look = get
let out_of_range x i = i < 0 || x.len <= i
let next _ i = i + 1
let prev _ i = i - 1
let move _ i n = i + n
let compare_index _ i j = i - j

let make ?bufsize len default =
  let buf = 
    Array.make 
      (match bufsize with None -> len | Some n -> n)
      default
  in
  {len = len; buf = buf; default = default}

let clear x = x.len <- 0

let reset x = x.len <- 0; x.buf <- Array.make 0 x.default

let copy x = {len = x.len; buf = Array.copy x.buf; default = x.default}
    
let sub x pos len = 
  {len = len; 
   buf = Array.sub x.buf pos len; 
   default = x.default}

let add_element x e =
  expand x (x.len + 1);
  x.buf.(x.len) <- e;
  x.len <- x.len + 1

let add_array x a =
  expand x (x.len + Array.length a);
  Array.blit a 0 x.buf x.len (Array.length a);
  x.len <- x.len + Array.length a

let add_xarray x1 x2 =
  expand x1 (x1.len + x2.len);
  Array.blit x2.buf 0 x1.buf x1.len x2.len;
  x1.len <- x1.len + x2.len

let shrink x len = x.len <- min x.len (max 0 len)
    
let append x1 x2 =
  let buf = Array.make (x1.len + x2.len) x1.default in
  Array.blit x1.buf 0 buf 0 x1.len;
  Array.blit x2.buf 0 buf x1.len x2.len;
  {len = x1.len + x2.len;
   buf = buf;
   default = x1.default}

let array_of x = Array.sub x.buf 0 x.len
    
let iter proc x = for i = 0 to x.len - 1 do proc x.buf.(i) done
