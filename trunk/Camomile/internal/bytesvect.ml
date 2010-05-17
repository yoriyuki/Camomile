(* $Id: bytesvect.ml,v 1.5 2003/04/27 17:47:59 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki *)

type t = 
    {len : int; 
     mutable bytes : int; 
     mutable contents : string;
     mutable id : int}

(* get b v i : read b-bytes from the k-th byte of v *)
(* b <= 4 *) 
let rec get_raw acc b v k =
  if b = 0 then acc else
  let acc' = (acc lsl 8) lor (Char.code v.[k]) in
  get_raw acc' (pred b) v (succ k)

let get v i = get_raw 0 v.bytes v.contents (i * v.bytes)

let rec unsafe_get_raw acc b v k =
  if b = 0 then acc else
  let acc' = (acc lsl 8) lor (Char.code (String.unsafe_get v k)) in
  unsafe_get_raw acc' (pred b) v (succ k)

let unsafe_get v i = unsafe_get_raw 0 v.bytes v.contents (i * v.bytes)

let rec set_bytes_raw b v k n =
  let c = (n lsr ((b - 1) lsl 3)) land 255 in
  v.[k] <- Char.chr c;
  if b > 1 then set_bytes_raw (b - 1) v (k + 1) n

let rec bytes n = if n = 0 then 0 else 1 + bytes (n lsr 8)

let set v i n =
  let b = bytes n in
  if v.bytes < b then
    let save = {len = v.len; bytes = v.bytes; contents = v.contents; id = 0} in
    let len = (String.length v.contents) / v.bytes in
    v.contents <- String.make (len * b) (Char.chr 0);
    v.bytes <- b;
    for i = 0 to len - 1 do 
      set_bytes_raw b v.contents (b * i) (get save i)
    done
  else ();
  set_bytes_raw v.bytes v.contents (i * v.bytes) n

let make i0 df =
  let b = max (bytes df) 1 in
  let v = 
    {len = i0; 
     bytes = b; 
     contents = String.make (b * i0) (Char.chr 0); 
     id = 0} 
  in
  for i = 0 to i0 - 1 do set v i df done; v

let copy v =
  {len = v.len;
   bytes = v.bytes;
   contents = String.copy v.contents;
   id = 0}

let iteri proc v =
  let len = (String.length v.contents) / v.bytes in
  for i = 0 to len - 1 do
    proc i (get v i)
  done

let length v = v.len

let set_id v id = v.id <- id
let id v = v.id
