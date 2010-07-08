(* $Id: bitsvect.ml,v 1.6 2003/04/27 17:47:59 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

let rec bits n = if n = 0 then 0 else 1 + bits (n lsr 1)

type t = 
    {len : int; 
     mutable bits : int; 
     mutable contents : string;
     mutable id : int}

(* get v i : read the i-th element of v *)
(* b < 8 *)
let get vect i =
  let b = vect.bits in
  let v = vect.contents in
  let k = b * i in
  let m0 = ((Char.code v.[k lsr 3]) lsr (k land 0b111)) in
  let m =
    ((Char.code v.[(k lsr 3) + 1]) lsl (8 - (k land 0b111))) lor m0
  in m land ((1 lsl b) - 1)

let unsafe_get vect i =
  let b = vect.bits in
  let v = vect.contents in
  let k = b * i in
  let j = k lsr 3 in
  let v1 = Char.code (String.unsafe_get v j) in
  let v2 = Char.code (String.unsafe_get v (j + 1)) in
  let j' = k land 0b111 in
  (v2 lsl (8 - j')) lor (v1 lsr j') land ((1 lsl b) - 1)

let set_raw vect i n =
  let b = vect.bits in
  let v = vect.contents in
  let i0 = (i * b) lsr 3 in
  let i1 = (i * b) land 7 in
  let masq1 = (1 lsl b - 1) in
  let c0 = (Char.code v.[i0]) land (lnot (masq1 lsl i1)) in
  let c0' = c0 lor ((n lsl i1) land 255) in
  v.[i0] <- Char.chr c0';
  if b + i1 <= 8 then () else
  let masq2 = (1 lsl (b + i1 - 8)) - 1 in
  let c1 = (Char.code v.[i0 + 1]) land (lnot masq2) in
  let c1' = c1 lor (n lsr (8 - i1)) in
  v.[i0 + 1] <- Char.chr c1'

let bits_to_bytes b = b / 8 + 2

let set vect i n =
  let b = bits n in
  if vect.bits < b then
    let save = 
      {len = vect.len; 
       bits = vect.bits; 
       contents = vect.contents;
       id = 0} 
    in
    let len = vect.len in
    vect.contents <-  String.make (bits_to_bytes (b * len)) (Char.chr 0);
    vect.bits <- b;
    for i = 0 to len - 1 do set_raw vect i (get save i) done;
  else ();
  set_raw vect i n

let make i0 df = 
  let b = max (bits df) 1 in
  let v = 
    {len = i0;
     bits = b; 
     contents = String.make (bits_to_bytes (b * i0)) (Char.chr 0);
     id = 0} 
  in
  for i = 0 to i0 - 1 do set v i df done; v

let copy v =
  {len = v.len;
   bits = v.bits;
   contents = String.copy v.contents;
   id = 0}

let iteri proc v =
  for i = 0 to v.len - 1 do
    proc i (get v i)
  done

let length v = v.len

let set_id v id = v.id <- id
let id v = v.id
