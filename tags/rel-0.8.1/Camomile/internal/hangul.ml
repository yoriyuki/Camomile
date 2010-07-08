(* $Id: hangul.ml,v 1.3 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

let sbase = 0xac00
let lbase = 0x1100
let vbase = 0x1161
let tbase = 0x11a7
let lcount = 19
let vcount = 21
let tcount = 28
let ncount = vcount * tcount
let scount = lcount * ncount

let decompose u =
  let n = UChar.uint_code u - sbase in
  let l = lbase + n / ncount in
  let v = vbase + (n mod ncount) / tcount in
  let t = tbase + n mod tcount in
  if t = tbase then
    [UChar.chr_of_uint l; UChar.chr_of_uint v]
  else
    [UChar.chr_of_uint l; UChar.chr_of_uint v; UChar.chr_of_uint t]

let add_decomposition x u =
  let n = UChar.uint_code u - sbase in
  if n < 0 || n >= scount then XString.add_char x u else begin
    XString.add_char x (UChar.chr_of_uint (lbase + n / ncount));
    XString.add_char x (UChar.chr_of_uint (vbase + (n mod ncount) / tcount));
    let t = tbase + n mod tcount in
    if t = tbase then () else
    XString.add_char x (UChar.chr_of_uint t)
  end

let compose x' x =
  if XString.length x = 0 then () else
  let pos = ref 0 in
  let last = ref (UChar.uint_code (XString.get x 0)) in
  for i = 1 to XString.length x - 1 do
    let n = UChar.uint_code (XString.get x i) in
    let l = !last - lbase in
    let v = n - vbase in
    if 0 <= l && l < lcount	&& 0 <= v && v < vcount then
      last := sbase + (l * vcount + v) * tcount
    else
      let s = !last - sbase in
      let t = n - tbase in
      if 
	0 <= s && s < scount && s mod tcount = 0 && 
	0 <= t && t < tcount
      then
	last := !last + t
      else begin
	XString.set x' !pos (UChar.chr_of_uint !last);
	last := n;
	incr pos
      end
  done;
  XString.set x' !pos (UChar.chr_of_uint !last);
  XString.shrink x' (!pos + 1)
    
