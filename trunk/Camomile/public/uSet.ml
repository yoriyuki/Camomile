(* $Id: uSet.ml,v 1.2 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

include ISet

external uset_of_iset : ISet.t -> t = "%identity"
external iset_of_uset : t -> ISet.t = "%identity"

let mem u s = ISet.mem (UChar.uint_code u) s

let add u s = ISet.add (UChar.uint_code u) s

let add_range u1 u2 s = 
  ISet.add_range (UChar.uint_code u1) (UChar.uint_code u2) s

let singleton u = ISet.singleton (UChar.uint_code u)

let remove u s = ISet.remove (UChar.uint_code u) s

let remove_range u1 u2 s = 
  ISet.remove_range (UChar.uint_code u1) (UChar.uint_code u2) s

let from u s = ISet.from (UChar.uint_code u) s
let after u s = ISet.after (UChar.uint_code u) s

let until u s = ISet.until (UChar.uint_code u) s
let before u s = ISet.before (UChar.uint_code u) s

let iter f s = ISet.iter (fun n -> f (UChar.chr_of_uint n)) s

let iter_range f s =
  let f' n1 n2 = 
    f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) in
  ISet.iter_range f' s

let fold f s a =
  let f' n a = f (UChar.chr_of_uint n) a in
  ISet.fold f' s a

let fold_range f s a =
  let f' n1 n2 a = 
    f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) a in
  ISet.fold_range f' s a

let for_all p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.for_all p' s

let exists p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.exists p' s

let filter p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.filter p' s

let partition p s =
  let p' n = p (UChar.chr_of_uint n) in
  ISet.partition p' s

let elements s =
  List.map UChar.chr_of_uint (ISet.elements s)

let ranges s =
  let f (n1, n2) = 
    (UChar.chr_of_uint n1, 
     UChar.chr_of_uint n2) in
  List.map f (ISet.ranges s)

let min_elt s = UChar.chr_of_uint (ISet.min_elt s)
let max_elt s = UChar.chr_of_uint (ISet.max_elt s)
let choose s = UChar.chr_of_uint (ISet.choose s)

let uset_of_iset s = s
let iset_of_uset s = s
