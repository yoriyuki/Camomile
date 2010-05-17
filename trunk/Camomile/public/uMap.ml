(* $Id: uMap.ml,v 1.2 2004/09/04 16:07:38 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

include IMap

let umap_of_imap m = m
let imap_of_umap m = m

let add ?eq u v m = IMap.add ?eq (UChar.uint_code u) v m

let add_range ?eq u1 u2 v m =
  IMap.add_range ?eq (UChar.uint_code u1) (UChar.uint_code u2) v m

let find u m = IMap.find (UChar.uint_code u) m
let remove u m = IMap.remove (UChar.uint_code u) m

let remove_range u1 u2 m =
  IMap.remove_range (UChar.uint_code u1) (UChar.uint_code u2) m

let from u m = IMap.from (UChar.uint_code u) m
let after u m = IMap.after (UChar.uint_code u) m

let until u m = IMap.until (UChar.uint_code u) m
let before u m = IMap.before (UChar.uint_code u) m

let mem u m = IMap.mem (UChar.uint_code u) m

let iter f m =
  let f' n = f (UChar.chr_of_uint n) in
  IMap.iter f' m

let iter_range f m =
  let f' n1 n2 = 
    f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) in
  IMap.iter_range f' m

let fold f m a =
  let f' n v a = f (UChar.chr_of_uint n) v a in
  IMap.fold f' m a

let fold_range f m a =
  let f' n1 n2 v a = 
    f (UChar.chr_of_uint n1) (UChar.chr_of_uint n2) v a in
  IMap.fold_range f' m a

let mapi ?eq f m =
  let f' n v = f (UChar.chr_of_uint n) v in
  IMap.mapi ?eq f' m

let set_to_map s = IMap.set_to_map (USet.iset_of_uset s)

let domain m = USet.uset_of_iset (IMap.domain m)

let map_to_set p m = USet.uset_of_iset (IMap.map_to_set p m)
