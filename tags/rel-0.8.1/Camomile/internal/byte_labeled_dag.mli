(* $Id: byte_labeled_dag.mli,v 1.2 2003/03/24 20:58:05 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

type bytes
val make_bytes : int -> (string * int) list -> bytes
val look_leaf_bytes : bytes -> int -> int
val look_branch_bytes : bytes -> int -> bytes option
