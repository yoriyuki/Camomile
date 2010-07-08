(* $Id: byte_labeled_dag.ml,v 1.7 2003/04/24 03:46:27 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

type bytes_node = 
    {bytes_leaf_offset : int;
     bytes_leaf : Bytesvect.t;
     bytes_default : int;
     bytes_branch_offset : int;
     bytes_branch : (bytes_node option) array}

type bytes = bytes_node

let rec search_array x i a =
  if Array.length a <= i || a.(i) <> x then i else
  search_array x (i + 1) a

let rec search_backward_array x i a =
  if i < 0 || a.(i) <> x then i else
  search_backward_array x (i - 1) a

let make_bytes def vs =
  let make_node leaf branch =
    let leaf_offset = search_array def 0 leaf in
    let branch_offset = search_array None 0 branch in
    let leaf_last = search_backward_array def 255 leaf in
    let branch_last = search_backward_array None 255 branch in
    let leaf_len = max 0 (leaf_last - leaf_offset + 1) in
    let branch_len = max 0 (branch_last - branch_offset + 1) in
    let a = Bytesvect.make leaf_len def in
    let b = Array.sub branch branch_offset branch_len in
    for i = 0 to Bytesvect.length a - 1 do
      Bytesvect.set a i leaf.(leaf_offset + i)
    done;
    {bytes_leaf_offset = leaf_offset;
     bytes_leaf = a;
     bytes_default = def;
     bytes_branch_offset = branch_offset;
     bytes_branch = b}
  in
  let rec scan d c leaf branch = function
      [] -> (make_node leaf branch, [])
    | (s, n) :: rest as vs ->
	if String.length s <= d ||  d >= 0 && s.[d] <> c then 
	  (make_node leaf branch, vs) 
	else
	  let c' = s.[d + 1] in
	  if String.length s = d + 2 then begin
	    leaf.(Char.code c') <- n;
	    scan d c leaf branch rest;
	  end else
	    let leaf' = Array.make 256 def in
	    let branch' = Array.make 256 None in
	    let node, rest = scan (d + 1) c' leaf' branch' vs in
	    branch.(Char.code c') <- Some node;
	    scan d c leaf branch rest
  in
  let comp (s1, _) (s2, _) = Pervasives.compare s1 s2 in
  let vs = List.sort comp vs in
  match vs with
    (s, n) :: _ -> 
      let leaf = Array.make 256 def in
      let branch = Array.make 256 None in
      let tbl, _ = scan ~-1 '\000' leaf branch vs in
      tbl
  | _ -> invalid_arg "Broken table"


let look_leaf_bytes tbl b = 
  let i = b - tbl.bytes_leaf_offset in
  if i < 0 || i >= Bytesvect.length tbl.bytes_leaf then 
    tbl.bytes_default
  else
    Bytesvect.get tbl.bytes_leaf i

let look_branch_bytes tbl b = 
  let i = b - tbl.bytes_branch_offset in
  if i < 0 || i >= Array.length tbl.bytes_branch then None else
  tbl.bytes_branch.(i)
