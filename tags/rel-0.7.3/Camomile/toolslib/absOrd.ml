(* $Id: absOrd.ml,v 1.6 2006/08/13 17:20:16 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

type point = int

module Int = struct type t = int let compare = (-) end

module Set = ISet
module Map = Map.Make (Int)
module IntSet = Set
module IntMap = Map

type node = 
    Empty | Leaf of point
  | Node of Set.t * node * node * int

module Node =
struct
let height = function
    Empty -> 0
  | Leaf _  -> 1
  | Node (_, _, _, h) -> h

let elts = function
    Empty -> Set.empty
  | Leaf p -> Set.add p Set.empty
  | Node (s, _, _, _) -> s

let create l r =
  match l, r with
    Empty, _ -> r
  | _, Empty -> l
  | _ ->
      let hl = height l in
      let hr = height r in
      let h = 1 + max hl hr in
      let s = Set.union (elts l) (elts r) in
      Node (s, l, r, h)
	
let rec bal = function
    Empty -> Empty
  | Leaf _ as s -> s
  | Node (s, l, r, _) as node ->
  let hl = height l in
  let hr = height r in
  if hl - hr > 2 then
    match l with
      Node (ls, ll, lr, _) ->
	let hll = height ll in
	let hlr = height lr in
	if hll >= hlr then
	  Node (s, ll, concat lr r, 1 + hll)
	else
	  (match lr with
	    Node (_, lrl, lrr, _) ->
	      let l' = concat ll lrl in
	      let r' = concat lrr r in
	      let h = 1 + max (height l') (height r') in
	      Node (s, l', r', h)
	  | _ -> assert false)
    |	_ -> assert false
  else if hr - hl > 2 then
    match r with
      Node (rs, rl, rr, _) ->
	let hrl = height rl in
	let hrr = height rr in
	if hrl <= hrr then
	  Node (s, concat l rl, rr, 1 + hrr)
	else
	  (match rl with
	    Node (_, rll, rlr, _) ->
	      let l' = concat l rll in
	      let r' = concat rlr rr in
	      let h = 1 + max (height l') (height r') in
	      Node (s, l', r', h)
	  | _ -> assert false)
    |	_ -> assert false
  else node

and concat l r = bal (create l r)
 
let mem p  = function
    Empty -> false
  | Leaf p' -> (p = p')
  | Node (s, _, _, _) -> Set.mem p s

let rec compare p1 p2 = function
    Empty -> raise Not_found
  | Leaf p ->
      if p1 = p && p2 = p then 0 else 
      raise Not_found
  | Node (s, s1, s2, _) ->
      if mem p1 s1 then
	if mem p2 s1 then compare p1 p2 s1 else
	if mem p2 s2 then -1 else
	raise Not_found
      else if mem p1 s2 then
	if mem p2 s1 then 1 else
	if mem p2 s2 then compare p1 p2 s2 else
	raise Not_found
      else
	raise Not_found

let rec top = function
    Empty -> raise Not_found
  |	Leaf p -> p
  |	Node (_, s1, s2, _) -> 
      try top s2 with Not_found -> top s1

let rec top_in m = function
    Empty -> raise Not_found
  | Leaf p ->
      if Map.mem p m then p else raise Not_found
  | Node (_, s1, s2, _) ->
      try top_in m s2 with Not_found -> top_in m s1

let rec bottom = function
    Empty -> raise Not_found
  |	Leaf p -> p
  |	Node (_, s1, s2, _) -> 
      try bottom s1 with Not_found -> bottom s2

let rec bottom_in m = function
    Empty -> raise Not_found
  | Leaf p ->
      if Map.mem p m then p else raise Not_found
  | Node (_, s1, s2, _) ->
      try bottom_in m s1 with Not_found -> bottom_in m s1

let rec next p = function
    Empty -> raise Not_found
  |	Leaf _ -> raise Not_found
  |	Node (_, s1, s2, _) ->
      if mem p s1 then try next p s1 with Not_found -> bottom s2 else
      next p s2

let rec prev p = function
    Empty -> raise Not_found
  |	Leaf _ -> raise Not_found
  |	Node (_, s1, s2, _) ->
      if mem p s2 then try prev p s2 with Not_found -> top s1 else
      prev p s1

let rec before p = function
    Empty -> Empty
  | Leaf p' ->
      if p = p' then Empty else raise Not_found
  | Node (_, s1, s2, h) ->
      if mem p s1 then
	before p s1
      else
	concat s1 (before p s2)

let rec upto p = function
    Empty -> Empty
  | Leaf p' as s -> 
      if p = p' then s else raise Not_found
  | Node (_, s1, s2, h) ->
      if mem p s1 then
	upto p s1
      else
	concat s1 (upto p s2)

let rec after p = function
    Empty -> Empty
  | Leaf p' ->
      if p = p' then Empty else raise Not_found
  | Node (_, s1, s2, h) ->
      if mem p s1 then
	concat (after p s1) s2
      else
	after p s2

let rec from p = function
    Empty -> Empty
  | Leaf p' as s ->
      if p = p' then s else raise Not_found
  | Node (_, s1, s2, h) ->
      if mem p s1 then
	concat (from p s1) s2
      else
	from p s2

let rec iter proc = function
    Empty -> ()
  | Leaf p -> proc p
  | Node (_, s1, s2, _) -> 
      iter proc s1; 
      iter proc s2

let rec fold f s init =
  match s with
    Empty -> init
  | Leaf p -> f p init
  | Node (_, s1, s2, _) ->
      fold f s2 (fold f s1 init)

let rec put_to_top p = function
    Empty -> Leaf p
  | Leaf _ as s1 -> create s1 (Leaf p)
  | Node (s, s1, s2, h) ->
      let s' = Set.add p s in
      let s2' = put_to_top p s2 in
      let h = 1 + max (height s1) (height s2') in
      bal (Node (s', s1, s2', h))

let rec put_to_bottom p = function
    Empty -> Leaf p
  | Leaf _ as s2 -> create (Leaf p) s2
  | Node (s, s1, s2, h) ->
      let s' = Set.add p s in
      let s1' = put_to_bottom p s1 in
      let h = 1 + max (height s1') (height s2) in
      bal (Node (s', s1', s2, h))

let rec put_before p0 p = function
    Empty -> raise Not_found
  | Leaf p1 as s ->
      if p1 = p0 then create (Leaf p) s else raise Not_found
  | Node (s, s1, s2, h) ->
      let s' = Set.add p s in
      let s1', s2' =
	if mem p0 s1 then (put_before p0 p s1), s2 else
	s1, (put_before p0 p s2)
      in
      let h = 1 + max (height s1') (height s2') in
      bal (Node (s', s1', s2', h))

let rec put_after p0 p = function
    Empty -> raise Not_found
  | Leaf p1 as s ->
      if p1 = p0 then create s (Leaf p) else raise Not_found
  | Node (s, s1, s2, h) ->
      let s' = Set.add p s in
      let s1', s2' =
	if mem p0 s1 then (put_after p0 p s1), s2 else
	s1, (put_after p0 p s2)
      in
      let h = 1 + max (height s1') (height s2') in
      bal (Node (s', s1', s2', h))

end

type t = node * int

let compare p1 p2 (node, _) = Node.compare p1 p2 node
let top (node, _) = Node.top node
let bottom (node, _) = Node.bottom node
let next p (node, _) = Node.next p node
let prev p (node, _) = Node.prev p node

let add_top (node, id) = (id, (Node.put_to_top id node, id + 1))

let add_bottom (node, id) = (id, (Node.put_to_bottom id node, id + 1))

let add_before p (node, id) =
  (id, (Node.put_before p id node, id + 1))

let add_after p (node, id) =
  (id, (Node.put_after p id node, id + 1))

let iter proc (node, _) = Node.iter proc node
let fold f (node, _) init = Node.fold f node init

let rec import_aux a i j w2p p2w id =
  if i = j then
    (Leaf id, IntMap.add a.(i) id w2p, Map.add id a.(i) p2w, id + 1)
  else
    let i' = i + (j - i) / 2 in
    let ord1, w2p, p2w, id = import_aux a i i' w2p p2w id in
    let ord2, w2p, p2w, id = import_aux a (i' + 1) j w2p p2w id in
    (Node.concat ord1 ord2, w2p, p2w, id)

let import weights =
  let set = List.fold_left (fun set w ->
    IntSet.add w set) 
      IntSet.empty 
      weights 
  in
  let weights = IntSet.fold (fun w ws -> w :: ws) set [] in
  let a = Array.of_list weights in
  Array.sort (-) a;
  let node, w2p, p2w, id = 
    import_aux a 0 (Array.length a - 1) IntMap.empty Map.empty 0
  in
  ((node, id), w2p, p2w)
