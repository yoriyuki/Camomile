(** Collaiton Element, abstracted *)

(* Copyright (C) 2003 Yamagata Yoriyuki *)

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
(* yoriyuki.y@gmail.com *)

open CamomileLibrary
open CamomileLibrary.Private
module Info = UCharInfo.Make (Camomileconfig)

type elt =
  [ `Seq of UChar.t list
  | `ImplicitWeight of int list
  | `CompleteIgnorable
  | `UCA_Weight of int
  | `LastVariable
  | `HiraganaQ
  | `FirstImplicit
  | `FirstTrailing ]

module Elt = struct
  type t = elt

  let compare = Stdlib.compare
end

module EltMap = Map.Make (Elt)

type ce = AbsOrd.point * AbsOrd.point * AbsOrd.point

module UCharMap = UMap

type ceset = {
  l1 : AbsOrd.t;
  l2 : AbsOrd.t;
  l3 : AbsOrd.t;
  implicits : int list AbsOrd.Map.t;
  contractions : UChar.t list list UCharMap.t;
  tbl1 : AbsOrd.point list EltMap.t;
  tbl2 : AbsOrd.point list EltMap.t;
  tbl3 : AbsOrd.point list EltMap.t;
}

let bottom' l =
  let p = AbsOrd.bottom l in
  AbsOrd.next p l

let rec tripling l1 l2 l3 =
  match (l1, l2, l3) with
    | x1 :: r1, x2 :: r2, x3 :: r3 -> (x1, x2, x3) :: tripling r1 r2 r3
    | [], [], [] -> []
    | _ -> assert false

let rec de_tripling = function
  | (x1, x2, x3) :: rest ->
      let l1, l2, l3 = de_tripling rest in
      (x1 :: l1, x2 :: l2, x3 :: l3)
  | [] -> ([], [], [])

let ce_of ceset us =
  let e = `Seq us in
  tripling (EltMap.find e ceset.tbl1) (EltMap.find e ceset.tbl2)
    (EltMap.find e ceset.tbl3)

let complete_ignorable ceset =
  let ces =
    tripling
      (EltMap.find `CompleteIgnorable ceset.tbl1)
      (EltMap.find `CompleteIgnorable ceset.tbl2)
      (EltMap.find `CompleteIgnorable ceset.tbl3)
  in
  match ces with [ce] -> ce | _ -> assert false

let last_variable ceset =
  match EltMap.find `LastVariable ceset.tbl1 with
    | [p] -> (p, AbsOrd.top ceset.l2, AbsOrd.top ceset.l3)
    | _ -> assert false

let hiraganaQ ceset =
  match EltMap.find `HiraganaQ ceset.tbl1 with
    | [p] -> (p, AbsOrd.top ceset.l2, AbsOrd.top ceset.l3)
    | _ -> assert false

let first_implicit ceset =
  match EltMap.find `FirstImplicit ceset.tbl1 with
    | [p] -> (p, bottom' ceset.l2, bottom' ceset.l3)
    | _ -> assert false

let first_trailing ceset =
  match EltMap.find `FirstTrailing ceset.tbl1 with
    | [p] -> (p, bottom' ceset.l2, bottom' ceset.l3)
    | _ -> assert false

let top ceset = (AbsOrd.top ceset.l1, AbsOrd.top ceset.l2, AbsOrd.top ceset.l3)

let rec next prec ((p1, p2, p3) as ce) ceset =
  match prec with
    | `Primary -> (AbsOrd.next p1 ceset.l1, bottom' ceset.l2, bottom' ceset.l3)
    | `Secondary -> (
        try (p1, AbsOrd.next p2 ceset.l2, bottom' ceset.l3)
        with Not_found -> next `Primary ce ceset)
    | `Tertiary -> (
        try (p1, p2, AbsOrd.next p3 ceset.l3)
        with Not_found -> next `Secondary ce ceset)
    | `Quaternary -> assert false

let rec prev prec ((p1, p2, p3) as ce) ceset =
  match prec with
    | `Primary ->
        (AbsOrd.prev p1 ceset.l1, AbsOrd.top ceset.l2, AbsOrd.top ceset.l3)
    | `Secondary -> (
        try (p1, AbsOrd.prev p2 ceset.l2, AbsOrd.top ceset.l3)
        with Not_found -> prev `Primary ce ceset)
    | `Tertiary -> (
        try (p1, p2, AbsOrd.prev p3 ceset.l3)
        with Not_found -> prev `Secondary ce ceset)
    | `Quaternary -> assert false

let add_after prec (p1, p2, p3) ceset =
  match prec with
    | `Primary ->
        let p1', l1' = AbsOrd.add_after p1 ceset.l1 in
        ( (p1', p2, p3),
          {
            l1 = l1';
            l2 = ceset.l2;
            l3 = ceset.l3;
            implicits = ceset.implicits;
            contractions = ceset.contractions;
            tbl1 = ceset.tbl1;
            tbl2 = ceset.tbl2;
            tbl3 = ceset.tbl3;
          } )
    | `Secondary ->
        let p2', l2' = AbsOrd.add_after p2 ceset.l2 in
        ( (p1, p2', p3),
          {
            l1 = ceset.l1;
            l2 = l2';
            l3 = ceset.l3;
            implicits = ceset.implicits;
            contractions = ceset.contractions;
            tbl1 = ceset.tbl1;
            tbl2 = ceset.tbl2;
            tbl3 = ceset.tbl3;
          } )
    | `Tertiary ->
        let p3', l3' = AbsOrd.add_after p3 ceset.l3 in
        ( (p1, p2, p3'),
          {
            l1 = ceset.l1;
            l2 = ceset.l2;
            l3 = l3';
            implicits = ceset.implicits;
            contractions = ceset.contractions;
            tbl1 = ceset.tbl1;
            tbl2 = ceset.tbl2;
            tbl3 = ceset.tbl3;
          } )
    | `Quaternary -> assert false

let add_before prec (p1, p2, p3) ceset =
  match prec with
    | `Primary ->
        let p1', l1' = AbsOrd.add_before p1 ceset.l1 in
        ( (p1', p2, p3),
          {
            l1 = l1';
            l2 = ceset.l2;
            l3 = ceset.l3;
            implicits = ceset.implicits;
            contractions = ceset.contractions;
            tbl1 = ceset.tbl1;
            tbl2 = ceset.tbl2;
            tbl3 = ceset.tbl3;
          } )
    | `Secondary ->
        let p2', l2' = AbsOrd.add_before p2 ceset.l2 in
        ( (p1, p2', p3),
          {
            l1 = ceset.l1;
            l2 = l2';
            l3 = ceset.l3;
            implicits = ceset.implicits;
            contractions = ceset.contractions;
            tbl1 = ceset.tbl1;
            tbl2 = ceset.tbl2;
            tbl3 = ceset.tbl3;
          } )
    | `Tertiary ->
        let p3', l3' = AbsOrd.add_before p3 ceset.l3 in
        ( (p1, p2, p3'),
          {
            l1 = ceset.l1;
            l2 = ceset.l2;
            l3 = l3';
            implicits = ceset.implicits;
            contractions = ceset.contractions;
            tbl1 = ceset.tbl1;
            tbl2 = ceset.tbl2;
            tbl3 = ceset.tbl3;
          } )
    | `Quaternary -> assert false

let put e ces ceset =
  let ps1, ps2, ps3 = de_tripling ces in
  let tbl1 = EltMap.add e ps1 ceset.tbl1 in
  let tbl2 = EltMap.add e ps2 ceset.tbl2 in
  let tbl3 = EltMap.add e ps3 ceset.tbl3 in
  let contractions =
    match e with
      | `Seq (u :: us) ->
          let entry =
            try UCharMap.find u ceset.contractions with Not_found -> []
          in
          UCharMap.add u (us :: entry) ceset.contractions
      | _ -> ceset.contractions
  in
  {
    l1 = ceset.l1;
    l2 = ceset.l2;
    l3 = ceset.l3;
    implicits = ceset.implicits;
    contractions;
    tbl1;
    tbl2;
    tbl3;
  }

(* the following potion of code comes from uCol.ml *)
let rec add_list x = function
  | [] -> ()
  | e :: rest ->
      XArray.add_element x e;
      add_list x rest

let rec burst_aux x i a =
  if i < 0 then a else burst_aux x (i - 1) (XArray.get x i :: a)

let burst x = burst_aux x (XArray.length x - 1) []

let noncharacter_code_point_tbl =
  Info.load_property_tbl `Noncharacter_Code_Point

let is_noncharacter_code_point u =
  UCharTbl.Bool.get noncharacter_code_point_tbl u

let assign_implicit ceset a b =
  let rec search pos =
    let next = try Some (AbsOrd.next pos ceset.l1) with Not_found -> None in
    match next with
      | None -> pos
      | Some next -> (
          let v =
            try Some (AbsOrd.Map.find next ceset.implicits)
            with Not_found -> None
          in
          match v with
            | Some [a'; b'] ->
                if a' > a || (a' = a && b' > b) then pos else search next
            | _ -> search next)
  in
  let pos = search (List.hd (EltMap.find `FirstImplicit ceset.tbl1)) in
  let p, l1 = AbsOrd.add_after pos ceset.l1 in
  let implicits = AbsOrd.Map.add p [a; b] ceset.implicits in
  let tbl1 = EltMap.add (`ImplicitWeight [a; b]) [p] ceset.tbl1 in
  ( p,
    {
      l1;
      l2 = ceset.l2;
      l3 = ceset.l3;
      implicits;
      contractions = ceset.contractions;
      tbl1;
      tbl2 = ceset.tbl2;
      tbl3 = ceset.tbl3;
    } )

let implicit_ce ceset cebuf u =
  let n = UChar.uint_code u in
  if
    n < 0 || n > 0x10ffff
    || Info.general_category u = `Cs
    || is_noncharacter_code_point u
  then begin
    XArray.add_element cebuf (complete_ignorable ceset);
    ceset
  end
  else (
    let base =
      if n >= 0x4e00 && n <= 0x9fff then 0xfb40
      else if n >= 0x3400 && n <= 0x4dbf then 0xfb80
      else if n >= 0x20000 && n <= 0x2a6df then 0xfb80
      else 0xfbc0
    in
    let a = base + (n lsr 15) in
    let b = n land 0x7fff lor 0x8000 in
    let p1, ceset =
      try
        let p1 = List.hd (EltMap.find (`ImplicitWeight [a; b]) ceset.tbl1) in
        (p1, ceset)
      with Not_found -> assign_implicit ceset a b
    in
    let p2 = List.hd (EltMap.find (`UCA_Weight 0x0020) ceset.tbl2) in
    let p3 = List.hd (EltMap.find (`UCA_Weight 0x0002) ceset.tbl3) in
    XArray.add_element cebuf (p1, p2, p3);
    ceset)

let shiftright x i j =
  for k = j downto i do
    XString.set x (k + 1) (XString.get x k)
  done

let rec remove_chars x i = function
  | [] -> i
  | j :: rest ->
      shiftright x i (j - 1);
      remove_chars x (i + 1) rest

let rec match_us2 x i c' = function
  | [] -> []
  | u :: rest as us ->
      if i >= XString.length x then raise Exit
      else (
        let u' = XString.get x i in
        let c = Info.combined_class u' in
        if c' = 0 || c = 0 || c' = c then raise Exit
        else if u = u' then i :: match_us2 x (i + 1) c' rest
        else match_us2 x (i + 1) c us)

let rec match_us1 x i = function
  | [] -> i
  | u :: rest as us ->
      if i >= XString.length x then raise Exit
      else (
        let u' = XString.get x i in
        if u = u' then match_us1 x (i + 1) rest
        else (
          let ps = match_us2 x (i + 1) (Info.combined_class u') us in
          remove_chars x i ps))

let rec longest_match ce_buf x i = function
  | [] -> assert false
  | (us, ces) :: rest -> (
      try
        let j = match_us1 x i us in
        add_list ce_buf ces;
        j
      with Exit -> longest_match ce_buf x i rest)

let rec getce ceset ce_buf x i =
  if i >= XString.length x then ceset
  else (
    let u = XString.get x i in
    let contractions =
      try UCharMap.find u ceset.contractions with Not_found -> []
    in
    let ces = List.map (fun us -> (us, ce_of ceset (u :: us))) contractions in
    let ces =
      List.sort (fun (us1, _) (us2, _) -> List.length us2 - List.length us1) ces
    in
    let ceset, i' =
      match ces with
        | [] ->
            let ceset = implicit_ce ceset ce_buf u in
            (ceset, i + 1)
        | [([], [ce])] ->
            XArray.add_element ce_buf ce;
            (ceset, i + 1)
        | info -> (ceset, longest_match ce_buf x (i + 1) info)
    in
    getce ceset ce_buf x i')

let ces_of ceset us =
  let x = XString.make 0 (UChar.chr_of_uint 0) in
  List.iter (XString.add_char x) us;
  let ce_buf = XArray.make 0 (complete_ignorable ceset) in
  let ceset = getce ceset ce_buf x 0 in
  (ceset, burst ce_buf)

let map_triple f (x1, x2, x3) = (f x1, f x2, f x3)
let map2_triple f (x1, x2, x3) (y1, y2, y3) = (f x1 y1, f x2 y2, f x3 y3)

let import weight_tbls =
  let burst weight_tbl = EltMap.fold (fun _ w ws -> w @ ws) weight_tbl [] in
  let weights1, weights2, weights3 = map_triple burst weight_tbls in
  let l1, w1_to_p1, _p1_to_w1 = AbsOrd.import weights1 in
  let l2, w2_to_p2, _p2_to_w2 = AbsOrd.import weights2 in
  let l3, w3_to_p3, _p3_to_w3 = AbsOrd.import weights3 in
  let w_to_ps = (w1_to_p1, w2_to_p2, w3_to_p3) in
  let tbls =
    map2_triple
      (fun weight_tbl w_to_p ->
        EltMap.map
          (fun ws -> List.map (fun w -> AbsOrd.IntMap.find w w_to_p) ws)
          weight_tbl)
      weight_tbls w_to_ps
  in
  let tbls =
    map2_triple
      (fun tbl w_to_p ->
        EltMap.add `CompleteIgnorable [AbsOrd.IntMap.find 0 w_to_p] tbl)
      tbls w_to_ps
  in
  let tbl1, tbl2, tbl3 =
    map2_triple
      (fun tbl w_to_p ->
        AbsOrd.IntMap.fold
          (fun w p tbl -> EltMap.add (`UCA_Weight w) [p] tbl)
          w_to_p tbl)
      tbls w_to_ps
  in
  let implicits =
    EltMap.fold
      (fun e ps implicits ->
        match (e, ps) with
          | `ImplicitWeight ws, [p] -> AbsOrd.Map.add p ws implicits
          | _ -> implicits)
      tbl1 AbsOrd.Map.empty
  in
  let contractions =
    EltMap.fold
      (fun e _ m ->
        match e with
          | `Seq (u :: us) ->
              let entry = try UCharMap.find u m with Not_found -> [] in
              UCharMap.add u (us :: entry) m
          | _ -> m)
      tbl1 UCharMap.empty
  in
  { l1; l2; l3; implicits; contractions; tbl1; tbl2; tbl3 }

let rec split_last = function
  | [x] -> ([], x)
  | x :: rest ->
      let xs, last = split_last rest in
      (x :: xs, last)
  | [] -> raise Not_found

type ace_info = {
  ceset : ceset;
  variable_option : UCol.variable_option;
  french : bool;
  hiraganaQ : bool;
}

[@@@ocaml.warning "-27"]

let create_ace_info ?(variable_option = `Shifted) ?(french = false)
    ?(hiraganaQ = false) ceset =
  { ceset; variable_option; french = false; hiraganaQ = false }

[@@@ocaml.warning "+27"]

let incr_ws min max ws =
  let prefix, w = split_last ws in
  if w + 1 >= max then (
    match min with
      | None -> failwith "Minimum weight is not given."
      | Some min -> prefix @ [max; min])
  else prefix @ [w + 1]

let weights_of ace_info =
  let first_regular =
    let p, _, _ =
      if ace_info.hiraganaQ then hiraganaQ ace_info.ceset
      else last_variable ace_info.ceset
    in
    AbsOrd.next p ace_info.ceset.l1
  in
  let p1_to_w1, _, _ =
    AbsOrd.fold
      (fun p (pw, ws, rw) ->
        let ws, next =
          try
            let ws = AbsOrd.Map.find p ace_info.ceset.implicits in
            match rw with
              | None -> failwith "Variables have too high weight."
              | Some regular ->
                  let next = ws @ [0xffff; regular] in
                  (ws, next)
          with Not_found -> (ws, incr_ws rw 0xfb3f ws)
        in
        let rw =
          match rw with
            | Some _ -> rw
            | None ->
                if AbsOrd.compare p first_regular ace_info.ceset.l1 >= 0 then (
                  match ws with
                    | [w] -> Some w
                    | _ -> failwith "Variables have too high weight.")
                else None
        in
        (AbsOrd.Map.add p ws pw, next, rw))
      ace_info.ceset.l1
      (AbsOrd.Map.empty, [0], None)
  in
  let p2_to_w2, _ =
    AbsOrd.fold
      (fun p (pw, ws) -> (AbsOrd.Map.add p ws pw, incr_ws (Some 1) 0xff ws))
      ace_info.ceset.l2 (AbsOrd.Map.empty, [0])
  in
  let p3_to_w3, _ =
    AbsOrd.fold
      (fun p (pw, ws) -> (AbsOrd.Map.add p ws pw, incr_ws (Some 1) 0x7f ws))
      ace_info.ceset.l3 (AbsOrd.Map.empty, [0])
  in
  (p1_to_w1, p2_to_w2, p3_to_w3)

let rec seq0 n =
  assert (n >= 0);
  if n = 0 then [] else 0 :: seq0 (n - 1)

let rec pad0 n = function x :: rest -> x :: pad0 (n - 1) rest | [] -> seq0 n

let rec list_map3 f l1 l2 l3 =
  match (l1, l2, l3) with
    | [], [], [] -> []
    | x1 :: r1, x2 :: r2, x3 :: r3 -> f x1 x2 x3 :: list_map3 f r1 r2 r3
    | _ -> assert false

let compose_ces french (ws1, ws2, ws3) =
  let len = max (List.length ws1) (max (List.length ws2) (List.length ws3)) in
  let ws1 = pad0 len ws1 in
  let ws2 =
    let x = pad0 len ws2 in
    if french then List.rev x else x
  in
  let ws3 = pad0 len ws3 in
  list_map3 Toolslib.Unidata.compose_ce ws1 ws2 ws3

module Tbl = UCharTbl.Make (struct
  type t = (UChar.t list * Toolslib.Unidata.ce_type list) list

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let cetbl_of ace_info =
  let ((p1_to_w1, _, _) as p_to_w) = weights_of ace_info in
  let variable_top =
    match EltMap.find `LastVariable ace_info.ceset.tbl1 with
      | [p] -> (
          match AbsOrd.Map.find p p1_to_w1 with
            | [w] -> w
            | _ -> failwith "Too high weight for variable top")
      | _ -> assert false
  in
  let hiraganaQ_weight =
    try
      match EltMap.find `HiraganaQ ace_info.ceset.tbl1 with
        | [p] -> (
            match AbsOrd.Map.find p p1_to_w1 with
              | [w] -> w
              | _ -> failwith "Too high weight for the HiraganaQ weight")
        | _ -> assert false
    with Not_found ->
      if ace_info.hiraganaQ then failwith "no HiraganaQ weight" else 0
  in
  let weight_map =
    EltMap.fold
      (fun e ps1 weight_map ->
        match e with
          | `Seq (u :: rest) ->
              let ps2 = EltMap.find e ace_info.ceset.tbl2 in
              let ps3 = EltMap.find e ace_info.ceset.tbl3 in
              let ps = tripling ps1 ps2 ps3 in
              let ws =
                List.map (fun p -> map2_triple AbsOrd.Map.find p p_to_w) ps
              in
              let ces = List.map (compose_ces ace_info.french) ws in
              let ce = List.concat ces in
              let entry =
                try UCharMap.find u weight_map with Not_found -> []
              in
              UCharMap.add u ((rest, ce) :: entry) weight_map
          | _ -> weight_map)
      ace_info.ceset.tbl1 UCharMap.empty
  in
  let comp (u1, _) (u2, _) = List.length u2 - List.length u1 in
  let weight_map = UCharMap.map (List.sort comp) weight_map in
  {
    Toolslib.Unidata.variable_top;
    variable_option = ace_info.variable_option;
    french_accent = ace_info.french;
    hiraganaQ = ace_info.hiraganaQ;
    hiraganaQ_weight;
    tbl = Tbl.of_map [] weight_map;
  }

type aceset_info = { lowercase_first_tbl : ceset; uppercase_first_tbl : ceset }
