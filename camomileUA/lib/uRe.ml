(** Regular expression engine. *)

(* Copyright (C) 2003 Yamagata Yoriyuki. distributed with LGPL *)

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

type regexp  =
  [ `Alt of regexp * regexp
  | `Seq of regexp * regexp
  | `Rep of regexp
  | `Repn of regexp * int * int option
  | `After of regexp
  | `Before of regexp
  | `Epsilon
  | `Group of regexp
  | `OneChar
  | `String of UChar.t list
  | `Set of USet.t
  | `BoS
  | `EoS ]

type match_semantics = [ `First | `Shortest | `Longest ]

let rec no_group  = function
    `Alt (r1, r2) -> `Alt (no_group r1, no_group r2)
  | `Seq (r1, r2) -> `Alt (no_group r1, no_group r2)
  | `Rep r -> `Rep (no_group r)
  | `Group r -> r
  | r -> r

module type Type = sig
  type text
  type index
  type compiled_regexp

  module SubText : 
    SubText.Type with type ur_text = text and type ur_index = index

  val compile : regexp -> compiled_regexp

  val regexp_match : ?sem:match_semantics ->
    compiled_regexp -> text -> index -> SubText.t option array option
  val string_match : compiled_regexp -> text -> index -> bool
  val search_forward : ?sem:match_semantics ->
    compiled_regexp -> text -> index -> SubText.t option array option
end

module Make (Text : UnicodeString.Type) = struct
  type text = Text.t
  type index = Text.index

  module SubText = SubText.Make (Text)

  type instr  =
      String of UChar.t list
    | OneChar
    | Set of USet.t
    | Par of instr * instr
    | Seq of instr * instr
    | Rep of instr
    | Repn of instr * int * int option
    | Epsilon
    | Group of int * instr
    | BoS
    | EoS
    | Before of instr
    | After of instr
    | Group_end of int * Text.index

  type compiled_regexp = int * instr

  let compile (r : regexp) = 
    let rec loop n = function
        `Alt (r1, r2) ->
        let n, r1 = loop n r1 in
        let n, r2 = loop n r2 in
        n, Par (r1, r2)
      | `Seq (r1, r2) ->
        let n, r1 = loop n r1 in
        let n, r2 = loop n r2 in
        n, Seq (r1, r2)
      | `Rep r ->
        let n, r = loop n r in
        n, Rep r
      | `Repn (r, n, m) ->
        let n, r = loop n r in
        n, Repn (r, n, m)
      | `Group r ->
        let n', r = loop (n + 1) r in
        n', Group (n, r)
      | `Epsilon -> n, Epsilon
      | `String s -> n, String s
      | `OneChar -> n, OneChar
      | `Set s -> n, Set s
      | `BoS -> n, BoS
      | `EoS -> n, EoS
      | `After r ->
        let n, r = loop n r in
        n, After r
      | `Before r ->
        let n, r = loop n r in
        n, Before r in
    loop 1 r

  let rec string_match t i = function
      [] -> i
    | u :: rest ->
      if Text.out_of_range t i then raise Exit else
      if UChar.eq (Text.look t i) u then
        string_match t (Text.next t i) rest
      else
        raise Exit

  let reverse_string_match t i us =
    let us = List.rev us in
    let rec loop i  = function
        [] -> i
      | u :: rest ->
        if Text.out_of_range t i then raise Exit else
        if UChar.eq (Text.look t i) u then
          loop (Text.prev t i) rest
        else
          raise Exit in
    loop i us

  let dec_opt = function
      None -> None
    | Some m -> Some (m - 1)

  let rec exec_first groups t i = function
    | [] -> i, groups
    | Seq (r1, r2) :: rest ->
      exec_first groups t i (r1 :: r2 :: rest)
    | Rep r :: rest as rs -> 
      (try exec_first groups t i rest with Exit -> 
         exec_first groups t i (r :: rs))
    | Repn (r, n, m) :: rest ->
      if n > 0 then
        exec_first groups t i (r :: Repn (r, n-1, dec_opt m) :: rest)
      else (match m with
            None ->
            exec_first groups t i (Rep r :: rest)
          | Some m ->
            if m <= 0 then exec_first groups t i rest else
              let s = Repn (r, 0, Some (m-1)) in
              try exec_first groups t i rest with Exit ->
                exec_first groups t i (r :: s :: rest))
    | Par (r1, r2) :: rest -> 
      (try exec_first groups t i (r1 :: rest) with Exit -> 
         exec_first groups t i (r2 :: rest))
    | Group (n, r) :: rest ->
      exec_first groups t i (r :: Group_end (n, i) :: rest)
    | Group_end (n, i0) :: rest ->
      let s = SubText.refer t i0 i in
      exec_first ((n, s) :: groups) t i rest
    | String t0 :: rest -> 
      exec_first groups t (string_match t i t0) rest
    | OneChar :: rest ->
      if Text.out_of_range t i then raise Exit else
        exec_first groups t (Text.next t i) rest
    | Set s :: rest -> 
      (if Text.out_of_range t i then raise Exit else
         match USet.mem (Text.look t i) s with
           true -> exec_first groups t (Text.next t i) rest
         | false -> raise Exit)
    | Epsilon :: rest -> exec_first groups t i rest
    | BoS :: rest ->
      if Text.compare_index t i (Text.nth t 0) > 0 then raise Exit else
        exec_first groups t i rest
    | EoS :: rest ->
      if Text.compare_index t i (Text.last t) <= 0 then raise Exit else
        exec_first groups t i rest
    | After r :: rest ->
      let _, groups = reverse groups t i [r] in
      exec_first groups t i rest
    | Before r :: rest ->
      let _, groups = exec_first groups t i [r] in
      exec_first groups t i rest
  and reverse g t i = function
    | [] -> i, g
    | Seq (r1, r2) :: rest ->
      reverse g t i (r2 :: r1 :: rest)
    | Rep r :: rest as rs -> 
      (try reverse g t i rest with Exit -> 
         reverse g t i (r :: rs))
    | Repn (r, n, m) :: rest ->
      if n > 0 then
        reverse g t i (r :: Repn (r, n-1, dec_opt m) :: rest)
      else (match m with
            None ->
            reverse g t i (Rep r :: rest)
          | Some m ->
            if m <= 0 then reverse g t i rest else
              let s = Repn (r, 0, Some (m-1)) in
              try reverse g t i rest with Exit ->
                reverse g t i (r :: s :: rest))
    | Par (r1, r2) :: rest -> 
      (try reverse g t i (r1 :: rest) with Exit -> 
         reverse g t i (r2 :: rest))
    | Group (n, r) :: rest ->
      reverse g t i (r :: Group_end (n, i) :: rest)
    | Group_end (n, j) :: rest ->
      let s = SubText.refer t i j in
      reverse ((n, s) :: g) t i rest
    | String t0 :: rest -> 
      reverse g t (reverse_string_match t i t0) rest
    | OneChar :: rest ->
      if Text.out_of_range t i then raise Exit else
        reverse g t (Text.prev t i) rest
    | Set s :: rest -> 
      (if Text.out_of_range t i then raise Exit else
         match USet.mem (Text.look t i) s with
           true -> reverse g t (Text.prev t i) rest
         | false -> raise Exit)
    | Epsilon :: rest -> reverse g t i rest
    | BoS :: rest ->
      if Text.compare_index t i (Text.nth t 0) > 0 then raise Exit else
        reverse g t i rest
    | EoS :: rest ->
      if Text.compare_index t i (Text.last t) <= 0 then raise Exit else
        reverse g t i rest
    | After r :: rest ->
      let _, g = reverse g t i [r] in
      reverse g t i rest
    | Before r :: rest ->
      let _, g = exec_first g t i [r] in
      reverse g t i rest

  let rec exec_longest groups t i = function
    | [] -> i, groups
    | Seq (r1, r2) :: rest ->
      exec_longest groups t i (r1 :: r2 :: rest)
    | Rep r :: rest as rs -> 
      (try
         let i1, g1 = exec_longest groups t i rest in try
           let i2, g2 = exec_longest groups t i (r :: rs) in
           if Text.compare_index t i1 i2 >= 0 then i1, g1 else i2, g2
         with Exit -> 
           i1, g1
       with Exit -> 
         exec_longest groups t i (r :: rs))
    | Repn (r, n, m) :: rest ->
      if n > 0 then
        exec_longest groups t i (r :: Repn (r, n-1, dec_opt m) :: rest)
      else (match m with
            None ->
            exec_longest groups t i (Rep r :: rest)
          | Some m ->
            if m <= 0 then exec_longest groups t i rest else
              let s = Repn (r, 0, Some (m-1)) in 
              try 
                let i1, g1 = exec_longest groups t i rest in 
                try
                  let i2, g2 = exec_longest groups t i (r :: s :: rest) in 
                  if Text.compare_index t i1 i2 >= 0 then i1, g1 else i2, g2
                with Exit ->
                  i1, g1
              with Exit ->
                exec_longest groups t i (r :: s :: rest))
    | Par (r1, r2) :: rest -> 
      (try exec_longest groups t i (r1 :: rest) with Exit -> 
         exec_longest groups t i (r2 :: rest))
    | Group (n, r) :: rest ->
      exec_longest groups t i (r :: Group_end (n, i) :: rest)
    | Group_end (n, i0) :: rest ->
      let s = SubText.refer t i0 i in
      exec_longest ((n, s) :: groups) t i rest
    | String t0 :: rest -> 
      exec_longest groups t (string_match t i t0) rest
    | OneChar :: rest ->
      if Text.out_of_range t i then raise Exit else
        exec_longest groups t (Text.next t i) rest
    | Set s :: rest -> 
      (if Text.out_of_range t i then raise Exit else
         match USet.mem (Text.look t i) s with
           true -> exec_longest groups t (Text.next t i) rest
         | false -> raise Exit)
    | Epsilon :: rest -> exec_longest groups t i rest
    | BoS :: rest ->
      if Text.compare_index t i (Text.nth t 0) > 0 then raise Exit else
        exec_longest groups t i rest
    | EoS :: rest ->
      if Text.compare_index t i (Text.last t) <= 0 then raise Exit else
        exec_longest groups t i rest
    | After r :: rest ->
      let _, g = reverse groups t i [r] in
      exec_longest g t i rest
    | Before r :: rest ->
      let _, g = exec_first groups t i [r] in
      exec_longest g t i rest

  let rec exec_shortest groups t i = function
    | [] -> i, groups
    | Seq (r1, r2) :: rest ->
      exec_shortest groups t i (r1 :: r2 :: rest)
    | Rep r :: rest as rs -> 
      (try
         let i1, g1 = exec_shortest groups t i rest in try
           let i2, g2 = exec_shortest groups t i (r :: rs) in
           if Text.compare_index t i1 i2 <= 0 then i1, g1 else i2, g2
         with Exit -> 
           i1, g1
       with Exit -> 
         exec_shortest groups t i (r :: rs))
    | Repn (r, n, m) :: rest ->
      if n > 0 then
        exec_shortest groups t i (r :: Repn (r, n-1, dec_opt m) :: rest)
      else (match m with
            None ->
            exec_shortest groups t i (Rep r :: rest)
          | Some m ->
            if m <= 0 then exec_shortest groups t i rest else
              let s = Repn (r, 0, Some (m-1)) in 
              try 
                let i1, g1 = exec_shortest groups t i rest in 
                try
                  let i2, g2 = exec_shortest groups t i (r :: s :: rest) in 
                  if Text.compare_index t i1 i2 <= 0 then i1, g1 else i2, g2
                with Exit ->
                  i1, g1
              with Exit ->
                exec_shortest groups t i (r :: s :: rest))
    | Par (r1, r2) :: rest -> 
      (try exec_shortest groups t i (r1 :: rest) with Exit -> 
         exec_shortest groups t i (r2 :: rest))
    | Group (n, r) :: rest ->
      exec_shortest groups t i (r :: Group_end (n, i) :: rest)
    | Group_end (n, i0) :: rest ->
      let s = SubText.refer t i0 i in
      exec_shortest ((n, s) :: groups) t i rest
    | String t0 :: rest -> 
      exec_shortest groups t (string_match t i t0) rest
    | OneChar :: rest ->
      if Text.out_of_range t i then raise Exit else
        exec_shortest groups t (Text.next t i) rest
    | Set s :: rest -> 
      (if Text.out_of_range t i then raise Exit else
         match USet.mem (Text.look t i) s with
           true -> exec_shortest groups t (Text.next t i) rest
         | false -> raise Exit)
    | Epsilon :: rest -> exec_shortest groups t i rest
    | BoS :: rest ->
      if Text.compare_index t i (Text.nth t 0) > 0 then raise Exit else
        exec_shortest groups t i rest
    | EoS :: rest ->
      if Text.compare_index t i (Text.last t) <= 0 then raise Exit else
        exec_shortest groups t i rest
    | After r :: rest ->
      let _, g = reverse groups t i [r] in
      exec_shortest g t i rest
    | Before r :: rest ->
      let _, g = exec_first groups t i [r] in
      exec_shortest g t i rest

  let set_groups groups g =
    let rec loop = function
        [] -> ()
      | (n, s) :: rest -> 
        groups.(n) <- Some s;
        loop rest in
    loop (List.rev g)

  let regexp_match ?(sem=`Longest) (n, r) t i =
    let groups = Array.make n None in
    try match sem with
        `First -> 
        let j, g = exec_first [] t i [r] in
        set_groups groups g;
        groups.(0) <- Some (SubText.refer t i j);
        Some groups
      | `Shortest -> 
        let j, g = exec_shortest [] t i [r] in
        set_groups groups g;
        groups.(0) <- Some (SubText.refer t i j);
        Some groups
      | `Longest ->
        let j, g = exec_longest [] t i [r] in
        set_groups groups g;
        groups.(0) <- Some (SubText.refer t i j);
        Some groups
    with Exit -> None

  let string_match (_, r) t i =
    try ignore (exec_first [] t i [r]); true with Exit -> false

  let search_forward ?(sem=`Longest) ((n, r) as c) t i =
    let groups = Array.make n None in
    let rec scan i =
      if Text.out_of_range t i then None else
        try let j, g = exec_first [] t i [r] in Some (i, j, g) with Exit -> 
          scan (Text.next t i) in
    match scan i with
      Some (i, j, g) ->
      (match sem with
         `First ->
         set_groups groups g;
         groups.(0) <- Some (SubText.refer t i j);
         Some groups
       | _ ->
         regexp_match ~sem c t i)
    | None -> None

end
