(** Unicode collation algorithm *)

(* Copyright (C) 2002, 2003 Yamagata Yoriyuki *)

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
(* yori@users.sourceforge.net *)

type variable_option =
  [ `Blanked
  | `Non_ignorable
  | `Shifted
  | `Shift_Trimmed ]

type precision = [ `Primary | `Secondary | `Tertiary | `Quaternary ]

module type Type =
sig
  type text
  type index

  (** For locale, see {!Locale}.
      If [locale] is omitted, the standard UCA order is used.
      If [prec] is omitted, the maximum possible strength is used.
      If [variable] is omitted, the default of the locale
      (usually [`Shifted]) is used.
      The meaning of the returned value is similar to Pervasives.compare *)
  val compare :
    ?locale:string -> ?prec:precision -> ?variable:variable_option ->
    text -> text -> int

  (** Binary comparison of sort_key gives the same result as [compare].
      i.e.
      [compare t1 t2 = Pervasives.compare (sort_key t1) (sort_key t2)]
      If the same texts are repeatedly compared,
      pre-computation of sort_key gives better performance. *)
  val sort_key :
    ?locale:string -> ?prec:precision -> ?variable:variable_option ->
    text -> string

  (** Comparison with the sort key. *)
  val compare_with_key :
    ?locale: string -> ?prec:precision -> ?variable:variable_option ->
    string -> text -> int

  val search_with_key :
    ?locale: string -> ?prec:precision -> ?variable:variable_option ->
    string -> text -> index -> (index * index)

  val search :
    ?locale: string -> ?prec:precision -> ?variable:variable_option ->
    text -> text -> index -> (index * index)

end

module Make (Config : ConfigInt.Type) (Text : UnicodeString.Type) = struct

  module Unidata = Unidata.Make(Config)
  module UCharInfo = UCharInfo.Make(Config)

  let logical_order_exception_tbl =
    UCharInfo.load_property_tbl `Logical_Order_Exception

  let is_logical_order_exception u =
    UCharTbl.Bool.get logical_order_exception_tbl u

  let rec rearrange_aux x pos =
    if pos > XString.length x - 2 then () else
      let u = XString.get x pos in
      if is_logical_order_exception u then begin
        XString.set x pos (XString.get x (pos + 1));
        XString.set x (pos + 1) u;
        rearrange_aux x (pos + 2)
      end else
        rearrange_aux x (pos + 1)

  let rearrange x = rearrange_aux x 0

  let remove_ignorable ce_tbl x =
    let rec loop0 i =
      if XString.length x <= i then () else
        let u = XString.get x i in
        match Unidata.ce ce_tbl u with
          [([], [ce])] when ce = Unidata.complete_ignorable ->
          loop1 (i + 1) i
        | _ ->
          loop0 (i + 1)
    and loop1 i k =			(*k < i *)
      if XString.length x <= i then begin
        XString.shrink x k;
      end else
        let u = XString.get x i in
        match Unidata.ce ce_tbl u with
          [([], [ce])] when ce = Unidata.complete_ignorable ->
          loop1 (i + 1) k
        | _ ->
          XString.set x k u; loop1 (i + 1) (k + 1) in
    loop0 0

  let noncharacter_code_point_tbl =
    UCharInfo.load_property_tbl `Noncharacter_Code_Point

  let is_noncharacter_code_point u =
    UCharTbl.Bool.get noncharacter_code_point_tbl u

  let reverse s =
    if Bytes.length s = 0 then () else
      let last = Bytes.length s - 1 in
      for i = 0 to last / 2 do
        let c = Bytes.get s i in
        Bytes.set s i (Bytes.get s (last - i));
        Bytes.set s (last - i) c
      done

  let shiftright x i j =
    for k = j downto i do XString.set x (k + 1) (XString.get x k) done

  let rec remove_chars x i = function
      [] -> i
    | j :: rest ->
      shiftright x i (j - 1);
      remove_chars x (i + 1) rest

  let trim start_regular key =
    let rec loop i =
      if i > 0 &&
         (Char.code key.[i - 1]) lsl 8
         lor (Char.code key.[i]) > start_regular
      then loop (i - 2)
      else String.sub key 0 (i + 1) in
    loop (String.length key - 1)

  let is_variable variable_top ce =
    Unidata.primary ce <> 0 && Unidata.primary ce <= variable_top

  let is_ignorable ce = Unidata.primary ce = 0

  let add_i16 buf n =
    Buffer.add_char buf (Char.unsafe_chr (n lsr 8));
    Buffer.add_char buf (Char.unsafe_chr (n land 255))

  let add_byte buf n =
    Buffer.add_char buf (Char.unsafe_chr n)

  type non_ignorable_keybuf =
    {non_ignorable_col_info : Unidata.col_info;
     non_ignorable_prec : precision;
     non_ignorable_primary : Buffer.t;
     non_ignorable_secondary : Buffer.t;
     non_ignorable_tertiary : Buffer.t;
     non_ignorable_quaternary : Buffer.t;
     mutable non_ignorable_count : int}

  let addce_non_ignorable keybuf ce =
    let w1 = Unidata.primary ce in
    if w1 <> 0 && w1 <> keybuf.non_ignorable_col_info.hiraganaQ_weight then
      add_i16 keybuf.non_ignorable_primary w1;
    match keybuf.non_ignorable_prec with `Primary -> () | _ ->
      let w2 = Unidata.secondary ce in
      if w2 <> 0 then add_byte keybuf.non_ignorable_secondary w2;
      match keybuf.non_ignorable_prec with `Secondary -> () | _ ->
        let w3 = Unidata.tertiary ce in
        if w3 <> 0 then add_byte keybuf.non_ignorable_tertiary w3;
        match keybuf.non_ignorable_prec with `Tertiary -> () | _ ->
          if not keybuf.non_ignorable_col_info.hiraganaQ then () else
          if w1 = keybuf.non_ignorable_col_info.hiraganaQ_weight then begin
            if keybuf.non_ignorable_count > 0 then begin
              add_i16
                keybuf.non_ignorable_quaternary
                (1 + keybuf.non_ignorable_count);
              keybuf.non_ignorable_count <- 0;
            end;
            add_i16 keybuf.non_ignorable_quaternary 1;
          end else begin
            keybuf.non_ignorable_count <- keybuf.non_ignorable_count + 1;
            if keybuf.non_ignorable_count = 0xffff - 1 then begin
              add_i16 keybuf.non_ignorable_quaternary 0xffff;
              keybuf.non_ignorable_count <- 0;
            end
          end

  let terminate_non_ignorable keybuf =
    let c = keybuf.non_ignorable_count in
    if c > 0 then add_i16 keybuf.non_ignorable_quaternary (1 + c)

  type blanked_keybuf =
    {blanked_col_info : Unidata.col_info;
     blanked_prec : precision;
     blanked_primary : Buffer.t;
     blanked_secondary : Buffer.t;
     blanked_tertiary : Buffer.t;
     blanked_quaternary : Buffer.t;
     mutable blanked_after_variable : bool;
     mutable blanked_count : int}

  let addce_blanked keybuf ce =
    if is_ignorable ce && keybuf.blanked_after_variable then () else
    if is_variable keybuf.blanked_col_info.variable_top ce then
      keybuf.blanked_after_variable <- true
    else begin
      keybuf.blanked_after_variable <- false;
      let w1 = Unidata.primary ce in
      if w1 <> 0 && w1 <> keybuf.blanked_col_info.hiraganaQ_weight then
        add_i16 keybuf.blanked_primary w1;
      match keybuf.blanked_prec with `Primary -> () | _ ->
        let w2 = Unidata.secondary ce in
        if w2 <> 0 then add_byte keybuf.blanked_secondary w2;
        match keybuf.blanked_prec with `Secondary -> () | _ ->
          let w3 = Unidata.tertiary ce in
          if w3 <> 0 then add_byte keybuf.blanked_tertiary w3;
          match keybuf.blanked_prec with `Tertiary -> () | _ ->
            if not keybuf.blanked_col_info.hiraganaQ then () else
            if w1 = keybuf.blanked_col_info.hiraganaQ_weight then begin
              if keybuf.blanked_count > 0 then begin
                add_i16 keybuf.blanked_quaternary (1 + keybuf.blanked_count);
                keybuf.blanked_count <- 0
              end;
              add_i16 keybuf.blanked_quaternary 1;
            end else begin
              keybuf.blanked_count <- keybuf.blanked_count + 1;
              if keybuf.blanked_count = 0xffff - 1 then begin
                add_i16 keybuf.blanked_quaternary 0xffff;
                keybuf.blanked_count <- 0
              end
            end
    end

  let terminate_blanked keybuf =
    let c = keybuf.blanked_count in
    if c > 0 then add_i16 keybuf.blanked_quaternary (1 + c)

  type shifted_keybuf =
    {shifted_col_info : Unidata.col_info;
     shifted_prec : precision;
     shifted_primary : Buffer.t;
     shifted_secondary : Buffer.t;
     shifted_tertiary : Buffer.t;
     shifted_quaternary : Buffer.t;
     mutable shifted_after_variable : bool;
     mutable shifted_count : int}

  let start_regular keybuf =
    if keybuf.shifted_col_info.hiraganaQ then
      keybuf.shifted_col_info.hiraganaQ_weight
    else
      keybuf.shifted_col_info.variable_top

  let addce_shifted keybuf ce =
    let start_regular = start_regular keybuf in
    if is_ignorable ce && keybuf.shifted_after_variable then () else
    if is_variable keybuf.shifted_col_info.variable_top ce then begin
      keybuf.shifted_after_variable <- true;
      match keybuf.shifted_prec with `Quaternary ->
        if keybuf.shifted_count > 0 then begin
          add_i16
            keybuf.shifted_quaternary
            (start_regular + keybuf.shifted_count);
          keybuf.shifted_count <- 0
        end;
        add_i16 keybuf.shifted_quaternary (Unidata.primary ce);
                                   | _ -> ()
    end else begin
      keybuf.shifted_after_variable <- false;
      let w1 = Unidata.primary ce in
      if w1 <> 0 && w1 <> keybuf.shifted_col_info.hiraganaQ_weight then
        add_i16 keybuf.shifted_primary w1;
      match keybuf.shifted_prec with `Primary -> () | _ ->
        let w2 = Unidata.secondary ce in
        if w2 <> 0 then add_byte keybuf.shifted_secondary w2;
        match keybuf.shifted_prec with `Secondary -> () | _ ->
          let w3 = Unidata.tertiary ce in
          if w3 <> 0 then add_byte keybuf.shifted_tertiary w3;
          match keybuf.shifted_prec with `Tertiary -> () | _ ->
            if is_ignorable ce then () else
            if w1 = keybuf.shifted_col_info.hiraganaQ_weight &&
               keybuf.shifted_col_info.hiraganaQ then begin
              if keybuf.shifted_count > 0 then begin
                add_i16
                  keybuf.shifted_quaternary
                  (start_regular + keybuf.shifted_count);
                keybuf.shifted_count <- 0
              end;
              add_i16 keybuf.shifted_quaternary w1
            end else begin
              keybuf.shifted_count <- keybuf.shifted_count + 1;
              if keybuf.shifted_count = 0xffff - start_regular then begin
                add_i16 keybuf.shifted_quaternary 0xffff;
                keybuf.shifted_count <- 0
              end
            end
    end

  let terminate_shifted keybuf =
    let c = keybuf.shifted_count in
    if c > 0 then
      add_i16
        keybuf.shifted_quaternary
        ((start_regular keybuf) + c)

  let terminate_shift_trimmed keybuf =
    let k4 = Buffer.contents keybuf.shifted_quaternary in
    let k4 = trim (start_regular keybuf) k4 in
    Buffer.clear keybuf.shifted_quaternary;
    Buffer.add_string keybuf.shifted_quaternary k4

  type keybuf =
      Non_ignorable of non_ignorable_keybuf
    | Blanked of blanked_keybuf
    | Shifted of shifted_keybuf
    | Shift_Trimmed of shifted_keybuf

  let create_keybuf prec col_info =
    match col_info.Unidata.variable_option with
      `Non_ignorable ->
      Non_ignorable
        {non_ignorable_col_info = col_info;
         non_ignorable_prec = prec;
         non_ignorable_primary = Buffer.create 0;
         non_ignorable_secondary = Buffer.create 0;
         non_ignorable_tertiary = Buffer.create 0;
         non_ignorable_quaternary = Buffer.create 0;
         non_ignorable_count = 0}
    | `Blanked ->
      Blanked
        {blanked_col_info = col_info;
         blanked_prec = prec;
         blanked_primary = Buffer.create 0;
         blanked_secondary = Buffer.create 0;
         blanked_tertiary = Buffer.create 0;
         blanked_quaternary = Buffer.create 0;
         blanked_after_variable = false;
         blanked_count = 0}
    | `Shifted ->
      Shifted
        {shifted_col_info = col_info;
         shifted_prec = prec;
         shifted_primary = Buffer.create 0;
         shifted_secondary = Buffer.create 0;
         shifted_tertiary = Buffer.create 0;
         shifted_quaternary = Buffer.create 0;
         shifted_after_variable = false;
         shifted_count = 0}
    | `Shift_Trimmed ->
      Shift_Trimmed
        {shifted_col_info = col_info;
         shifted_prec = prec;
         shifted_primary = Buffer.create 0;
         shifted_secondary = Buffer.create 0;
         shifted_tertiary = Buffer.create 0;
         shifted_quaternary = Buffer.create 0;
         shifted_after_variable = false;
         shifted_count = 0}

  let col_info_of_keybuf = function
      Non_ignorable b -> b.non_ignorable_col_info
    | Blanked b -> b.blanked_col_info
    | Shifted b | Shift_Trimmed b -> b.shifted_col_info

  let precision_of_keybuf = function
      Non_ignorable b -> b.non_ignorable_prec
    | Blanked b -> b.blanked_prec
    | Shifted b | Shift_Trimmed b -> b.shifted_prec

  let primary_of_keybuf = function
      Non_ignorable b -> b.non_ignorable_primary
    | Blanked b -> b.blanked_primary
    | Shifted b | Shift_Trimmed b -> b.shifted_primary

  let secondary_of_keybuf = function
      Non_ignorable b -> b.non_ignorable_secondary
    | Blanked b -> b.blanked_secondary
    | Shifted b | Shift_Trimmed b -> b.shifted_secondary

  let tertiary_of_keybuf = function
      Non_ignorable b -> b.non_ignorable_tertiary
    | Blanked b -> b.blanked_tertiary
    | Shifted b | Shift_Trimmed b -> b.shifted_tertiary

  let quaternary_of_keybuf = function
      Non_ignorable b -> b.non_ignorable_quaternary
    | Blanked b -> b.blanked_quaternary
    | Shifted b | Shift_Trimmed b -> b.shifted_quaternary

  let addce keybuf ce =
    (*  Printf.printf "addce ce: %x " ce; *)
    match keybuf with
      Non_ignorable keybuf -> addce_non_ignorable keybuf ce
    | Blanked keybuf -> addce_blanked keybuf ce
    | Shifted keybuf | Shift_Trimmed keybuf ->
      addce_shifted keybuf ce

  let terminate = function
      Non_ignorable keybuf -> terminate_non_ignorable keybuf
    | Blanked keybuf -> terminate_blanked keybuf
    | Shifted keybuf  ->
      terminate_shifted keybuf
    | Shift_Trimmed keybuf ->
      terminate_shift_trimmed keybuf

  let rec add_list keybuf = function
      [] -> ()
    | e :: rest -> addce keybuf e; add_list keybuf rest

  let implicit_ce cebuf u =
    let n = UChar.uint_code u in
    if
      n < 0 || n > 0x10ffff ||
      (match UCharInfo.general_category u with `Cs -> true | _ -> false) ||
      is_noncharacter_code_point u
    then
      addce cebuf Unidata.complete_ignorable		(*illegal code point*)
    else
      let base =
        if n >= 0x4e00 && n <= 0x9fff then 0xfb40 else
        if n >= 0x3400 && n <= 0x4dbf then 0xfb80 else
        if n >= 0x20000 && n <= 0x2a6df then 0xfb80 else
          0xfbc0
      in
      let a = base + n lsr 15 in
      let b = (n land 0x7fff) lor 0x8000 in
      addce cebuf (Unidata.compose_ce a 1 1);
      addce cebuf (Unidata.compose_ce b 0 0)

  let rec match_us2 x i c' = function
      [] -> []
    | (u :: rest) as us ->
      if i >= XString.length x then raise Exit else
        let u' = XString.get x i in
        let c = UCharInfo.combined_class u' in
        if c'= 0 || c = 0 || c' = c then raise Exit else
        if UChar.eq u u' then i :: (match_us2 x (i + 1) c' rest) else
          match_us2 x (i + 1) c us

  let rec match_us1 x i = function
      [] -> i
    | (u :: rest) as us ->
      if i >= XString.length x then raise Exit else
        let u' = XString.get x i in
        if UChar.eq u u' then match_us1 x (i + 1) rest else
          let ps = match_us2 x (i + 1) (UCharInfo.combined_class u') us in
          remove_chars x i ps

  let rec longest_match ce_buf x i = function
      [] -> assert false
    | (us, ces) :: rest ->
      try
        let j = match_us1 x i us in
        add_list ce_buf ces;
        j
      with Exit -> longest_match ce_buf x i rest

  let getce keybuf x i =
    let col_info = col_info_of_keybuf keybuf in
    let hiraganaQ_mark = Unidata.compose_ce col_info.hiraganaQ_weight 0 0 in
    let rec loop i =
      if i >= XString.length x then () else
        let u = XString.get x i in
        (match UCharInfo.script u with `Hiragana when col_info.hiraganaQ ->
           addce keybuf hiraganaQ_mark | _ -> ());
        let i' = match Unidata.ce col_info.tbl u with
            [] -> implicit_ce keybuf u; i + 1
          | [([], [ce])] ->
            addce keybuf ce; i + 1
          | info -> longest_match keybuf x (i + 1) info in
        loop i' in
    loop i

  let getkey keybuf =
    let col_info = col_info_of_keybuf keybuf in
    let prec = precision_of_keybuf keybuf in
    terminate keybuf;
    let buf1 = primary_of_keybuf keybuf in
    (match prec with `Primary -> () | _ ->
        add_i16 buf1 0;
        let buf2 = secondary_of_keybuf keybuf in
        if col_info.french_accent then
          let key2 = Buffer.to_bytes buf2 in
          reverse key2;
          Buffer.add_bytes buf1 key2
        else
          Buffer.add_buffer buf1 buf2;
        match prec with `Secondary -> () | _ ->
          add_i16 buf1 0;
          Buffer.add_buffer buf1 (tertiary_of_keybuf keybuf);
          match prec with `Tertiary -> () | _ ->
            add_i16 buf1 0;
            Buffer.add_buffer buf1 (quaternary_of_keybuf keybuf));
    Buffer.contents buf1

  type text = Text.t
  type index = Text.index

  module NF = UNF.Make(Config)(Text)

  let sort_key_aux col_info prec t =
    let x = XString.make 0 (UChar.chr_of_uint 0) in
    NF.put_nfd x t;
    rearrange x;
    remove_ignorable col_info.Unidata.tbl x;
    let cebuf = create_keybuf prec col_info in
    getce cebuf x 0;
    getkey cebuf

  let sort_key ?locale ?prec ?variable text =
    let col_info =
      let default = Unidata.get_col_info ?locale () in
      match variable with
        None -> default
      | Some v -> {default with variable_option = v} in
    let prec = match prec with
        None -> (match col_info.variable_option with
            `Shifted | `Shift_Trimmed -> `Quaternary
          | _ -> `Tertiary)
      | Some prec -> prec
    in
    sort_key_aux col_info prec text

  (* Incremental sorting and search *)
  let rec primaries_of_ces col_info = function
      [] -> []
    | ce :: rest ->
      (*	  Printf.printf "ce: %x " ce; *)
      let w =
        let w = Unidata.primary ce in
        if w = col_info.Unidata.hiraganaQ_weight then 0 else
          match col_info.variable_option with
            `Non_ignorable -> w
          | _ ->
            if is_variable col_info.variable_top ce then 0 else
              w
      in
      if w = 0 then primaries_of_ces col_info rest else
        w :: primaries_of_ces col_info rest

  let rec inc_end i =
    `Inc ([], i, lazy (inc_end i))

  let inc_prim col_info (`Inc(ces, i, f)) =
    let rec loop i f ws =
      let `Inc (ces, i', f) = Lazy.force f in
      if ces = [] then `Inc (ws, i, lazy (inc_end i)) else
        match primaries_of_ces col_info ces with
          [] -> loop i' f ws
        | ws' ->
          `Inc (ws, i, lazy (loop i' f ws')) in
    loop i f (primaries_of_ces col_info ces)

  let implicit_ce_list u =
    let n = UChar.uint_code u in
    if
      n < 0 || n > 0x10ffff ||
      match UCharInfo.general_category u with `Cs -> true | _ -> false ||
                                                                 is_noncharacter_code_point u
    then
      [Unidata.complete_ignorable]		(*illegal code point*)
    else
      let base =
        if n >= 0x4e00 && n <= 0x9fff then 0xfb40 else
        if n >= 0x3400 && n <= 0x4dbf then 0xfb80 else
        if n >= 0x20000 && n <= 0x2a6df then 0xfb80 else
          0xfbc0
      in
      let a = base + n lsr 15 in
      let b = (n land 0x7fff) lor 0x8000 in
      [Unidata.compose_ce a 1 1; Unidata.compose_ce b 0 0]

  let rec inc_match_us2 i f us0 us1 c' = function
      [] -> `Match (us0 @ us1, i, f)
    | (u :: rest) as us ->
      match us1 with
        [] ->
        let `Inc (us1, i, f) = Lazy.force f in
        if us1 = [] then `Not_Match else
          inc_match_us2 i f us0 us1 c' us
      | u' :: r' ->
        let c = UCharInfo.combined_class u' in
        if c'= 0 || c = 0 || c' = c then `Not_Match else
        if UChar.eq u u' then
          inc_match_us2 i f us0 r' c' rest
        else
          inc_match_us2 i f (us0 @ [u']) r' c us

  let rec inc_match_us1 i f us1 = function
      [] -> `Match (us1, i, f)
    | (u :: rest) as us ->
      match us1 with
        [] ->
        let `Inc (us1, i, f) = Lazy.force f in
        if us1 = [] then `Not_Match else
          inc_match_us1 i f us1 us
      | u' :: r' ->
        if UChar.eq u u' then
          inc_match_us1 i f r' rest
        else
          inc_match_us2 i f [u'] r' (UCharInfo.combined_class u') us

  let rec inc_longest_match us i f = function
      [] -> `Not_Match
    | (us1, ces) :: rest ->
      match inc_match_us1 i f us us1 with
        `Match (us, i, f) -> `Match (ces, us, i, f)
      | `Not_Match ->
        inc_longest_match us i f rest

  let get_next_ce col_info i f u us =
    match Unidata.ce col_info.Unidata.tbl u with
      [] -> (implicit_ce_list u, us, i, f)
    | [([], ces)] -> (ces, us, i, f)
    | info ->
      match inc_longest_match us i f info with
        `Not_Match -> (implicit_ce_list u, us, i, f)
      | `Match (ces, us, i, f) -> (ces, us, i, f)

  let get_ces col_info f t i =
    let hiraganaQ_mark = Unidata.compose_ce col_info.Unidata.hiraganaQ_weight 0 0 in
    let rec loop i f a = function
        [] ->
        (match Lazy.force f with
           `Inc ([], i, _) -> `Inc (a, i, lazy (inc_end i))
         | `Inc (us, i', f) ->
           match a with
             [] -> loop i' f a us
           | _ -> `Inc (a, i, lazy (loop i' f [] us)))
      | u :: us ->
        let a =
          match UCharInfo.script u with
            `Hiragana when col_info.hiraganaQ ->
            a @ [hiraganaQ_mark]
          | _ -> a in
        let ces, us, i, f = get_next_ce col_info i f u us in
        loop i f (a @ ces) us in
    let `Inc (us, i, f) = f t i in
    loop i f [] us

  let inc_prep col_info f t i =
    let rec loop i f prev a = function
        [] ->
        (match a, prev with
           [], _ | _, [_] ->
           (match Lazy.force f with
              `Inc ([], i, _) -> `Inc (a @ prev, i, lazy (inc_end i))
            | `Inc (us, i, f) -> loop i f prev a us)
         | _ -> `Inc (a, i, lazy (loop i f [] [] [])))
      | u :: rest ->
        (*	    Printf.printf "prep uchar %x " (UChar.code u); *)
        match Unidata.ce col_info.Unidata.tbl u with
          [([], [ce])] when ce = Unidata.complete_ignorable ->
          (*		Printf.printf "discarded "; *)
          loop i f prev a rest
        | _ ->
          match prev with
            [] ->
            if is_logical_order_exception u then
              loop i f [u] a rest
            else
              loop i f [] (a @ [u]) rest
          | [u0] ->
            loop i f [] (a @ [u; u0]) rest
          | _ -> assert false in
    let `Inc (us, i, f) = f t i in
    loop i f [] [] us

  let inc_ce col_info t i =
    get_ces col_info (inc_prep col_info NF.nfd_inc) t i

  let key_of_inc prec col_info x =
    let keybuf = create_keybuf prec col_info in
    let rec loop (`Inc(ces, _, f)) =
      add_list keybuf ces;
      match ces with
        [] -> ()
      | _ -> loop (Lazy.force f) in
    loop x;
    getkey keybuf

  let null_weight f =
    match Lazy.force f with `Inc ([], _, _) -> true | _ -> false

  let inc_compare prec col_info t1 t2 =
    let rec loop f1 f2 ws1 ws2 =
      match ws1, ws2 with
        w1 :: rest1, w2 :: rest2 ->
        let sgn = w1 - w2 in
        if sgn = 0 then loop f1 f2 rest1 rest2 else sgn
      | [], ws2 ->
        let `Inc (ws1, _, f1) = Lazy.force f1 in
        if ws1 = [] then
          if ws2 = [] && null_weight f2 then 0 else ~-1
        else
          loop f1 f2 ws1 ws2
      | ws1, [] ->
        let `Inc (ws2, _, f2) = Lazy.force f2 in
        if ws2 = [] then 1 else
          loop f1 f2 ws1 ws2 in
    let x1 = inc_ce col_info t1 (Text.nth t1 0) in
    let x2 = inc_ce col_info t2 (Text.nth t1 0) in
    let `Inc (ws1, _, g1) = inc_prim col_info x1 in
    let `Inc (ws2, _, g2) = inc_prim col_info x2 in
    let sgn = loop g1 g2 ws1 ws2 in
    if sgn <> 0 then sgn else
      match prec with
        `Primary -> 0
      | _ ->
        let key1 = key_of_inc prec col_info x1 in
        let key2 = key_of_inc prec col_info x2 in
        Pervasives.compare key1 key2

  let compare ?locale ?prec ?variable t1 t2 =
    let col_info =
      let default = Unidata.get_col_info ?locale () in
      match variable with
        None -> default
      | Some v ->
        {default with variable_option = v} in
    let prec = match prec with
        None -> (match col_info.variable_option with
            `Shifted | `Shift_Trimmed -> `Quaternary
          | _ -> `Tertiary)
      | Some prec -> prec in
    inc_compare prec col_info t1 t2

  let get_weight k i =
    (Char.code k.[i]) lsl 8 lor (Char.code k.[i + 1])

  let rec primary_length k i =
    if String.length k <= i || get_weight k i = 0 then i else
      primary_length k (i + 2)

  let inc_compare_key prec col_info k t =
    let k_len = primary_length k 0 in
    let rec loop f ws i =
      match ws with
        w :: rest ->
        (*	    Printf.printf "prim %x " w;  *)
        if k_len <= i then ~-1 else
          let w' = get_weight k i in
          let sgn = w' - w in
          if sgn = 0 then loop f rest (i + 2) else sgn
      | [] ->
        let `Inc (ws, _, f) = Lazy.force f in
        if ws = [] then
          if k_len = i then 0 else
          if k_len > i then 1 else
            assert false
        else
          loop f ws i in
    let x = inc_ce col_info t (Text.nth t 0) in
    let `Inc (ws, _, g) = inc_prim col_info x in
    let sgn = loop g ws 0 in
    (*      print_newline ();*)
    if sgn <> 0 then sgn else
      match prec with
        `Primary -> 0
      | _ ->
        let key = key_of_inc prec col_info x in
        (*      Printf.printf "key_of_inc %s\n" (String.escaped key);*)
        Pervasives.compare k key

  let compare_with_key ?locale ?prec ?variable k t =
    let col_info =
      let default = Unidata.get_col_info ?locale () in
      match variable with
        None -> default
      | Some v -> {default with variable_option = v} in
    let prec = match prec with
        None -> (match col_info.variable_option with
            `Shifted | `Shift_Trimmed -> `Quaternary
          | _ -> `Tertiary)
      | Some prec -> prec
    in
    inc_compare_key prec col_info k t

  let search_common col_info prec k t loc =
    let k_len = primary_length k 0 in
    let rec null i f =
      let `Inc (ces, j, f) = Lazy.force f in
      if primaries_of_ces col_info ces = [] then null j f else i in
    let rec test_match i f j = function
        w :: rest ->
        (*	    Printf.printf "prim %x : loc %d " w j; *)
        if k_len <= j then raise Exit else
          let w' = get_weight k j in
          if w' = w then test_match i f (j + 2) rest else raise Exit
      | [] ->
        if k_len = j then (i, null i f) else
          let `Inc (ces, i, f) = Lazy.force f in
          if ces = [] then raise Exit else
            test_match i f j (primaries_of_ces col_info ces) in
    let keys loc f i j ces =
      let keybuf = create_keybuf prec col_info in
      add_list keybuf ces;
      let rec loop f ks =
        let `Inc (ces, loc, f) = Lazy.force f in
        if Text.compare_index t loc j > 0 || ces = [] then ks else begin
          add_list keybuf ces;
          if Text.compare_index t loc i >= 0 then
            loop f ((getkey keybuf, i) :: ks)
          else
            loop f ks
        end in
      if Text.compare_index t loc i >= 0 then
        loop f [(getkey keybuf, i)]
      else
        loop f [] in
    let rec scan loc f =
      let `Inc (ces, i, f) = Lazy.force f in
      if ces = [] then raise Not_found else
        try
          let (i, j) = test_match i f 0 (primaries_of_ces col_info ces) in
          match prec with
            `Primary -> (loc, j)
          | _ ->
            let ks = keys loc f i j ces in
            (*	      Printf.printf "%s %s " (String.escaped k) (String.escaped k'); *)
            try (loc, List.assoc k ks) with Not_found -> raise Exit
        with Exit ->
          scan i f in
    scan loc (lazy (inc_ce col_info t loc))

  let search_with_key ?locale ?prec ?variable k t loc =
    let col_info =
      let default = Unidata.get_col_info ?locale () in
      match variable with
        None -> default
      | Some v -> {default with variable_option = v} in
    let prec = match prec with
        None -> (match col_info.variable_option with
            `Shifted | `Shift_Trimmed -> `Quaternary
          | _ -> `Tertiary)
      | Some prec -> prec in
    search_common col_info prec k t loc

  let search ?locale ?prec ?variable t0 t loc =
    let k = sort_key ?locale ?prec ?variable t0 in
    search_with_key ?locale ?prec ?variable k t loc
end
