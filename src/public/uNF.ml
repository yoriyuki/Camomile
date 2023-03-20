(** Unicode normal form (NFD, NFKD, NFC, NFKC) as described in UTR #15 *)

(* Copyright (C) 2002 Yamagata Yoriyuki. *)

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

module type Type = sig
  type text

  open OOChannel

  class nfd : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel
  class nfc : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel
  class nfkd : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel
  class nfkc : UChar.t #obj_output_channel -> [UChar.t] obj_output_channel

  (** Conversion to NFD, NFKD, NFC, NFKC forms. *)

  val nfd : text -> text
  val nfkd : text -> text
  val nfc : text -> text
  val nfkc : text -> text

  module NFCBuf : sig
    type buf

    val create : int -> buf
    val contents : buf -> text
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> UChar.t -> unit
    val add_string : buf -> text -> unit
    val add_buffer : buf -> buf -> unit
  end

  val nfc_append : text -> text -> text

  (** [put_nfd b t], [put_nfkd b t], [put_nfc b t], [put_nfkc b t]
      clear the contents of [b] and put the NFD, NFKD, NFC, NFKC
      forms of [t] into [b] respectively. *)

  val put_nfd : XString.t -> text -> unit
  val put_nfkd : XString.t -> text -> unit
  val put_nfc : XString.t -> text -> unit
  val put_nfkc : XString.t -> text -> unit

  type index

  val nfd_inc :
    text -> index -> ([ `Inc of UChar.t list * index * 'a lazy_t ] as 'a)

  val canon_compare : text -> text -> int
  val nfd_decompose : UChar.t -> UChar.t list
  val nfkd_decompose : UChar.t -> UChar.t list
end

module Make (Config : Config.Type) (Text : UnicodeString.Type) = struct
  module UInfo = UCharInfo.Make (Config)

  let null = UChar.chr_of_uint 0
  let decomposition_tbl = UInfo.load_decomposition_tbl ()
  let decomposition u = UCharTbl.get decomposition_tbl u
  let composition_exclusion_tbl = UInfo.load_composition_exclusion_tbl ()
  let composition_exclusion u = UCharTbl.Bool.get composition_exclusion_tbl u
  let composition_tbl = UInfo.load_composition_tbl ()
  let composition u = UCharTbl.get composition_tbl u

  let rec add_list x = function
    | [] -> ()
    | u :: r ->
        XString.add_char x u;
        add_list x r

  let shiftright x i j =
    for k = j downto i do
      XString.set x (k + 1) (XString.get x k)
    done

  let rotate x i j =
    let u = XString.get x j in
    shiftright x i (j - 1);
    XString.set x i u

  let blit x i x' i' len =
    for k = 0 to len - 1 do
      XString.set x' (i' + k) (XString.get x (i + k))
    done

  let nfd u =
    match decomposition u with
      | `HangulSyllable -> Hangul.decompose u
      | `Composite (`Canon, d) -> d
      | _ -> [u]

  let nfd_decompose = nfd

  let rec nfkd u =
    match decomposition u with
      | `HangulSyllable -> Hangul.decompose u
      | `Composite (_, d) -> List.fold_right (fun u a -> nfkd u @ a) d []
      | `Canonform -> [u]

  let nfkd_decompose = nfkd

  let canon_decompose_uchar x u =
    match decomposition u with
      | `HangulSyllable -> Hangul.add_decomposition x u
      | `Composite (`Canon, d) -> add_list x d
      | _ -> XString.add_char x u

  class canon_decompose (c_out : UChar.t OOChannel.obj_output_channel) =
    object
      method put u = List.iter c_out#put (nfd u)
      method close_out = c_out#close_out
      method flush () = c_out#flush ()
    end

  let rec kompat_decompose_uchar x u =
    match decomposition u with
      | `Composite (_, d) -> List.iter (kompat_decompose_uchar x) d
      | _ -> Hangul.add_decomposition x u

  class kompat_decompose c_out : [UChar.t] OOChannel.obj_output_channel =
    object
      method put u = List.iter c_out#put (nfkd u)
      method close_out = c_out#close_out
      method flush () = c_out#flush ()
    end

  let canon_reorder x =
    let chead = ref 0 in
    let pos = ref 0 in
    for i = 0 to XString.length x - 1 do
      let u = XString.get x i in
      let c = UInfo.combined_class u in
      if c = 0 then chead := i
      else begin
        pos := i - 1;
        while !pos >= !chead && UInfo.combined_class (XString.get x !pos) > c do
          decr pos
        done;
        rotate x (!pos + 1) i
      end
    done

  class canon_reorder c_out : [UChar.t] OOChannel.obj_output_channel =
    object (self)
      val mutable sq = []

      method private out_buf =
        let sq' = List.stable_sort (fun (c1, _) (c2, _) -> c1 - c2) sq in
        List.iter (fun (_, u) -> c_out#put u) sq';
        sq <- []

      method put u =
        let c = UInfo.combined_class u in
        if c = 0 then (
          if sq <> [] then self#out_buf;
          c_out#put u)
        else sq <- (c, u) :: sq

      method close_out () =
        self#out_buf;
        c_out#close_out ()

      method flush () =
        if sq <> [] then
          failwith "uNF.canon_reorder#flush: Cannot flush the entire buffer";
        c_out#flush ()
    end

  let rec look_composition u1 = function
    | [] -> None
    | (u, u') :: _ when u = u1 ->
        if composition_exclusion u' || UInfo.combined_class u' <> 0 then None
        else Some u'
    | _ :: rest -> look_composition u1 rest

  let rec canon_compose_loop x i j x' k c' =
    if j >= XString.length x then begin
      blit x i x' (k + 1) (XString.length x - i);
      k + max (XString.length x - i) 0
    end
    else (
      let u = XString.get x j in
      let c = UInfo.combined_class u in
      let b =
        if j = i || c' <> c then (
          (*not blocked!*)
          match look_composition u (composition (XString.get x' k)) with
            | None -> true
            | Some u' ->
                XString.set x' k u';
                shiftright x i (j - 1);
                false)
        else true
      in
      if b && c = 0 then begin
        blit x i x' (k + 1) (j - i + 1);
        canon_compose_loop x (j + 1) (j + 1) x' (k + 1 + j - i) 0
      end
      else (
        let i' = if b then i else i + 1 in
        let c' = if b then c else c' in
        canon_compose_loop x i' (j + 1) x' k c'))

  let canon_compose x' x =
    if XString.length x = 0 then ()
    else (
      let pos = ref 0 in
      while
        !pos < XString.length x
        && UInfo.combined_class (XString.get x !pos) <> 0
      do
        incr pos
      done;
      blit x 0 x' 0 !pos;
      if !pos < XString.length x then begin
        XString.set x' !pos (XString.get x !pos);
        pos := canon_compose_loop x (!pos + 1) (!pos + 1) x' !pos 0
      end
      else ();
      XString.shrink x' (!pos + 1))

  class canon_compose c_out : [UChar.t] OOChannel.obj_output_channel =
    object (self)
      val mutable has_strt = false
      val mutable strt = null
      val mutable cmp_cnd = []
      val mutable lst_cc = -1
      val sq = Queue.create ()

      method private set_strt u =
        if has_strt then c_out#put strt;
        Queue.iter c_out#put sq;
        strt <- u;
        has_strt <- true;
        cmp_cnd <- composition u;
        lst_cc <- -1;
        Queue.clear sq

      method private output_buffer () =
        if has_strt then c_out#put strt;
        Queue.iter c_out#put sq

      method put u =
        let c = UInfo.combined_class u in
        if not has_strt then if c = 0 then self#set_strt u else c_out#put u
        else if c = lst_cc then Queue.add u sq
        else (
          match look_composition u cmp_cnd with
            | Some u' -> strt <- u'
            | None ->
                if c = 0 then self#set_strt u
                else begin
                  Queue.add u sq;
                  lst_cc <- c
                end)

      method close_out () =
        self#output_buffer ();
        c_out#close_out ()

      method flush () =
        self#output_buffer ();
        c_out#flush ()
    end

  class nfd c_out =
    let c = new canon_reorder c_out in
    object
      inherit canon_decompose c
    end

  class nfc c_out =
    let c = new canon_compose c_out in
    let c = new canon_reorder c in
    object
      inherit canon_decompose c
      method! flush = c_out#flush
    end

  class nfkd c_out =
    let c = new canon_reorder c_out in
    object
      inherit kompat_decompose c
      method! flush = c_out#flush
    end

  class nfkc c_out =
    let c = new canon_compose c_out in
    let c = new canon_reorder c in
    object
      inherit kompat_decompose c
      method! flush = c_out#flush
    end

  type inc = [ `Inc of UChar.t list * Text.index * 'a lazy_t ] as 'a

  let rec inc_end i = `Inc ([], i, lazy (inc_end i))

  let rec inc_canon_decompose t i : inc =
    if Text.out_of_range t i then inc_end i
    else (
      let i' = Text.next t i in
      `Inc (nfd (Text.look t i), i', lazy (inc_canon_decompose t i')))

  let rec canon_insert_list ((_, c) as x) a =
    match a with
      | [] -> [x]
      | ((_, c') as y) :: rest ->
          if c' <= c then y :: canon_insert_list x rest else x :: a

  let rec split1_list = function
    | [] -> []
    | (x, _) :: rest -> x :: split1_list rest

  let canon_sort_list sq =
    let rec loop a = function
      | [] -> split1_list a
      | (u, 0) :: rest -> split1_list a @ (u :: loop [] rest)
      | ((_, _) as x) :: rest -> loop (canon_insert_list x a) rest
    in
    loop [] sq

  let rec read_combined_class = function
    | [] -> []
    | u :: rest -> (u, UInfo.combined_class u) :: read_combined_class rest

  let inc_reorder f t i =
    let (`Inc (us, i', f)) = f t i in
    let rec loop (f : inc Lazy.t) a i =
      let (`Inc (us, i', f)) = Lazy.force f in
      let a' = read_combined_class us in
      match a' with
        | [] -> `Inc (canon_sort_list a, i, lazy (inc_end i))
        | (_, 0) :: _ -> `Inc (canon_sort_list a, i, lazy (loop f a' i'))
        | _ -> loop f (a @ a') i'
    in
    loop f (read_combined_class us) i'

  let nfd_inc t i = inc_reorder inc_canon_decompose t i

  let canon_compare t1 t2 =
    let (`Inc (us1, _, f1)) = nfd_inc t1 (Text.nth t1 0) in
    let (`Inc (us2, _, f2)) = nfd_inc t2 (Text.nth t2 0) in
    let rec loop us1 us2 f1 f2 =
      match (us1, us2) with
        | [], [] -> 0
        | [], _ -> ~-1
        | _, [] -> 1
        | u1 :: r1, u2 :: r2 ->
            let sgn = UChar.compare u1 u2 in
            if sgn <> 0 then sgn
            else (
              let us1, f1 =
                if r1 = [] then (
                  let (`Inc (us1, _, f1)) = Lazy.force f1 in
                  (us1, f1))
                else (r1, f1)
              in
              let us2, f2 =
                if r2 = [] then (
                  let (`Inc (us2, _, f2)) = Lazy.force f2 in
                  (us2, f2))
                else (r2, f2)
              in
              loop us1 us2 f1 f2)
    in
    loop us1 us2 f1 f2

  type text = Text.t
  type index = Text.index

  let canon_decompose x t = Text.iter (canon_decompose_uchar x) t
  let kompat_decompose x t = Text.iter (kompat_decompose_uchar x) t
  let text_of_xstring x = Text.init (XString.length x) (XString.get x)

  let nfd t =
    let x = XString.make 0 (UChar.chr_of_uint 0) in
    canon_decompose x t;
    canon_reorder x;
    text_of_xstring x

  let nfkd t =
    let x = XString.make 0 (UChar.chr_of_uint 0) in
    kompat_decompose x t;
    canon_reorder x;
    text_of_xstring x

  let nfc t =
    let x = XString.make 0 (UChar.chr_of_uint 0) in
    canon_decompose x t;
    canon_reorder x;
    canon_compose x x;
    Hangul.compose x x;
    text_of_xstring x

  let nfkc t =
    let x = XString.make 0 (UChar.chr_of_uint 0) in
    kompat_decompose x t;
    canon_reorder x;
    canon_compose x x;
    Hangul.compose x x;
    text_of_xstring x

  let put_nfd x t =
    XString.clear x;
    canon_decompose x t;
    canon_reorder x

  let put_nfkd x t =
    XString.clear x;
    kompat_decompose x t;
    canon_reorder x

  let put_nfc x t =
    XString.clear x;
    canon_decompose x t;
    canon_reorder x;
    canon_compose x x;
    Hangul.compose x x

  let put_nfkc x t =
    XString.clear x;
    kompat_decompose x t;
    canon_reorder x;
    canon_compose x x;
    Hangul.compose x x

  module NFCBuf = struct
    type buf = { mutable normalized : bool; mutable buf : XString.t }

    let create bufsize =
      { normalized = true; buf = XString.make ~bufsize 0 null }

    let contents b =
      if not b.normalized then (
        let buf = XString.make 0 null in
        XString.iter (canon_decompose_uchar buf) b.buf;
        canon_reorder buf;
        canon_compose buf buf;
        b.buf <- buf;
        b.normalized <- true);
      text_of_xstring b.buf

    let clear b =
      b.normalized <- true;
      XString.clear b.buf

    let reset b =
      b.normalized <- true;
      XString.reset b.buf

    let add_char b u =
      b.normalized <- false;
      XString.add_char b.buf u

    let add_string b t =
      b.normalized <- false;
      Text.iter (XString.add_char b.buf) t

    let add_buffer b1 b2 =
      b1.normalized <- false;
      XString.add_xstring b1.buf b2.buf
  end

  let nfc_append t1 t2 =
    let b = XString.make 0 null in
    canon_decompose b t1;
    canon_decompose b t2;
    canon_reorder b;
    canon_compose b b;
    text_of_xstring b
end
