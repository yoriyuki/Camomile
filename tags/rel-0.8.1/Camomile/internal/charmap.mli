(* $Id: charmap.mli,v 1.7 2006/08/13 17:09:17 yori Exp $ *)
(* Copyright 2002 Yamagata Yoriyuki *)

module type Interface = sig
type enc_to_ucs

val make_enc_to_ucs : int -> (string * int) list -> enc_to_ucs
val no_char_of : enc_to_ucs -> int

type probe_state
val start_probe : enc_to_ucs -> probe_state
val look_probe : probe_state -> int -> int
val next_probe : probe_state -> int -> probe_state option

type t = {
  name : string;
  ucs_to_enc : string Tbl31.t;
  enc_to_ucs : enc_to_ucs;
} 

type data = Alias of string | CMap of t

val of_name : string -> t
end

module Configure(Config : ConfigInt.Type) : Interface
