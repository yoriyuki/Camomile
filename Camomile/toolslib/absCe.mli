(* $Id: absCe.mli,v 1.16 2006/08/13 17:26:41 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki *)

open Toolslib
open UCol

type elt =
  [ `Seq of UChar.t list
  | `ImplicitWeight of int list
  | `CompleteIgnorable
  | `UCA_Weight of int
  | `LastVariable
  | `HiraganaQ
  | `FirstImplicit
  | `FirstTrailing ]

type ce

type ceset

module EltMap : Map.S with type key = elt

val ces_of : ceset -> UChar.t list -> ceset * ce list
val complete_ignorable : ceset -> ce
val last_variable : ceset -> ce
val first_implicit : ceset -> ce
val first_trailing : ceset -> ce
val top : ceset -> ce
val next : precision -> ce -> ceset -> ce
val prev : precision -> ce -> ceset -> ce
val add_after : precision -> ce -> ceset -> ce * ceset
val add_before : precision -> ce -> ceset -> ce * ceset
val put : elt -> ce list -> ceset -> ceset

val import : int list EltMap.t * int list EltMap.t * int list EltMap.t -> ceset

type ace_info =
    {ceset : ceset;
     variable_option : variable_option;
     french : bool;
     hiraganaQ : bool}

val create_ace_info :
    ?variable_option:variable_option ->
    ?french:bool ->
    ?hiraganaQ:bool ->
    ceset ->
      ace_info

val cetbl_of : ace_info -> Unidata.col_info

type aceset_info = 
    {lowercase_first_tbl : ceset;
     uppercase_first_tbl : ceset}
