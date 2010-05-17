(* $Id: uRe.mli,v 1.6 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** Regular expression engine. *)

(** Abstract syntax trees of regular expressions. *)
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

(** Match semantics. *)
type match_semantics = [ `First | `Shortest | `Longest ]

(** Remove [`Group] from the regular expressions. *)
val no_group : regexp -> regexp

module type Type = sig
  type text
  type index
  type compiled_regexp

  module SubText : 
    SubText.Type with type ur_text = text and type ur_index = index

(** Compile regular expressions. *)
  val compile : regexp -> compiled_regexp

(** [regexp_match ?sem r t i] tries matching [r] and substrings
   of [t] beginning from [i].  If match successes,  [Some g] is 
   returned where [g] is the array containing the matched 
   string of [n]-th group in the [n]-element.  
   The matched string of the whole [r] is stored in the [0]-th element.  
   If matching fails, [None] is returned. *)
  val regexp_match : ?sem:match_semantics ->
    compiled_regexp -> text -> index -> SubText.t option array option

(** [string_match r t i] tests whether [r] can match a substring
   of [t] beginning from [i]. *)
  val string_match : compiled_regexp -> text -> index -> bool

(** [search_forward ?sem r t i] searches a substring of [t]
   matching [r] from [i].  The returned value is similar to 
   {!URe.Type.regexp_match}. *)
  val search_forward : ?sem:match_semantics ->
      compiled_regexp -> text -> index -> SubText.t option array option
end

module Make : functor (Text : UnicodeString.Type) ->
  Type with type text = Text.t and type index = Text.index
