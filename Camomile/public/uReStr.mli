(* $Id: uReStr.mli,v 1.7 2006/08/13 20:28:58 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** Module for a Str-like regular expression syntax.
   The difference can be summarised as follows.
   - Non-ASCII characters can be used if encoded by UTF-8, or
   using the escape syntax \u<code number as hex digits>.
   - Each Unicode character is treated as a single character.
   - Character properties like Lu ({!UCharInfo.general_category_type}),
   White_Space ({!UCharInfo.character_property_type}),
   Ogham ({!UCharInfo.script_type}) can be used in character sets. e.g.
   \[\{Lu & ID_Start\}\]\[\{ID_Continue\}\]* (capitalised identifier),
   \(\[\{Han\}\]+\|\[\{Katakana\}\]+\)\[\{Hiragana\}\]* 
   (Japanese word component).
   Boolean notations as | (or) :, & (and) - (set subtraction) can be used
   in \{...\} notations.  Any is used to denote the set of all characters
   in \{...\} notations.

*)

module type Interface = sig

type regexp = URe.regexp

(** Theses functions are similar to Str. *)

val regexp : string -> regexp
val quote : string -> string
val regexp_string : string -> regexp

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
  val regexp_match : ?sem:URe.match_semantics ->
    compiled_regexp -> text -> index -> SubText.t option array option

(** [string_match r t i] tests whether [r] can match a substring
   of [t] beginning from [i]. *)
  val string_match : compiled_regexp -> text -> index -> bool

(** [search_forward ?sem r t i] searches a substring of [t]
   matching [r] from [i].  The returned value is similar to 
   {!URe.Type.regexp_match}. *)
  val search_forward : ?sem:URe.match_semantics ->
      compiled_regexp -> text -> index -> SubText.t option array option
end

module Make (Text : UnicodeString.Type) : 
    Type with type text = Text.t and type index = Text.index
end

module Configure (Config : ConfigInt.Type) : Interface
