(* $Id: caseMap.mli,v 1.7 2006/08/06 19:48:55 yori Exp $ *)
(* Copyright 2002, 2004 Yamagata Yoriyuki *)

(** Case mappings as defined in Unicode Technical Report #21 *)

(** For locale, see {!Locale}.
   If locale is omitted, default mapping is used. *)

module type Type =
  sig
    type text

    val lowercase : ?locale:string -> text -> text
    val uppercase : ?locale:string -> text -> text

    (** Capitalize the beginning of words *)
    val titlecase : ?locale:string -> text -> text

    (** Case foldding *)
    val casefolding : text -> text

    (** Caseless comparison *)
    val compare_caseless : text -> text -> int
  end

module Make (Config : ConfigInt.Type) (Text : UnicodeString.Type) :  (Type with type text = Text.t)
