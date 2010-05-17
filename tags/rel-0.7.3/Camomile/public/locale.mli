(* $Id: locale.mli,v 1.13 2004/08/29 04:48:21 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki *)

(** Camomile has a locale system similar to Java.
   A locale is a string with a form as
   "<LANG>_<COUNTRY>_<MODIFIER>..." where <LANG> is 
   a 2-letter ISO 639 language code, <COUNTRY> is a 2-letter ISO 3166
   country code.  Some field may not present. *)

(** Type of locales. *)
type t = string

(** [read root suffix reader locale]
   reads locale information using [reader].  
   Locale data is supposed to reside in [root] directory with 
   the name [locale].[suffix].
   [reader] takes [in_channel] as an argument and read data from in_channel.
   If data is not found, then [reader] should raise Not_found.
   If the file is not found or [reader] raises Not_found, then 
   more generic locales are tried.
   For example, if fr_CA.[suffix] is not found, then [read] tries fr.[suffix].
   If fr.[suffix] is also not found, then the file [root].[suffix] is tried.
   Still the data is not found, then [Not_found] is raised. *)
val read : string -> string -> (in_channel -> 'a) -> string -> 'a

(** [contain loc1 loc2] :
   If [loc1] is contained in [loc2] then true otherwise false.
   For example, "fr" is contained in "fr_CA" while "en_CA" 
   does not contain "fr" *)
val contain : string -> string -> bool
