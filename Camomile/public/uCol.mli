(* $Id: uCol.mli,v 1.12 2006/08/13 17:18:12 yori Exp $ *)
(* Copyright 2002, 2003 Yamagata Yoriyuki. distributed with LGPL *)

(** String comparison by collation as described in UTR #10 *)

(** How variables are handled *)
    type variable_option = 
	[ `Blanked 
      | `Non_ignorable 
      | `Shifted
      | `Shift_Trimmed ]
	  
(** Strength of comparison.  For European languages, each strength
    roughly means as
    `Primary : Ignore accents and case
    `Secondary : Ignore case but accents are counted in.
    `Tertiary : Accents and case are counted in.
    For the case of `Shifted, `Shift_Trimmed, there is the fourth strength.
    `Quaternary : Variables such as - (hyphen) are counted in. *)
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

module Make (Config : ConfigInt.Type) (Text : UnicodeString.Type) : (Type with type text = Text.t and type index = Text.index)
