(* Database.ml : Unified Interfaces of Stored Data for Camomile
   Copyright (C) 2011 National Institute of Advanced Science and
   Technology
*)

(** [read dir suffix reader key] reads information using [reader].
   Data are supposed to reside in the files under [dir] directory
   with suffix [suffix].  [reader] takes [in_channel] as an argument
   and read data from in_channel.  The [key] specifies key associated
   the value.  Any characters can be used in [key], since they are
   properly escaped.  If there are no data associated to [key], raise
   Not_found. 
  *) 
val read : string -> string -> (in_channel -> 'a) -> string -> 'a

(** [writer dir suffix writer key data] write [data] associated the
 [key] into the directory [dir] with [suffix]. You can use
 any characters in [key] since they are propery escaped.*)
val write : 
    string -> string -> 
      (out_channel -> 'a -> unit) -> string -> 'a -> unit
