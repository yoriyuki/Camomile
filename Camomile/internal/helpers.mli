(** Ensure /, \, are not contained in strings.  Raise Failwith if *)
(** they are containd. *)
val sanitize : string -> unit 
