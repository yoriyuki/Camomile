(** Configuration values *)
module type Type = sig
  (** Directory of compiled Unicode data *)
  val datadir : string

  (** Directory of compiled character mapping tables a la ISO *)
  val charmapdir : string

  (** Directory of camomile-style compiled character mapping table *)
  val unimapdir : string

  (** Directory of compiled locale data *)
  val localedir : string
end

module Default : Type
