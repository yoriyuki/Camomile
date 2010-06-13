(* Copyright 2010 Pierre Chambart *)

module type Type =
sig
  type text

  exception Prohibited of UChar.t
  exception Bad_bidi

  type profile =
    [ `Nameprep (** RFC 3491 *)
    | `Nodeprep (** RFC 3920, Appendix A *)
    | `Resourceprep (** RFC 3920, Appendix B *)
    | `Saslprep (** RFC 4013 *)
    | `Trace (** for SASL Anonymous, RFC 4505, Section 3 *)
    | `Iscsi (** RFC 3722 *)
    | `Mib (** RFC 4011 *) ]

  val stringprep : profile -> text -> text

end

module Make (Config : ConfigInt.Type) (Text : UnicodeString.Type) :
  Type with type text = Text.t

