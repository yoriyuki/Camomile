type token = Text of string | Brace_r | Brace_l | Colon | Comma

val pp : Format.formatter -> token -> unit
val lexer : Camomile.UChar.t Stream.t -> token list
