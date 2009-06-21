type token =
  | UCHAR of (UChar.t)
  | OPTION of (string list)
  | PRIMARY
  | SECONDARY
  | TERTIARY
  | EQ
  | RESET
  | EXPAND
  | PREFIX
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AbsCe.ace_info
