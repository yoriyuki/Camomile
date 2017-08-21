
type token =
  | Text of string
  | Brace_r
  | Brace_l
  | Colon
  | Comma

val lexer : UChar.t Stream.t -> token list
