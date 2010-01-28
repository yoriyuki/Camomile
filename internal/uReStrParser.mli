type token =
  | UCHAR of (UChar.t)
  | ASCII of (char)
  | DOT
  | ASTARISK
  | REPN of (int * (int option) * string)
  | PLUS
  | QUESTION
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | MINUS
  | HAT
  | DOLLAR
  | ALT
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | SPACE
  | AND
  | OR
  | COLON
  | BOS
  | EOS
  | END

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> UReStrParserType.tree
