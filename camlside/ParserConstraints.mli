type token =
  | EOF
  | NUM of (string)
  | ID of (string)
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | POWER
  | ASSERT
  | AND
  | OR
  | EQ
  | GEQ
  | LEQ
  | GR
  | LE
  | NEQ

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> PolynomialConstraint.constraints
