type token =
  | IC
  | IN
  | ID of (string)
  | NUM of (string)
  | LPAREN
  | RPAREN
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * IA.interval) list
