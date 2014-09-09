{
open Lexing
open ParserConstraints
}

let alpha = ['a'-'z' 'A'-'Z' '_']
let num   = ['0'-'9' '.']

rule lex = parse
  | [' ' '\r' '\t'] {  lex lexbuf }
  | '\n'   { new_line lexbuf; lex lexbuf }
  | ('-'? num+ (['e''E'] ['+''-']? ['0'-'9']+)?) | ("inf") | ("-inf") as s { NUM s }
  | "and"  { AND }
  | "or"   { OR }
  | "assert" { ASSERT }
  | alpha (alpha|num)* as s { ID s }
  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { TIMES }
  | "^"    { POWER }
  | "!="    { NEQ }
  | "="    { EQ }
  | ">="   { GEQ }
  | "<="   { LEQ }
  | ">"    { GR  }
  | "<"    { LE  }
  | "("    { LPAREN }
  | ")"    { RPAREN }  
  | eof    { EOF }
