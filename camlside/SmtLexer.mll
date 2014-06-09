{
open Lexing
open SmtParser
}

let alpha = ['a'-'z' 'A'-'Z' '_']
let num   = ['0'-'9' '.']

rule lex = parse
  | [' ' '\r' '\t'] {  lex lexbuf }
  | '\n'   { new_line lexbuf; lex lexbuf }
  | ('-'? num+) | ("inf") | ("-inf") as s { NUM s }
  | "and"  { AND }  
  | "&"	{ CONJ}   
  | "or"   { OR }
  | "not"  { NOT }
  | "let"  { LET }
  | "assert" { ASSERT }
  | alpha (alpha|num)* as s { ID s }
  | '?'(alpha|num)* as s {SUBVAR s}
  | "+"    { PLUS }
  | "-"    { MINUS }
  | "*"    { TIMES }
  | "/"    { DIV }
  | "^"    { POWER }
  | "="    { EQ }
  | ">="   { GEQ }
  | "<="   { LEQ }
  | ">"    { GR  }
  | "<"    { LE  }
  | "("    { LPAREN }
  | ")"    { RPAREN }  
  | eof    { EOF }
