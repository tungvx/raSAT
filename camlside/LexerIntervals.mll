{
open Lexing
open ParserIntervals
}

let alpha = ['a'-'z' 'A'-'Z' '_']
let num   = ['0'-'9' '.']

rule lex = parse
  | [' ' '\r' '\t'] {  lex lexbuf }
  | '\n'   { new_line lexbuf; lex lexbuf }
  | ('-'? num+ (['e''E'] ['+''-']? ['0'-'9']+)?) | ("inf") | ("-inf") as s { NUM s }
  | "in"   { IN }
  | "ic"   { IC }
  | alpha (alpha|num)* as s { ID s }
  | "("    { LPAREN }
  | ")"    { RPAREN }  
  | eof    { EOF }
