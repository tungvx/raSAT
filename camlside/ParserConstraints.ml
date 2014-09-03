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

open Parsing;;
# 1 "ParserConstraints.mly"

open Parsing
open Ast
open PolynomialConstraint
# 27 "ParserConstraints.ml"
let yytransl_const = [|
    0 (* EOF *);
  259 (* LPAREN *);
  260 (* RPAREN *);
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* TIMES *);
  264 (* POWER *);
  265 (* ASSERT *);
  266 (* AND *);
  267 (* OR *);
  268 (* EQ *);
  269 (* GEQ *);
  270 (* LEQ *);
  271 (* GR *);
  272 (* LE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\001\000\003\000\000\000\000\000\000\000\016\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\010\000\011\000\000\000\000\000\000\000\000\000\
\000\000\004\000\005\000\006\000\007\000\008\000\018\000\012\000\
\013\000\014\000\015\000"

let yydgoto = "\002\000\
\005\000\006\000\016\000\029\000"

let yysindex = "\003\000\
\002\255\000\000\002\255\007\255\000\000\006\000\003\255\007\255\
\007\255\007\255\031\255\031\255\031\255\031\255\031\255\000\000\
\000\000\000\000\004\255\007\255\007\255\000\000\000\000\031\255\
\031\255\031\255\031\255\031\255\008\255\023\255\024\255\034\255\
\039\255\000\000\000\000\000\000\040\255\031\255\031\255\031\255\
\042\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\042\000\021\000\244\255"

let yytablesize = 45
let yytable = "\030\000\
\031\000\032\000\033\000\001\000\003\000\017\000\018\000\034\000\
\042\000\008\000\004\000\037\000\038\000\039\000\040\000\041\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\043\000\
\044\000\048\000\049\000\050\000\019\000\020\000\021\000\022\000\
\023\000\024\000\045\000\025\000\026\000\027\000\028\000\046\000\
\035\000\036\000\051\000\047\000\007\000"

let yycheck = "\012\000\
\013\000\014\000\015\000\001\000\003\001\000\000\004\001\004\001\
\001\001\003\001\009\001\024\000\025\000\026\000\027\000\028\000\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\001\001\038\000\039\000\040\000\008\000\009\000\010\000\001\001\
\002\001\003\001\001\001\005\001\006\001\007\001\008\001\001\001\
\020\000\021\000\001\001\004\001\003\000"

let yynames_const = "\
  EOF\000\
  LPAREN\000\
  RPAREN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  POWER\000\
  ASSERT\000\
  AND\000\
  OR\000\
  EQ\000\
  GEQ\000\
  LEQ\000\
  GR\000\
  LE\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'f_expr) in
    Obj.repr(
# 40 "ParserConstraints.mly"
               ( _1 )
# 141 "ParserConstraints.ml"
               : PolynomialConstraint.constraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'polynomialConstraints) in
    Obj.repr(
# 43 "ParserConstraints.mly"
                                 ( _2 )
# 148 "ParserConstraints.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'f_expr) in
    Obj.repr(
# 44 "ParserConstraints.mly"
                                 ( _2 )
# 155 "ParserConstraints.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Single (new polynomialConstraint (Eq _2))
                                   else Single (new polynomialConstraint (Eq (Sub(_2, Real number))))
                                 )
# 167 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Single (new polynomialConstraint (Geq  _2))
                                   else Single (new polynomialConstraint (Geq (Sub(_2, Real number))))
                                 )
# 179 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Single (new polynomialConstraint(Leq  _2))
                                   else Single (new polynomialConstraint(Leq (Sub(_2, Real number))))
                                 )
# 191 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Single (new polynomialConstraint(Gr  _2))
                                   else Single (new polynomialConstraint(Gr (Sub(_2, Real number))))
                                 )
# 203 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Single (new polynomialConstraint(Le  _2))
                                   else Single (new polynomialConstraint(Le (Sub(_2, Real number))))
                                 )
# 215 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'polynomialConstraints) in
    Obj.repr(
# 72 "ParserConstraints.mly"
                                             ( _2 )
# 222 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'polynomialConstraints) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'polynomialConstraints) in
    Obj.repr(
# 73 "ParserConstraints.mly"
                                                         ( And (_2, _3) )
# 230 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'polynomialConstraints) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'polynomialConstraints) in
    Obj.repr(
# 74 "ParserConstraints.mly"
                                                         ( BOr (_2, _3) )
# 238 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 77 "ParserConstraints.mly"
                                 ( Add (_2, _3) )
# 246 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 78 "ParserConstraints.mly"
                                 ( Sub (_2, _3) )
# 254 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 79 "ParserConstraints.mly"
                                 ( Mul (_2, _3) )
# 262 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "ParserConstraints.mly"
                                 ( Pow (_2, int_of_string _3) )
# 270 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "ParserConstraints.mly"
                                 ( Real (float_of_string _1) )
# 277 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "ParserConstraints.mly"
                                 ( Var _1 )
# 284 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    Obj.repr(
# 83 "ParserConstraints.mly"
                                 ( _2 )
# 291 "ParserConstraints.ml"
               : 'poly_expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : PolynomialConstraint.constraints)
