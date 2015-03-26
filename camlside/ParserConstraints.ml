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

open Parsing;;
# 1 "ParserConstraints.mly"

open Parsing
open Ast
open PolynomialConstraint
open Variable
# 29 "ParserConstraints.ml"
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
  273 (* NEQ *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\001\000\003\000\000\000\000\000\000\000\017\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\000\011\000\012\000\000\000\000\000\
\000\000\000\000\000\000\004\000\006\000\007\000\008\000\009\000\
\005\000\019\000\013\000\014\000\015\000\016\000"

let yydgoto = "\002\000\
\005\000\006\000\017\000\030\000"

let yysindex = "\004\000\
\003\255\000\000\003\255\008\255\000\000\007\000\004\255\008\255\
\008\255\008\255\033\255\033\255\033\255\033\255\033\255\033\255\
\000\000\000\000\000\000\005\255\008\255\008\255\000\000\000\000\
\033\255\033\255\033\255\033\255\033\255\009\255\025\255\026\255\
\036\255\041\255\042\255\000\000\000\000\000\000\043\255\033\255\
\033\255\033\255\045\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\045\000\023\000\244\255"

let yytablesize = 48
let yytable = "\031\000\
\032\000\033\000\034\000\035\000\001\000\003\000\018\000\019\000\
\036\000\044\000\008\000\004\000\039\000\040\000\041\000\042\000\
\043\000\009\000\010\000\011\000\012\000\013\000\014\000\015\000\
\016\000\045\000\046\000\051\000\052\000\053\000\020\000\021\000\
\022\000\023\000\024\000\025\000\047\000\026\000\027\000\028\000\
\029\000\048\000\049\000\037\000\038\000\054\000\050\000\007\000"

let yycheck = "\012\000\
\013\000\014\000\015\000\016\000\001\000\003\001\000\000\004\001\
\004\001\001\001\003\001\009\001\025\000\026\000\027\000\028\000\
\029\000\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\001\001\001\001\040\000\041\000\042\000\008\000\009\000\
\010\000\001\001\002\001\003\001\001\001\005\001\006\001\007\001\
\008\001\001\001\001\001\021\000\022\000\001\001\004\001\003\000"

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
  NEQ\000\
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
# 42 "ParserConstraints.mly"
               ( _1 )
# 145 "ParserConstraints.ml"
               : PolynomialConstraint.constraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'polynomialConstraints) in
    Obj.repr(
# 45 "ParserConstraints.mly"
                                 ( _2 )
# 152 "ParserConstraints.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'f_expr) in
    Obj.repr(
# 46 "ParserConstraints.mly"
                                 ( _2 )
# 159 "ParserConstraints.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   let (polyCons, variablesSet) = _2 in
                                   if number = 0. then Single (new polynomialConstraint (Eq polyCons) variablesSet)
                                   else Single (new polynomialConstraint (Eq (Sub(_2, (Real number, VariablesSet.empty)))) variablesSet)
                                 )
# 172 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "ParserConstraints.mly"
                                  ( 
                                   let number = float_of_string _3 in
                                   let (polyCons, variablesSet) = _2 in
                                   if number = 0. then Single (new polynomialConstraint (Neq polyCons) variablesSet)
                                   else Single (new polynomialConstraint (Neq (Sub(_2, (Real number, variablesSet)))) variablesSet)
                                 )
# 185 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   let (polyCons, variablesSet) = _2 in
                                   if number = 0. then Single (new polynomialConstraint (Geq  polyCons) variablesSet)
                                   else Single (new polynomialConstraint (Geq (Sub(_2, (Real number, variablesSet)))) variablesSet)
                                 )
# 198 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   let (polyCons, variablesSet) = _2 in
                                   if number = 0. then Single (new polynomialConstraint(Leq polyCons) variablesSet)
                                   else Single (new polynomialConstraint(Leq (Sub(_2, (Real number, variablesSet)))) variablesSet)
                                 )
# 211 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   let (polyCons, variablesSet) = _2 in
                                   if number = 0. then Single (new polynomialConstraint(Gr  polyCons) variablesSet)
                                   else Single (new polynomialConstraint(Gr (Sub(_2, (Real number, variablesSet)))) variablesSet)
                                 )
# 224 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "ParserConstraints.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   let (polyCons, variablesSet) = _2 in
                                   if number = 0. then Single (new polynomialConstraint(Le  polyCons) variablesSet)
                                   else Single (new polynomialConstraint(Le (Sub(_2, (Real number, variablesSet)))) variablesSet)
                                 )
# 237 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'polynomialConstraints) in
    Obj.repr(
# 85 "ParserConstraints.mly"
                                             ( _2 )
# 244 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'polynomialConstraints) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'polynomialConstraints) in
    Obj.repr(
# 86 "ParserConstraints.mly"
                                                         ( And (_2, _3) )
# 252 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'polynomialConstraints) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'polynomialConstraints) in
    Obj.repr(
# 87 "ParserConstraints.mly"
                                                         ( BOr (_2, _3) )
# 260 "ParserConstraints.ml"
               : 'polynomialConstraints))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 90 "ParserConstraints.mly"
                                 ( let (polyExpr2, variablesSet2) = _2 in let (polyExpr3, variablesSet3) = _3 in 
                                   (Add (_2, _3), VariablesSet.union variablesSet2 variablesSet3) )
# 269 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 92 "ParserConstraints.mly"
                                 ( let (polyExpr2, variablesSet2) = _2 in let (polyExpr3, variablesSet3) = _3 in 
                                   (Sub (_2, _3), VariablesSet.union variablesSet2 variablesSet3) )
# 278 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 94 "ParserConstraints.mly"
                                 ( let (polyExpr2, variablesSet2) = _2 in let (polyExpr3, variablesSet3) = _3 in 
                                   (Mul (_2, _3), VariablesSet.union variablesSet2 variablesSet3) )
# 287 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "ParserConstraints.mly"
                                 ( let (polyExpr, variablesSet) = _2 in (Pow (_2, int_of_string _3), variablesSet) )
# 295 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "ParserConstraints.mly"
                                 ( (Real (float_of_string _1), VariablesSet.empty) )
# 302 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "ParserConstraints.mly"
                                 ( (Var _1, VariablesSet.singleton _1) )
# 309 "ParserConstraints.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    Obj.repr(
# 99 "ParserConstraints.mly"
                                 ( _2 )
# 316 "ParserConstraints.ml"
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
