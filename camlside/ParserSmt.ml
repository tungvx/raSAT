type token =
  | EOF
  | NUM of (string)
  | ID of (string)
  | SUBVAR of (string)
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | POWER
  | LET
  | ASSERT
  | AND
  | CONJ
  | OR
  | NOT
  | EQ
  | GEQ
  | LEQ
  | GR
  | LE

open Parsing;;
# 1 "ParserSmt.mly"

open Parsing
open Exp
# 31 "ParserSmt.ml"
let yytransl_const = [|
    0 (* EOF *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* TIMES *);
  265 (* DIV *);
  266 (* POWER *);
  267 (* LET *);
  268 (* ASSERT *);
  269 (* AND *);
  270 (* CONJ *);
  271 (* OR *);
  272 (* NOT *);
  273 (* EQ *);
  274 (* GEQ *);
  275 (* LEQ *);
  276 (* GR *);
  277 (* LE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
  259 (* SUBVAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\005\000\
\005\000\005\000\005\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\007\000\007\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\001\000\003\000\003\000\003\000\002\000\
\002\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\001\000\001\000\002\000\003\000\
\003\000\002\000\003\000\003\000\003\000\001\000\001\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\004\000\001\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\030\000\031\000\032\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\017\000\021\000\000\000\009\000\008\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\018\000\006\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\013\000\014\000\
\015\000\016\000\010\000\023\000\033\000\024\000\025\000\027\000\
\028\000\029\000"

let yydgoto = "\002\000\
\005\000\006\000\024\000\021\000\058\000\053\000\061\000"

let yysindex = "\011\000\
\004\255\000\000\004\255\054\255\000\000\020\000\017\255\000\000\
\054\255\011\255\128\255\054\255\128\255\128\255\166\255\166\255\
\166\255\166\255\166\255\000\000\000\000\000\000\000\000\018\255\
\031\255\090\255\011\255\109\255\128\255\128\255\054\255\128\255\
\000\000\000\000\000\000\000\000\166\255\166\255\166\255\166\255\
\166\255\166\255\166\255\166\255\166\255\166\255\166\255\000\000\
\000\000\000\000\090\255\000\000\000\000\006\255\090\255\109\255\
\000\000\011\255\031\255\128\255\000\000\000\000\000\000\037\255\
\166\255\176\255\166\255\166\255\044\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\000\000\
\000\000\145\255\000\000\019\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\044\000\009\000\030\000\246\255\243\255\231\255"

let yytablesize = 289
let yytable = "\028\000\
\026\000\043\000\044\000\045\000\046\000\047\000\063\000\003\000\
\026\000\027\000\075\000\001\000\020\000\026\000\027\000\004\000\
\054\000\021\000\022\000\022\000\031\000\023\000\048\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\076\000\049\000\057\000\064\000\025\000\062\000\
\030\000\077\000\032\000\033\000\082\000\054\000\007\000\000\000\
\000\000\000\000\000\000\078\000\079\000\080\000\081\000\052\000\
\008\000\009\000\059\000\060\000\000\000\060\000\000\000\000\000\
\010\000\000\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\000\000\000\000\000\000\000\000\000\000\
\059\000\000\000\000\000\000\000\052\000\025\000\000\000\000\000\
\000\000\060\000\034\000\035\000\050\000\051\000\000\000\038\000\
\039\000\040\000\041\000\042\000\000\000\000\000\011\000\000\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\055\000\
\056\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\018\000\019\000\008\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\011\000\000\000\000\000\
\000\000\000\000\000\000\011\000\000\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\034\000\035\000\
\036\000\037\000\000\000\038\000\039\000\040\000\041\000\042\000\
\034\000\035\000\036\000\037\000\000\000\000\000\000\000\040\000\
\041\000\042\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\026\000\026\000\
\000\000\000\000\000\000\026\000\000\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\021\000\022\000\
\000\000\000\000\000\000\000\000\021\000\022\000\000\000\021\000\
\022\000"

let yycheck = "\010\000\
\000\000\015\000\016\000\017\000\018\000\019\000\032\000\004\001\
\003\001\004\001\005\001\001\000\004\000\003\001\004\001\012\001\
\027\000\000\000\000\000\000\000\012\000\005\001\005\001\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\060\000\005\001\028\000\051\000\009\000\031\000\
\011\000\005\001\013\000\014\000\001\001\056\000\003\000\255\255\
\255\255\255\255\255\255\065\000\066\000\067\000\068\000\026\000\
\003\001\004\001\029\000\030\000\255\255\032\000\255\255\255\255\
\011\001\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\255\255\255\255\255\255\
\051\000\255\255\255\255\255\255\055\000\056\000\255\255\255\255\
\255\255\060\000\001\001\002\001\003\001\004\001\255\255\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\003\001\
\004\001\255\255\255\255\255\255\255\255\255\255\255\255\011\001\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\003\001\004\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\255\255\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\005\001\255\255\255\255\
\255\255\255\255\255\255\011\001\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\001\001\002\001\
\003\001\004\001\255\255\006\001\007\001\008\001\009\001\010\001\
\001\001\002\001\003\001\004\001\255\255\255\255\255\255\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\007\001\
\255\255\255\255\255\255\011\001\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\005\001\005\001\
\255\255\255\255\255\255\255\255\011\001\011\001\255\255\014\001\
\014\001"

let yynames_const = "\
  EOF\000\
  LPAREN\000\
  RPAREN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  POWER\000\
  LET\000\
  ASSERT\000\
  AND\000\
  CONJ\000\
  OR\000\
  NOT\000\
  EQ\000\
  GEQ\000\
  LEQ\000\
  GR\000\
  LE\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  SUBVAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fass_expr) in
    Obj.repr(
# 43 "ParserSmt.mly"
                  ( _1 )
# 233 "ParserSmt.ml"
               : Exp.ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 46 "ParserSmt.mly"
                                 ( _2 )
# 240 "ParserSmt.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fass_expr) in
    Obj.repr(
# 47 "ParserSmt.mly"
                                 ( _2 )
# 247 "ParserSmt.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 50 "ParserSmt.mly"
                                     ( Ch _1 )
# 254 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 51 "ParserSmt.mly"
                                 ( As (_2, _3) )
# 262 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 52 "ParserSmt.mly"
                            ( Conj (_2, _3))
# 270 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    Obj.repr(
# 53 "ParserSmt.mly"
                                 ( _2 )
# 277 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 56 "ParserSmt.mly"
                                     ( PEq (_1, _2) )
# 285 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 57 "ParserSmt.mly"
                                     ( BEq (_1, _2) )
# 293 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    Obj.repr(
# 58 "ParserSmt.mly"
                                 ( _2 )
# 300 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'let_expr) in
    Obj.repr(
# 59 "ParserSmt.mly"
                                 ( Let (_1, _2) )
# 308 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 62 "ParserSmt.mly"
                                         ( Eq  (_2, _3) )
# 316 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 63 "ParserSmt.mly"
                                         ( Geq (_2, _3) )
# 324 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 64 "ParserSmt.mly"
                                         ( Leq (_2, _3) )
# 332 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 65 "ParserSmt.mly"
                                         ( Gr  (_2, _3) )
# 340 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 66 "ParserSmt.mly"
                                         ( Le  (_2, _3) )
# 348 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    Obj.repr(
# 67 "ParserSmt.mly"
                                     ( _2 )
# 355 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 68 "ParserSmt.mly"
                                          ( And (_2, _3) )
# 363 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 69 "ParserSmt.mly"
                                         ( Or (_2, _3) )
# 371 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 70 "ParserSmt.mly"
                                     ( Not _2 )
# 378 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "ParserSmt.mly"
                                 ( BVar _1 )
# 385 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 74 "ParserSmt.mly"
                                  (_1)
# 392 "ParserSmt.ml"
               : 'smt_bool_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 75 "ParserSmt.mly"
                                   (Multiple(_1, _2))
# 400 "ParserSmt.ml"
               : 'smt_bool_exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 78 "ParserSmt.mly"
                                         ( Add (_2, _3) )
# 408 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 79 "ParserSmt.mly"
                                         ( Sub (_2, _3) )
# 416 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 80 "ParserSmt.mly"
                                     ( Sub (Real (0.0), _2) )
# 423 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 81 "ParserSmt.mly"
                                         ( Mul (_2, _3) )
# 431 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 82 "ParserSmt.mly"
                                         ( Div (_2, _3) )
# 439 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "ParserSmt.mly"
                                     ( Pow (_2, int_of_string _3) )
# 447 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "ParserSmt.mly"
                                 ( Real (float_of_string _1) )
# 454 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "ParserSmt.mly"
                                 ( Var _1 )
# 461 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "ParserSmt.mly"
                                 ( SubVar _1 )
# 468 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    Obj.repr(
# 87 "ParserSmt.mly"
                                     ( _2 )
# 475 "ParserSmt.ml"
               : 'smt_poly_expr))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Exp.ass_expr)
