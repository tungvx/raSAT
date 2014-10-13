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
  | ITE
  | EQ
  | GEQ
  | LEQ
  | GR
  | LE

open Parsing;;
# 1 "ParserSmt.mly"

open Parsing
open Exp
# 32 "ParserSmt.ml"
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
  273 (* ITE *);
  274 (* EQ *);
  275 (* GEQ *);
  276 (* LEQ *);
  277 (* GR *);
  278 (* LE *);
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
\006\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\001\000\003\000\003\000\003\000\002\000\
\002\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\001\000\001\000\002\000\003\000\
\003\000\002\000\003\000\003\000\003\000\001\000\001\000\001\000\
\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\036\000\000\000\000\000\021\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\004\000\001\000\003\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\030\000\031\000\032\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\017\000\021\000\000\000\009\000\008\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\018\000\006\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\013\000\014\000\
\015\000\016\000\010\000\023\000\033\000\000\000\024\000\025\000\
\027\000\028\000\029\000\035\000"

let yydgoto = "\002\000\
\005\000\006\000\024\000\021\000\058\000\078\000\061\000\079\000"

let yysindex = "\014\000\
\000\255\000\000\000\255\069\255\000\000\016\000\018\255\000\000\
\069\255\003\255\145\255\069\255\145\255\145\255\179\255\179\255\
\179\255\179\255\179\255\000\000\000\000\000\000\000\000\021\255\
\024\255\105\255\003\255\125\255\145\255\145\255\069\255\145\255\
\000\000\000\000\000\000\000\000\179\255\179\255\179\255\179\255\
\179\255\179\255\179\255\179\255\179\255\179\255\179\255\000\000\
\000\000\000\000\105\255\000\000\000\000\006\255\105\255\125\255\
\000\000\003\255\024\255\145\255\000\000\000\000\000\000\026\255\
\179\255\189\255\179\255\179\255\032\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\179\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\157\255\000\000\032\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\031\000\252\255\045\000\251\255\002\000\226\255\192\255"

let yytablesize = 302
let yytable = "\020\000\
\026\000\063\000\081\000\003\000\028\000\026\000\027\000\031\000\
\026\000\027\000\075\000\004\000\034\000\084\000\001\000\022\000\
\043\000\044\000\045\000\046\000\047\000\054\000\023\000\057\000\
\021\000\048\000\062\000\053\000\049\000\076\000\077\000\022\000\
\083\000\007\000\000\000\000\000\000\000\000\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\000\000\054\000\000\000\064\000\025\000\000\000\030\000\
\053\000\032\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\080\000\000\000\082\000\052\000\008\000\
\009\000\059\000\060\000\000\000\060\000\000\000\000\000\010\000\
\000\000\011\000\012\000\013\000\014\000\000\000\015\000\016\000\
\017\000\018\000\019\000\000\000\000\000\000\000\000\000\059\000\
\000\000\000\000\000\000\052\000\025\000\000\000\000\000\000\000\
\060\000\034\000\035\000\050\000\051\000\000\000\038\000\039\000\
\040\000\041\000\042\000\000\000\000\000\011\000\000\000\013\000\
\014\000\000\000\015\000\016\000\017\000\018\000\019\000\055\000\
\056\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\011\000\012\000\013\000\014\000\000\000\015\000\016\000\
\017\000\018\000\019\000\008\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\013\000\
\014\000\011\000\015\000\016\000\017\000\018\000\019\000\011\000\
\000\000\011\000\011\000\011\000\011\000\000\000\011\000\011\000\
\011\000\011\000\011\000\034\000\035\000\036\000\037\000\000\000\
\038\000\039\000\040\000\041\000\042\000\034\000\035\000\036\000\
\037\000\000\000\000\000\000\000\040\000\041\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\026\000\026\000\
\000\000\000\000\000\000\026\000\000\000\026\000\026\000\026\000\
\026\000\034\000\026\000\026\000\026\000\026\000\026\000\034\000\
\000\000\034\000\034\000\034\000\034\000\021\000\034\000\034\000\
\034\000\034\000\034\000\021\000\022\000\000\000\021\000\000\000\
\000\000\000\000\022\000\000\000\000\000\022\000"

let yycheck = "\004\000\
\000\000\032\000\067\000\004\001\010\000\003\001\004\001\012\000\
\003\001\004\001\005\001\012\001\000\000\078\000\001\000\000\000\
\015\000\016\000\017\000\018\000\019\000\027\000\005\001\028\000\
\000\000\005\001\031\000\026\000\005\001\060\000\005\001\000\000\
\001\001\003\000\255\255\255\255\255\255\255\255\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\255\255\056\000\255\255\051\000\009\000\255\255\011\000\
\055\000\013\000\014\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\066\000\255\255\068\000\026\000\003\001\
\004\001\029\000\030\000\255\255\032\000\255\255\255\255\011\001\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\255\255\255\255\255\255\255\255\051\000\
\255\255\255\255\255\255\055\000\056\000\255\255\255\255\255\255\
\060\000\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\255\255\015\001\
\016\001\255\255\018\001\019\001\020\001\021\001\022\001\003\001\
\004\001\255\255\255\255\255\255\255\255\255\255\255\255\011\001\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\015\001\
\016\001\005\001\018\001\019\001\020\001\021\001\022\001\011\001\
\255\255\013\001\014\001\015\001\016\001\255\255\018\001\019\001\
\020\001\021\001\022\001\001\001\002\001\003\001\004\001\255\255\
\006\001\007\001\008\001\009\001\010\001\001\001\002\001\003\001\
\004\001\255\255\255\255\255\255\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\007\001\
\255\255\255\255\255\255\011\001\255\255\013\001\014\001\015\001\
\016\001\005\001\018\001\019\001\020\001\021\001\022\001\011\001\
\255\255\013\001\014\001\015\001\016\001\005\001\018\001\019\001\
\020\001\021\001\022\001\011\001\005\001\255\255\014\001\255\255\
\255\255\255\255\011\001\255\255\255\255\014\001"

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
  ITE\000\
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
# 44 "ParserSmt.mly"
                  ( _1 )
# 238 "ParserSmt.ml"
               : Exp.ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 47 "ParserSmt.mly"
                                 ( _2 )
# 245 "ParserSmt.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fass_expr) in
    Obj.repr(
# 48 "ParserSmt.mly"
                                 ( _2 )
# 252 "ParserSmt.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 51 "ParserSmt.mly"
                                     ( Ch _1 )
# 259 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 52 "ParserSmt.mly"
                                 ( As (_2, _3) )
# 267 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 53 "ParserSmt.mly"
                            ( Conj (_2, _3))
# 275 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    Obj.repr(
# 54 "ParserSmt.mly"
                                 ( _2 )
# 282 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 57 "ParserSmt.mly"
                                     ( PEq (_1, _2) )
# 290 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 58 "ParserSmt.mly"
                                     ( BEq (_1, _2) )
# 298 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    Obj.repr(
# 59 "ParserSmt.mly"
                                 ( _2 )
# 305 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'let_expr) in
    Obj.repr(
# 60 "ParserSmt.mly"
                                 ( Let (_1, _2) )
# 313 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 63 "ParserSmt.mly"
                                         ( Eq  (_2, _3) )
# 321 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 64 "ParserSmt.mly"
                                         ( (*print_endline (bool_toString 0 (Geq ($2, $3))); flush stdout;*)Geq (_2, _3) )
# 329 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 65 "ParserSmt.mly"
                                         ( Leq (_2, _3) )
# 337 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 66 "ParserSmt.mly"
                                         ( Gr  (_2, _3) )
# 345 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 67 "ParserSmt.mly"
                                         ( Le  (_2, _3) )
# 353 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    Obj.repr(
# 68 "ParserSmt.mly"
                                     ( _2 )
# 360 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 69 "ParserSmt.mly"
                                          ( And (_2, _3) )
# 368 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 70 "ParserSmt.mly"
                                         ( Or (_2, _3) )
# 376 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 71 "ParserSmt.mly"
                                     ( Not _2 )
# 383 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "ParserSmt.mly"
                                 ( BVar _1 )
# 390 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 75 "ParserSmt.mly"
                                  (_1)
# 397 "ParserSmt.ml"
               : 'smt_bool_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 76 "ParserSmt.mly"
                                  (Multiple(_1, _2))
# 405 "ParserSmt.ml"
               : 'smt_bool_exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_exprs) in
    Obj.repr(
# 79 "ParserSmt.mly"
                                          ( Add (_2, _3) )
# 413 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 80 "ParserSmt.mly"
                                         ( Sub (_2, _3) )
# 421 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 81 "ParserSmt.mly"
                                     ( Sub (Real (0.0), _2) )
# 428 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_exprs) in
    Obj.repr(
# 82 "ParserSmt.mly"
                                          ( Mul (_2, _3) )
# 436 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 83 "ParserSmt.mly"
                                         ( Div (_2, _3) )
# 444 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "ParserSmt.mly"
                                     ( Pow (_2, int_of_string _3) )
# 452 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "ParserSmt.mly"
                                 ( Real (float_of_string _1) )
# 459 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "ParserSmt.mly"
                                 ( Var _1 )
# 466 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "ParserSmt.mly"
                                 ( SubVar _1 )
# 473 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    Obj.repr(
# 88 "ParserSmt.mly"
                                     ( _2 )
# 480 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 91 "ParserSmt.mly"
                  (_1)
# 487 "ParserSmt.ml"
               : 'smt_poly_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_exprs) in
    Obj.repr(
# 92 "ParserSmt.mly"
                                 (MultiplePoly (_1, _2))
# 495 "ParserSmt.ml"
               : 'smt_poly_exprs))
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
