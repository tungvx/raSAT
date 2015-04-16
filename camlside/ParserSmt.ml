type token =
  | NUMERAL of (string)
  | DECIMAL of (string)
  | HEXADECIMAL of (string)
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
open Interval
# 36 "ParserSmt.ml"
let yytransl_const = [|
    0 (* EOF *);
  263 (* LPAREN *);
  264 (* RPAREN *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIV *);
  269 (* POWER *);
  270 (* LET *);
  271 (* ASSERT *);
  272 (* AND *);
  273 (* CONJ *);
  274 (* OR *);
  275 (* NOT *);
  276 (* ITE *);
  277 (* EQ *);
  278 (* GEQ *);
  279 (* LEQ *);
  280 (* GR *);
  281 (* LE *);
    0|]

let yytransl_block = [|
  257 (* NUMERAL *);
  258 (* DECIMAL *);
  259 (* HEXADECIMAL *);
  260 (* NUM *);
  261 (* ID *);
  262 (* SUBVAR *);
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
\253\254\000\000\253\254\066\255\000\000\016\000\015\255\000\000\
\066\255\000\255\142\255\066\255\142\255\142\255\176\255\176\255\
\176\255\176\255\176\255\000\000\000\000\000\000\000\000\018\255\
\021\255\102\255\000\255\122\255\142\255\142\255\066\255\142\255\
\000\000\000\000\000\000\000\000\176\255\176\255\176\255\176\255\
\176\255\176\255\176\255\176\255\176\255\176\255\176\255\000\000\
\000\000\000\000\102\255\000\000\000\000\003\255\102\255\122\255\
\000\000\000\255\021\255\142\255\000\000\000\000\000\000\023\255\
\176\255\186\255\176\255\176\255\029\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\176\255\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\154\255\000\000\032\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\031\000\252\255\045\000\251\255\002\000\226\255\192\255"

let yytablesize = 305
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\026\000\026\000\000\000\000\000\000\000\026\000\000\000\
\026\000\026\000\026\000\026\000\034\000\026\000\026\000\026\000\
\026\000\026\000\034\000\000\000\034\000\034\000\034\000\034\000\
\021\000\034\000\034\000\034\000\034\000\034\000\021\000\022\000\
\000\000\021\000\000\000\000\000\000\000\022\000\000\000\000\000\
\022\000"

let yycheck = "\004\000\
\000\000\032\000\067\000\007\001\010\000\006\001\007\001\012\000\
\006\001\007\001\008\001\015\001\000\000\078\000\001\000\000\000\
\015\000\016\000\017\000\018\000\019\000\027\000\008\001\028\000\
\000\000\008\001\031\000\026\000\008\001\060\000\008\001\000\000\
\004\001\003\000\255\255\255\255\255\255\255\255\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\255\255\056\000\255\255\051\000\009\000\255\255\011\000\
\055\000\013\000\014\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\066\000\255\255\068\000\026\000\006\001\
\007\001\029\000\030\000\255\255\032\000\255\255\255\255\014\001\
\255\255\016\001\017\001\018\001\019\001\255\255\021\001\022\001\
\023\001\024\001\025\001\255\255\255\255\255\255\255\255\051\000\
\255\255\255\255\255\255\055\000\056\000\255\255\255\255\255\255\
\060\000\004\001\005\001\006\001\007\001\255\255\009\001\010\001\
\011\001\012\001\013\001\255\255\255\255\016\001\255\255\018\001\
\019\001\255\255\021\001\022\001\023\001\024\001\025\001\006\001\
\007\001\255\255\255\255\255\255\255\255\255\255\255\255\014\001\
\255\255\016\001\017\001\018\001\019\001\255\255\021\001\022\001\
\023\001\024\001\025\001\006\001\007\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\016\001\255\255\018\001\
\019\001\008\001\021\001\022\001\023\001\024\001\025\001\014\001\
\255\255\016\001\017\001\018\001\019\001\255\255\021\001\022\001\
\023\001\024\001\025\001\004\001\005\001\006\001\007\001\255\255\
\009\001\010\001\011\001\012\001\013\001\004\001\005\001\006\001\
\007\001\255\255\255\255\255\255\011\001\012\001\013\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\009\001\010\001\255\255\255\255\255\255\014\001\255\255\
\016\001\017\001\018\001\019\001\008\001\021\001\022\001\023\001\
\024\001\025\001\014\001\255\255\016\001\017\001\018\001\019\001\
\008\001\021\001\022\001\023\001\024\001\025\001\014\001\008\001\
\255\255\017\001\255\255\255\255\255\255\014\001\255\255\255\255\
\017\001"

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
  NUMERAL\000\
  DECIMAL\000\
  HEXADECIMAL\000\
  NUM\000\
  ID\000\
  SUBVAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fass_expr) in
    Obj.repr(
# 49 "ParserSmt.mly"
                  ( _1 )
# 250 "ParserSmt.ml"
               : Exp.ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 52 "ParserSmt.mly"
                                 ( _2 )
# 257 "ParserSmt.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fass_expr) in
    Obj.repr(
# 53 "ParserSmt.mly"
                                 ( _2 )
# 264 "ParserSmt.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 56 "ParserSmt.mly"
                                     ( Ch _1 )
# 271 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 57 "ParserSmt.mly"
                                 ( As (_2, _3) )
# 279 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 58 "ParserSmt.mly"
                            ( Conj (_2, _3))
# 287 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    Obj.repr(
# 59 "ParserSmt.mly"
                                 ( _2 )
# 294 "ParserSmt.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 62 "ParserSmt.mly"
                                     ( PEq (_1, _2) )
# 302 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 63 "ParserSmt.mly"
                                     ( BEq (_1, _2) )
# 310 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    Obj.repr(
# 64 "ParserSmt.mly"
                                 ( _2 )
# 317 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'let_expr) in
    Obj.repr(
# 65 "ParserSmt.mly"
                                 ( Let (_1, _2) )
# 325 "ParserSmt.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 68 "ParserSmt.mly"
                                         ( Eq  (_2, _3) )
# 333 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 69 "ParserSmt.mly"
                                         ( (*print_endline (bool_toString 0 (Geq ($2, $3))); flush stdout;*)Geq (_2, _3) )
# 341 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 70 "ParserSmt.mly"
                                         ( Leq (_2, _3) )
# 349 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 71 "ParserSmt.mly"
                                         ( Gr  (_2, _3) )
# 357 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 72 "ParserSmt.mly"
                                         ( Le  (_2, _3) )
# 365 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    Obj.repr(
# 73 "ParserSmt.mly"
                                     ( _2 )
# 372 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 74 "ParserSmt.mly"
                                          ( And (_2, _3) )
# 380 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 75 "ParserSmt.mly"
                                         ( Or (_2, _3) )
# 388 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 76 "ParserSmt.mly"
                                     ( Not _2 )
# 395 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "ParserSmt.mly"
                                 ( BVar _1 )
# 402 "ParserSmt.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 80 "ParserSmt.mly"
                                  (_1)
# 409 "ParserSmt.ml"
               : 'smt_bool_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_exprs) in
    Obj.repr(
# 81 "ParserSmt.mly"
                                  (Multiple(_1, _2))
# 417 "ParserSmt.ml"
               : 'smt_bool_exprs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_exprs) in
    Obj.repr(
# 84 "ParserSmt.mly"
                                          ( Add (_2, _3) )
# 425 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 85 "ParserSmt.mly"
                                         ( Sub (_2, _3) )
# 433 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 86 "ParserSmt.mly"
                                     ( Sub (Constant {low=0.;high=0.}, _2) )
# 440 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_exprs) in
    Obj.repr(
# 87 "ParserSmt.mly"
                                          ( (*print_endline (poly_toString "" 0 (Mul ($2, $3))); flush stdout;*) Mul (_2, _3) )
# 448 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 88 "ParserSmt.mly"
                                         ( Div (_2, _3) )
# 456 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "ParserSmt.mly"
                                     ( Pow (_2, int_of_string _3) )
# 464 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "ParserSmt.mly"
                                 ( (*print_endline $1; flush stdout;*) Constant {low=float_of_string _1;high=float_of_string _1} )
# 471 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "ParserSmt.mly"
                                 ( (*print_endline $1; flush stdout;*) Var _1 )
# 478 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "ParserSmt.mly"
                                 ( SubVar _1 )
# 485 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    Obj.repr(
# 93 "ParserSmt.mly"
                                     ((*print_endline (poly_toString "" 0 $2); flush stdout;*) _2 )
# 492 "ParserSmt.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 96 "ParserSmt.mly"
                  (_1)
# 499 "ParserSmt.ml"
               : 'smt_poly_exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_exprs) in
    Obj.repr(
# 97 "ParserSmt.mly"
                                 (MultiplePoly (_1, _2))
# 507 "ParserSmt.ml"
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
