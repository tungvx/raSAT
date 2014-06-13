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
let _ = parse_error;;
# 1 "SmtParser.mly"

open Parsing
open Exp
# 32 "SmtParser.ml"
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
\004\000\004\000\004\000\004\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\002\000\003\000\001\000\003\000\003\000\003\000\002\000\
\002\000\003\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\001\000\003\000\003\000\002\000\003\000\
\003\000\003\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\031\000\000\000\000\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\004\000\001\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\027\000\
\028\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\007\000\017\000\020\000\
\000\000\009\000\008\000\000\000\000\000\000\000\005\000\000\000\
\000\000\018\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\013\000\014\000\015\000\016\000\010\000\030\000\
\021\000\022\000\024\000\025\000\026\000"

let yydgoto = "\002\000\
\005\000\006\000\023\000\020\000\056\000\051\000"

let yysindex = "\013\000\
\003\255\000\000\003\255\052\255\000\000\017\000\014\255\000\000\
\052\255\008\255\113\255\052\255\113\255\151\255\151\255\151\255\
\151\255\151\255\000\000\000\000\000\000\000\000\015\255\017\255\
\083\255\008\255\094\255\113\255\113\255\052\255\000\000\000\000\
\000\000\000\000\151\255\151\255\151\255\151\255\151\255\151\255\
\151\255\151\255\151\255\151\255\151\255\000\000\000\000\000\000\
\083\255\000\000\000\000\005\255\083\255\094\255\000\000\008\255\
\017\255\000\000\000\000\029\255\151\255\161\255\151\255\151\255\
\034\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\000\000\130\255\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\038\000\009\000\029\000\246\255\244\255"

let yytablesize = 288
let yytable = "\027\000\
\023\000\041\000\042\000\043\000\044\000\045\000\003\000\025\000\
\026\000\071\000\025\000\026\000\019\000\001\000\004\000\052\000\
\021\000\020\000\022\000\046\000\030\000\047\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\072\000\077\000\055\000\060\000\024\000\059\000\029\000\
\007\000\031\000\000\000\052\000\000\000\000\000\000\000\000\000\
\073\000\074\000\075\000\076\000\000\000\050\000\008\000\009\000\
\057\000\058\000\000\000\000\000\000\000\000\000\010\000\000\000\
\011\000\012\000\000\000\013\000\014\000\015\000\016\000\017\000\
\018\000\000\000\000\000\000\000\000\000\057\000\000\000\000\000\
\000\000\050\000\024\000\032\000\033\000\048\000\049\000\000\000\
\036\000\037\000\038\000\039\000\040\000\000\000\000\000\011\000\
\053\000\054\000\013\000\014\000\015\000\016\000\017\000\018\000\
\010\000\000\000\011\000\012\000\000\000\013\000\014\000\015\000\
\016\000\017\000\018\000\008\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\013\000\014\000\015\000\016\000\017\000\018\000\011\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\011\000\011\000\
\000\000\011\000\011\000\011\000\011\000\011\000\011\000\032\000\
\033\000\034\000\035\000\000\000\036\000\037\000\038\000\039\000\
\040\000\032\000\033\000\034\000\035\000\000\000\000\000\000\000\
\038\000\039\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\023\000\023\000\023\000\
\000\000\000\000\000\000\023\000\000\000\023\000\023\000\000\000\
\023\000\023\000\023\000\023\000\023\000\023\000\020\000\000\000\
\000\000\000\000\000\000\000\000\020\000\000\000\000\000\020\000"

let yycheck = "\010\000\
\000\000\014\000\015\000\016\000\017\000\018\000\004\001\003\001\
\004\001\005\001\003\001\004\001\004\000\001\000\012\001\026\000\
\000\000\000\000\005\001\005\001\012\000\005\001\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\005\001\001\001\027\000\049\000\009\000\030\000\011\000\
\003\000\013\000\255\255\054\000\255\255\255\255\255\255\255\255\
\061\000\062\000\063\000\064\000\255\255\025\000\003\001\004\001\
\028\000\029\000\255\255\255\255\255\255\255\255\011\001\255\255\
\013\001\014\001\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\255\255\255\255\255\255\049\000\255\255\255\255\
\255\255\053\000\054\000\001\001\002\001\003\001\004\001\255\255\
\006\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\003\001\004\001\016\001\017\001\018\001\019\001\020\001\021\001\
\011\001\255\255\013\001\014\001\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\003\001\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\005\001\255\255\
\255\255\255\255\255\255\255\255\011\001\255\255\013\001\014\001\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\001\001\
\002\001\003\001\004\001\255\255\006\001\007\001\008\001\009\001\
\010\001\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\008\001\009\001\010\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\006\001\007\001\
\255\255\255\255\255\255\011\001\255\255\013\001\014\001\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\005\001\255\255\
\255\255\255\255\255\255\255\255\011\001\255\255\255\255\014\001"

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
# 43 "SmtParser.mly"
                  ( _1 )
# 227 "SmtParser.ml"
               : Exp.ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 46 "SmtParser.mly"
                                 ( _2 )
# 234 "SmtParser.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fass_expr) in
    Obj.repr(
# 47 "SmtParser.mly"
                                 ( _2 )
# 241 "SmtParser.ml"
               : 'fass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 50 "SmtParser.mly"
                                     ( Ch _1 )
# 248 "SmtParser.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 51 "SmtParser.mly"
                                 ( As (_2, _3) )
# 256 "SmtParser.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ass_expr) in
    Obj.repr(
# 52 "SmtParser.mly"
                            ( Conj (_2, _3))
# 264 "SmtParser.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ass_expr) in
    Obj.repr(
# 53 "SmtParser.mly"
                                 ( _2 )
# 271 "SmtParser.ml"
               : 'ass_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 56 "SmtParser.mly"
                                     ( PEq (_1, _2) )
# 279 "SmtParser.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 57 "SmtParser.mly"
                                     ( BEq (_1, _2) )
# 287 "SmtParser.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    Obj.repr(
# 58 "SmtParser.mly"
                                 ( _2 )
# 294 "SmtParser.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'let_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'let_expr) in
    Obj.repr(
# 59 "SmtParser.mly"
                                 ( Let (_1, _2) )
# 302 "SmtParser.ml"
               : 'let_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 62 "SmtParser.mly"
                                         ( Eq  (_2, _3) )
# 310 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 63 "SmtParser.mly"
                                         ( Geq (_2, _3) )
# 318 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 64 "SmtParser.mly"
                                         ( Leq (_2, _3) )
# 326 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 65 "SmtParser.mly"
                                         ( Gr  (_2, _3) )
# 334 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 66 "SmtParser.mly"
                                         ( Le  (_2, _3) )
# 342 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    Obj.repr(
# 67 "SmtParser.mly"
                                     ( _2 )
# 349 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 68 "SmtParser.mly"
                                         ( And (_2, _3) )
# 357 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_bool_expr) in
    Obj.repr(
# 69 "SmtParser.mly"
                                     ( Not _2 )
# 364 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "SmtParser.mly"
                                 ( BVar _1 )
# 371 "SmtParser.ml"
               : 'smt_bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 73 "SmtParser.mly"
                                         ( Add (_2, _3) )
# 379 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 74 "SmtParser.mly"
                                         ( Sub (_2, _3) )
# 387 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 75 "SmtParser.mly"
                                     ( Sub (Real (0.0), _2) )
# 394 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 76 "SmtParser.mly"
                                         ( Mul (_2, _3) )
# 402 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'smt_poly_expr) in
    Obj.repr(
# 77 "SmtParser.mly"
                                         ( Div (_2, _3) )
# 410 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "SmtParser.mly"
                                     ( Pow (_2, int_of_string _3) )
# 418 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "SmtParser.mly"
                                 ( Real (float_of_string _1) )
# 425 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "SmtParser.mly"
                                 ( Var _1 )
# 432 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "SmtParser.mly"
                                 ( SubVar _1 )
# 439 "SmtParser.ml"
               : 'smt_poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'smt_poly_expr) in
    Obj.repr(
# 82 "SmtParser.mly"
                                     ( _2 )
# 446 "SmtParser.ml"
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
