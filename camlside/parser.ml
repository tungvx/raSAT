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
  | IC
  | IN
  | EQ
  | GEQ
  | LEQ
  | GR
  | LE

open Parsing;;
# 1 "parser.mly"

open Parsing
open Ast
# 28 "parser.ml"
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
  268 (* IC *);
  269 (* IN *);
  270 (* EQ *);
  271 (* GEQ *);
  272 (* LEQ *);
  273 (* GR *);
  274 (* LE *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\004\000\004\000\006\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\003\000\001\000\003\000\003\000\004\000\003\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\003\000\020\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\001\000\000\000\004\000\022\000\
\025\000\000\000\000\000\000\000\017\000\018\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\000\000\021\000\023\000\010\000\011\000\012\000\
\000\000\000\000\000\000\000\000\000\000\005\000\006\000\007\000\
\008\000\009\000\019\000\013\000\014\000\015\000\016\000"

let yydgoto = "\002\000\
\008\000\009\000\024\000\010\000\044\000\011\000"

let yysindex = "\003\000\
\015\255\000\000\249\254\015\255\032\255\049\255\042\255\000\000\
\007\000\000\000\000\000\009\255\008\255\021\255\033\255\032\255\
\032\255\032\255\056\255\056\255\056\255\056\255\056\255\000\000\
\049\255\049\255\042\255\042\255\000\000\012\255\000\000\000\000\
\000\000\037\255\032\255\032\255\000\000\000\000\056\255\056\255\
\056\255\056\255\056\255\035\255\054\255\055\255\064\255\065\255\
\033\255\000\000\021\255\000\000\000\000\000\000\000\000\000\000\
\063\255\056\255\056\255\056\255\067\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\065\000\254\255\001\000\236\255\005\000"

let yytablesize = 69
let yytable = "\045\000\
\046\000\047\000\048\000\001\000\014\000\012\000\029\000\028\000\
\015\000\030\000\026\000\031\000\053\000\034\000\035\000\036\000\
\003\000\004\000\057\000\058\000\059\000\060\000\061\000\005\000\
\032\000\006\000\007\000\051\000\052\000\049\000\050\000\015\000\
\055\000\056\000\016\000\062\000\033\000\068\000\069\000\070\000\
\054\000\017\000\018\000\003\000\027\000\019\000\020\000\021\000\
\022\000\023\000\003\000\025\000\006\000\007\000\063\000\064\000\
\037\000\038\000\039\000\006\000\040\000\041\000\042\000\043\000\
\065\000\066\000\067\000\071\000\013\000"

let yycheck = "\020\000\
\021\000\022\000\023\000\001\000\004\000\013\001\000\000\007\000\
\004\000\001\001\006\000\004\001\001\001\016\000\017\000\018\000\
\002\001\003\001\039\000\040\000\041\000\042\000\043\000\009\001\
\004\001\011\001\012\001\027\000\028\000\025\000\026\000\027\000\
\035\000\036\000\003\001\001\001\004\001\058\000\059\000\060\000\
\004\001\010\001\011\001\002\001\003\001\014\001\015\001\016\001\
\017\001\018\001\002\001\003\001\011\001\012\001\001\001\001\001\
\001\001\002\001\003\001\011\001\005\001\006\001\007\001\008\001\
\001\001\001\001\004\001\001\001\004\000"

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
  IC\000\
  IN\000\
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
# 44 "parser.mly"
               ( _1 )
# 160 "parser.ml"
               : Ast.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 47 "parser.mly"
                                 ( Ass _2 )
# 167 "parser.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'intv_expr) in
    Obj.repr(
# 48 "parser.mly"
                                 ( Intv _1 )
# 174 "parser.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'f_expr) in
    Obj.repr(
# 49 "parser.mly"
                                 ( _2 )
# 181 "parser.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Eq  _2 
                                   else Eq (Sub(_2, Real number))
                                 )
# 193 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Geq  _2 
                                   else Geq (Sub(_2, Real number))
                                 )
# 205 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Leq  _2 
                                   else Leq (Sub(_2, Real number))
                                 )
# 217 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Gr  _2 
                                   else Gr (Sub(_2, Real number))
                                 )
# 229 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Le  _2 
                                   else Le (Sub(_2, Real number))
                                 )
# 241 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    Obj.repr(
# 77 "parser.mly"
                                 ( _2 )
# 248 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 78 "parser.mly"
                                 ( And (_2, _3) )
# 256 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 79 "parser.mly"
                                 ( BOr (_2, _3) )
# 264 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 82 "parser.mly"
                                 ( Add (_2, _3) )
# 272 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 83 "parser.mly"
                                 ( Sub (_2, _3) )
# 280 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 84 "parser.mly"
                                 ( Mul (_2, _3) )
# 288 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                                 ( Pow (_2, int_of_string _3) )
# 296 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                                 ( Real (float_of_string _1) )
# 303 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                                 ( Var _1 )
# 310 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    Obj.repr(
# 88 "parser.mly"
                                 ( _2 )
# 317 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'intv_clause) in
    Obj.repr(
# 91 "parser.mly"
                                 ( Cl _1 )
# 324 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intv_expr) in
    Obj.repr(
# 92 "parser.mly"
                                 ( Ic (_2, _3) )
# 332 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_expr) in
    Obj.repr(
# 93 "parser.mly"
                                 ( _2 )
# 339 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "parser.mly"
                   ( In (_1, float_of_string _3, float_of_string _4))
# 348 "parser.ml"
               : 'intv_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intv_clause) in
    Obj.repr(
# 98 "parser.mly"
                                 ( Or (_2, _3) )
# 356 "parser.ml"
               : 'intv_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_clause) in
    Obj.repr(
# 99 "parser.mly"
                                 ( _2 )
# 363 "parser.ml"
               : 'intv_clause))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.formula)
