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
\003\000\003\000\003\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\004\000\006\000\006\000\006\000\
\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\001\000\003\000\001\000\003\000\003\000\004\000\003\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\000\
\000\000\003\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\001\000\000\000\004\000\021\000\024\000\
\000\000\000\000\016\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\020\000\022\000\010\000\011\000\000\000\000\000\000\000\
\000\000\000\000\005\000\006\000\007\000\008\000\009\000\018\000\
\012\000\013\000\014\000\015\000"

let yydgoto = "\002\000\
\008\000\009\000\023\000\010\000\042\000\011\000"

let yysindex = "\007\000\
\002\255\000\000\250\254\002\255\031\255\051\255\040\255\000\000\
\017\000\000\000\000\000\034\255\035\255\036\255\046\255\031\255\
\031\255\058\255\058\255\058\255\058\255\058\255\000\000\051\255\
\051\255\040\255\040\255\000\000\043\255\000\000\000\000\000\000\
\052\255\031\255\000\000\000\000\058\255\058\255\058\255\058\255\
\058\255\058\255\058\255\058\255\058\255\058\255\046\255\000\000\
\036\255\000\000\000\000\000\000\000\000\053\255\058\255\058\255\
\058\255\054\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\054\000\255\255\002\000\237\255\006\000"

let yytablesize = 66
let yytable = "\043\000\
\044\000\045\000\046\000\003\000\004\000\014\000\012\000\001\000\
\027\000\015\000\005\000\025\000\006\000\007\000\033\000\034\000\
\028\000\054\000\055\000\056\000\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\049\000\050\000\047\000\048\000\015\000\
\053\000\016\000\029\000\065\000\066\000\067\000\030\000\031\000\
\017\000\003\000\026\000\051\000\018\000\019\000\020\000\021\000\
\022\000\032\000\006\000\007\000\003\000\024\000\068\000\052\000\
\064\000\013\000\035\000\036\000\037\000\006\000\038\000\039\000\
\040\000\041\000"

let yycheck = "\019\000\
\020\000\021\000\022\000\002\001\003\001\004\000\013\001\001\000\
\007\000\004\000\009\001\006\000\011\001\012\001\016\000\017\000\
\000\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\026\000\027\000\024\000\025\000\026\000\
\034\000\003\001\001\001\055\000\056\000\057\000\004\001\004\001\
\010\001\002\001\003\001\001\001\014\001\015\001\016\001\017\001\
\018\001\004\001\011\001\012\001\002\001\003\001\001\001\004\001\
\004\001\004\000\001\001\002\001\003\001\011\001\005\001\006\001\
\007\001\008\001"

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
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 52 "parser.mly"
                                 ( Eq  (_2, _3) )
# 189 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 53 "parser.mly"
                                 ( Geq (_2, _3) )
# 197 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 54 "parser.mly"
                                 ( Leq (_2, _3) )
# 205 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 55 "parser.mly"
                                 ( Gr  (_2, _3) )
# 213 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 56 "parser.mly"
                                 ( Le  (_2, _3) )
# 221 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    Obj.repr(
# 57 "parser.mly"
                                 ( _2 )
# 228 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 58 "parser.mly"
                                 ( And (_2, _3) )
# 236 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 61 "parser.mly"
                                 ( Add (_2, _3) )
# 244 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 62 "parser.mly"
                                 ( Sub (_2, _3) )
# 252 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 63 "parser.mly"
                                 ( Mul (_2, _3) )
# 260 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                                 ( Pow (_2, int_of_string _3) )
# 268 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                                 ( Real (float_of_string _1) )
# 275 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                                 ( Var _1 )
# 282 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    Obj.repr(
# 67 "parser.mly"
                                 ( _2 )
# 289 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'intv_clause) in
    Obj.repr(
# 70 "parser.mly"
                                 ( Cl _1 )
# 296 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intv_expr) in
    Obj.repr(
# 71 "parser.mly"
                                 ( Ic (_2, _3) )
# 304 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_expr) in
    Obj.repr(
# 72 "parser.mly"
                                 ( _2 )
# 311 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
                                 ( In (_1, float_of_string _3, float_of_string _4) )
# 320 "parser.ml"
               : 'intv_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_clause) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intv_clause) in
    Obj.repr(
# 76 "parser.mly"
                                 ( Or (_2, _3) )
# 328 "parser.ml"
               : 'intv_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_clause) in
    Obj.repr(
# 77 "parser.mly"
                                 ( _2 )
# 335 "parser.ml"
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
