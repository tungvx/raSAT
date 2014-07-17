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
open IA
# 29 "parser.ml"
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
\005\000\005\000\005\000\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\003\000\004\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\000\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\001\000\
\000\000\004\000\022\000\000\000\000\000\000\000\017\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\020\000\010\000\011\000\012\000\
\000\000\000\000\000\000\000\000\000\000\005\000\006\000\007\000\
\008\000\009\000\019\000\013\000\014\000\015\000\016\000"

let yydgoto = "\002\000\
\007\000\008\000\021\000\009\000\038\000"

let yysindex = "\003\000\
\019\255\000\000\249\254\019\255\026\255\036\255\000\000\008\000\
\000\000\011\255\009\255\010\255\026\255\026\255\026\255\044\255\
\044\255\044\255\044\255\044\255\000\000\036\255\036\255\000\000\
\014\255\000\000\000\000\023\255\026\255\026\255\000\000\000\000\
\044\255\044\255\044\255\044\255\044\255\029\255\031\255\052\255\
\053\255\054\255\010\255\000\000\000\000\000\000\000\000\000\000\
\055\255\044\255\044\255\044\255\056\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\052\000\252\255\001\000\239\255"

let yytablesize = 59
let yytable = "\039\000\
\040\000\041\000\042\000\001\000\012\000\010\000\023\000\024\000\
\028\000\029\000\030\000\025\000\026\000\027\000\045\000\049\000\
\050\000\051\000\052\000\053\000\003\000\004\000\043\000\044\000\
\047\000\048\000\046\000\005\000\013\000\054\000\006\000\055\000\
\060\000\061\000\062\000\014\000\015\000\003\000\022\000\016\000\
\017\000\018\000\019\000\020\000\031\000\032\000\033\000\006\000\
\034\000\035\000\036\000\037\000\056\000\057\000\058\000\011\000\
\063\000\000\000\059\000"

let yycheck = "\017\000\
\018\000\019\000\020\000\001\000\004\000\013\001\006\000\000\000\
\013\000\014\000\015\000\001\001\004\001\004\001\001\001\033\000\
\034\000\035\000\036\000\037\000\002\001\003\001\022\000\023\000\
\029\000\030\000\004\001\009\001\003\001\001\001\012\001\001\001\
\050\000\051\000\052\000\010\001\011\001\002\001\003\001\014\001\
\015\001\016\001\017\001\018\001\001\001\002\001\003\001\012\001\
\005\001\006\001\007\001\008\001\001\001\001\001\001\001\004\000\
\001\001\255\255\004\001"

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
# 45 "parser.mly"
               ( _1 )
# 154 "parser.ml"
               : Ast.formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 48 "parser.mly"
                                 ( Ass _2 )
# 161 "parser.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'intv_expr) in
    Obj.repr(
# 49 "parser.mly"
                                 ( Intv _1 )
# 168 "parser.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'f_expr) in
    Obj.repr(
# 50 "parser.mly"
                                 ( _2 )
# 175 "parser.ml"
               : 'f_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Eq  _2 
                                   else Eq (Sub(_2, Real number))
                                 )
# 187 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Geq  _2 
                                   else Geq (Sub(_2, Real number))
                                 )
# 199 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Leq  _2 
                                   else Leq (Sub(_2, Real number))
                                 )
# 211 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 68 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Gr  _2 
                                   else Gr (Sub(_2, Real number))
                                 )
# 223 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                                 ( 
                                   let number = float_of_string _3 in
                                   if number = 0. then Le  _2 
                                   else Le (Sub(_2, Real number))
                                 )
# 235 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    Obj.repr(
# 78 "parser.mly"
                                 ( _2 )
# 242 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 79 "parser.mly"
                                 ( And (_2, _3) )
# 250 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 80 "parser.mly"
                                 ( BOr (_2, _3) )
# 258 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 83 "parser.mly"
                                 ( Add (_2, _3) )
# 266 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 84 "parser.mly"
                                 ( Sub (_2, _3) )
# 274 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'poly_expr) in
    Obj.repr(
# 85 "parser.mly"
                                 ( Mul (_2, _3) )
# 282 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                                 ( Pow (_2, int_of_string _3) )
# 290 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                                 ( Real (float_of_string _1) )
# 297 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                                 ( Var _1 )
# 304 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'poly_expr) in
    Obj.repr(
# 89 "parser.mly"
                                 ( _2 )
# 311 "parser.ml"
               : 'poly_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "parser.mly"
                   ( [(_1, new IA.interval (float_of_string _3) (float_of_string _4))])
# 320 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intv_expr) in
    Obj.repr(
# 93 "parser.mly"
                                 ( List.append _2 _3 )
# 328 "parser.ml"
               : 'intv_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intv_expr) in
    Obj.repr(
# 94 "parser.mly"
                                 ( _2 )
# 335 "parser.ml"
               : 'intv_expr))
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
