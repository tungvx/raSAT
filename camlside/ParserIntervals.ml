type token =
  | IC
  | IN
  | ID of (string)
  | NUM of (string)
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "ParserIntervals.mly"
open Parsing
open IA
# 16 "ParserIntervals.ml"
let yytransl_const = [|
  257 (* IC *);
  258 (* IN *);
  261 (* LPAREN *);
  262 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  259 (* ID *);
  260 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\004\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\001\000\003\000\000\000\004\000\002\000"

let yydgoto = "\002\000\
\006\000\007\000"

let yysindex = "\002\000\
\003\255\000\000\003\255\255\254\003\255\000\000\007\000\003\255\
\005\255\004\255\000\000\000\000\007\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255"

let yytablesize = 11
let yytable = "\008\000\
\009\000\010\000\001\000\003\000\012\000\004\000\011\000\005\000\
\013\000\014\000\015\000"

let yycheck = "\003\000\
\002\001\005\000\001\000\001\001\008\000\003\001\000\000\005\001\
\004\001\006\001\004\001"

let yynames_const = "\
  IC\000\
  IN\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'intervals) in
    Obj.repr(
# 22 "ParserIntervals.mly"
                  ( _1 )
# 83 "ParserIntervals.ml"
               : (string * IA.interval) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 25 "ParserIntervals.mly"
                   ( [(_1, new IA.interval (float_of_string _3) (float_of_string _4))])
# 92 "ParserIntervals.ml"
               : 'intervals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intervals) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'intervals) in
    Obj.repr(
# 26 "ParserIntervals.mly"
                                 ( List.append _2 _3 )
# 100 "ParserIntervals.ml"
               : 'intervals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'intervals) in
    Obj.repr(
# 27 "ParserIntervals.mly"
                                 ( _2 )
# 107 "ParserIntervals.ml"
               : 'intervals))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string * IA.interval) list)
