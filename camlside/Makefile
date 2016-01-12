#------------------------------------------------------------------------------

OCAMLDIR:= "$(shell ocamlc -where)"

#CFLAGS=-I $(OCAMLDIR)/caml
#------------------------------------------------------------------------------

SOURCES = ast.ml smtlib_util.ml smtlib_syntax.ml smtlib_parse.mly smtlib_lex.mll str.cma main.ml

RESULT = smt22hys

LDFLAGS = -static

# LFLAGS = -cclib -static

include OCamlMakefile
