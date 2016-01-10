#------------------------------------------------------------------------------

OCAMLDIR:= "$(shell ocamlc -where)"

OCAML_LIB=camllib.a

CC=gcc
OC=ocamlopt
#CFLAGS=-I $(OCAMLDIR)/caml
#------------------------------------------------------------------------------

SOURCES = ast.ml smtlib_util.ml smtlib_syntax.ml smtlib_parse.mly smtlib_lex.mll str.cma Caml.ml #example.ml

RESULT = a

all: ncl $(OCAML_LIB)

include OCamlMakefile

#------------------------------------------------------------------------------

modcaml.o: ast.cmx Caml.cmx smtlib_util.cmx smtlib_syntax.cmx smtlib_parse.cmx smtlib_lex.cmx

	$(OC) -output-obj -o modcaml.o ast.cmx smtlib_util.cmx smtlib_syntax.cmx smtlib_parse.cmx smtlib_lex.cmx Caml.cmx

$(OCAML_LIB): modcaml.o
	rm -f $(OCAML_LIB)
	cp $(OCAMLDIR)/libasmrun.a $(OCAML_LIB)

	ar r $(OCAML_LIB) modcaml.o
	#ranlib $@
	#rm -f $@
	#ar rc $@  chcw.o
	#ranlib $@

c:
	rm -f $(OCAML_LIB) modcaml.o chcw.o
