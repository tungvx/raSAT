#------------------------------------------------------------------------------

OCAMLDIR:= "$(shell ocamlc -where)"

OCAML_LIB=camllib.a

CC=g++
OC=ocamlopt
#CFLAGS=-I $(OCAMLDIR)/caml
#------------------------------------------------------------------------------

SOURCES = multiset.ml fpu.ml fpu_rename.ml fpu_rename_all.ml interval.ml InfiniteList.ml IA.ml Variable.ml Assignments.ml Util.ml ast.ml Expr.ml PolynomialConstraint.ml icp.ml Testing.ml smtlib_util.ml smtlib_syntax.ml smtlib_parse.mly smtlib_lex.mll str.cma Decomposer.ml EqualitiesHandler.ml UnsatCore.ml Caml.ml #example.ml

RESULT = a

all: ncl $(OCAML_LIB)

include OCamlMakefile

#------------------------------------------------------------------------------

modcaml.o: multiset.cmx InfiniteList.cmx Variable.cmx Util.cmx ast.cmx Expr.cmx PolynomialConstraint.cmx icp.cmx Testing.cmx Exp.cmx Expr.cmx IA.cmx Caml.cmx Assignments.cmx smtlib_util.cmx smtlib_syntax.cmx smtlib_parse.cmx smtlib_lex.cmx Decomposer.cmx EqualitiesHandler.cmx UnsatCore.cmx fpu.cmx fpu_rename.cmx fpu_rename_all.cmx interval.cmx

	$(OC) -output-obj -o modcaml.o multiset.cmx fpu.cmx fpu_rename.cmx fpu_rename_all.cmx interval.cmx InfiniteList.cmx IA.cmx Variable.cmx Assignments.cmx Util.cmx ast.cmx Expr.cmx PolynomialConstraint.cmx icp.cmx Testing.cmx smtlib_util.cmx smtlib_syntax.cmx smtlib_parse.cmx smtlib_lex.cmx Decomposer.cmx EqualitiesHandler.cmx UnsatCore.cmx Caml.cmx

$(OCAML_LIB): modcaml.o chcw.o
	rm -f $(OCAML_LIB)
	cp $(OCAMLDIR)/libasmrun.a $(OCAML_LIB)

	ar r $(OCAML_LIB) chcw.o modcaml.o
	#ranlib $@
	#rm -f $@
	#ar rc $@  chcw.o
	#ranlib $@

chcw.o:
	gcc -W -Wall -O5 -I $(OCAMLDIR)/caml -c chcw.c

c:
	rm -f $(OCAML_LIB) modcaml.o chcw.o
