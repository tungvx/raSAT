#------------------------------------------------------------------------------

OCAML_LIB=camllib.a

CC=g++
OC=ocamlopt

#------------------------------------------------------------------------------

SOURCES = InfiniteList.ml IA.ml Util.ml Assignments.ml Variable.ml ast.ml Testing.ml parser.mly lexer.mll Exp.ml SmtParser.mly SmtLexer.mll Expr.ml str.cma Decomposer.ml Equalities_handler.ml Unsat_Core.ml Caml.ml Caml_Smt.ml

RESULT = a

all: nc $(OCAML_LIB)

include OCamlMakefile

#------------------------------------------------------------------------------

modcaml.o: InfiniteList.cmx Variable.cmx ast.cmx Testing.cmx parser.cmx lexer.cmx Exp.cmx SmtParser.cmx SmtLexer.cmx Expr.cmx IA.cmx Util.cmx Caml.cmx Caml_Smt.cmx Assignments.cmx Decomposer.cmx Equalities_handler.cmx Unsat_Core.cmx

	$(OC) -output-obj -o modcaml.o InfiniteList.cmx IA.cmx Util.cmx Assignments.cmx Variable.cmx ast.cmx Testing.cmx parser.cmx lexer.cmx Exp.cmx SmtParser.cmx SmtLexer.cmx Expr.cmx Decomposer.cmx Equalities_handler.cmx Unsat_Core.cmx Caml.cmx Caml_Smt.cmx

$(OCAML_LIB): modcaml.o
	@rm -f $(OCAML_LIB)
	cp `ocamlc -where`/libasmrun.a $(OCAML_LIB)

	ar r $(OCAML_LIB) modcaml.o

c:
	rm -f $(OCAML_LIB) modcaml.o
