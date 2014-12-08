#------------------------------------------------------------------------------

OCAML_LIB=camllib.a

CC=g++
OC=ocamlopt

#------------------------------------------------------------------------------

SOURCES = InfiniteList.ml IA.ml Variable.ml Assignments.ml Util.ml ast.ml PolynomialConstraint.ml Testing.ml ParserConstraints.mly LexerConstraints.mll Exp.ml ParserSmt.mly LexerSmt.mll ParserIntervals.mly LexerIntervals.mll Expr.ml str.cma Decomposer.ml EqualitiesHandler.ml UnsatCore.ml Caml.ml CamlSmt.ml #fpu.ml fpu_rename.ml fpu_rename_all.ml interval.ml

RESULT = a

all: nc $(OCAML_LIB)

include OCamlMakefile

#------------------------------------------------------------------------------

modcaml.o: InfiniteList.cmx Variable.cmx Util.cmx ast.cmx PolynomialConstraint.cmx Testing.cmx ParserConstraints.cmx LexerConstraints.cmx Exp.cmx ParserSmt.cmx  LexerSmt.cmx ParserIntervals.cmx LexerIntervals.cmx Expr.cmx IA.cmx Caml.cmx CamlSmt.cmx Assignments.cmx Decomposer.cmx EqualitiesHandler.cmx UnsatCore.cmx #fpu.cmx fpu_rename.cmx fpu_rename_all.cmx interval.cmx

	$(OC) -output-obj -o modcaml.o InfiniteList.cmx IA.cmx Variable.cmx Assignments.cmx Util.cmx ast.cmx PolynomialConstraint.cmx Testing.cmx ParserConstraints.cmx LexerConstraints.cmx Exp.cmx ParserSmt.cmx LexerSmt.cmx ParserIntervals.cmx LexerIntervals.cmx Expr.cmx Decomposer.cmx EqualitiesHandler.cmx UnsatCore.cmx Caml.cmx CamlSmt.cmx #fpu.cmx fpu_rename.cmx fpu_rename_all.cmx interval.cmx

$(OCAML_LIB): modcaml.o
	@rm -f $(OCAML_LIB)
	cp `ocamlc -where`/libasmrun.a $(OCAML_LIB)

	ar r $(OCAML_LIB) modcaml.o

c:
	rm -f $(OCAML_LIB) modcaml.o
