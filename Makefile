
OCAMLC  = ocamlopt

.SUFFIXES: .v .vo

all: coq

rebuild: clean coq ocaml usable

coq: Ident.vo Exp.vo Aexp.vo Typ.vo Sigma.vo Commands.vo Evals.vo BinStringToQ.vo

.v.vo:
	coqc $<

clean:	cleanv cleano

cleanv:
	rm -rf *.vo

cleano:
	rm -rf ocaml/

ocaml:
	coqtop < Extract.v
	ocamllex lex.mll
	ocamlyacc parse.mly
	mkdir ocaml
	mv *.ml ocaml/
	cp ocaml/main.ml .
	cp ocaml/CoreString.ml .
	cp ocaml/Printer.ml .
	mv *.mli ocaml/
	mv ocaml/String.ml ocaml/CoqString.ml
	mv ocaml/String.mli ocaml/CoqString.mli
	python PostProcessor.py

usable:
	cd ocaml ;\
	$(OCAMLC) -o Kodellama CoreString.ml Datatypes.mli Datatypes.ml Specif.mli Specif.ml Bool.mli Bool.ml BinNums.mli BinNums.ml Ring_theory.mli Ring_theory.ml Sumbool.mli Sumbool.ml Peano.mli Peano.ml BinPosDef.mli BinPosDef.ml BinPos.mli BinPos.ml BinNat.mli BinNat.ml BinInt.mli BinInt.ml ZArith_dec.mli ZArith_dec.ml Zeven.mli Zeven.ml Zbool.mli Zbool.ml QArith_base.mli QArith_base.ml EqNat.mli EqNat.ml Ascii.mli Ascii.ml CoqString.mli CoqString.ml Ident.mli Ident.ml Exp.mli Exp.ml Aexp.mli Aexp.ml Typ.mli Typ.ml Sigma.mli Sigma.ml Commands.mli Commands.ml Printer.ml Evals.mli Evals.ml BinStringToQ.mli BinStringToQ.ml parse.mli parse.ml lex.ml main.ml
