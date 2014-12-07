.SUFFIXES: .v .vo

all: kodellama

kodellama: Ident.vo Exp.vo Aexp.vo Typ.vo Sigma.vo Commands.vo Evals.vo BinStringToQ.vo

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
	cp ocaml/BatString.ml .
	cp ocaml/main.ml .
	mv *.mli ocaml/
