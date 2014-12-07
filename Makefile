.SUFFIXES: .v .vo

all: kodellama

kodellama: Ident.vo Exp.vo Aexp.vo Typ.vo Sigma.vo Commands.vo Eval.vo BinStringToQ.vo

.v.vo:
	coqc $<

clean:	cleanv cleano

cleanv:
	rm -rf *.vo

cleano:
	rm -rf ocaml/

ocaml:
	coqtop < Extract.v
	mkdir ocaml
	mv *.ml ocaml/
	mv *.mli ocaml/
