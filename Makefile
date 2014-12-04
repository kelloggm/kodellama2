.SUFFIXES: .v .vo

all: kodellama

kodellama: Ident.vo Aexp.vo Bexp.vo Exp.vo Typ.vo Sigma.vo Commands.vo Eval.vo

.v.vo:
	coqc $<

clean:
	rm -rf *.vo
	rm -rf ocaml/

ocaml:
	coqtop < Extract.v
	mkdir ocaml
	mv *.ml ocaml/
	mv *.mli ocaml/
