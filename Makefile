.SUFFIXES: .v .vo

all: kodellama

kodellama: Ident.vo Aexp.vo Bexp.vo Exp.vo Typ.vo Sigma.vo Commands.vo Eval.vo

.v.vo:
	coqc $<

clean:
	rm -rf *.vo
