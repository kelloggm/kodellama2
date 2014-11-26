.SUFFIXES: .v .vo

all: kodellama

kodellama: Ident.vo Aexp.vo Bexp.vo Typ.vo Sigma.vo Eval.vo

.v.vo:
	coqc $<

clean:
	rm -rf *.vo