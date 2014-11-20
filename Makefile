.SUFFIXES: .v .vo

all: kodellama

kodellama: Aexp.vo Typ.vo Sigma.vo

.v.vo:
	coqc $<

clean:
	rm -rf *.vo