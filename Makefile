all: kodellama

kodellama: Aexp.vo

Aexp.vo: Aexp.v
	coqc Aexp.v

clean:
	rm -rf *.vo