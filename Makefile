default: types

debug:
	cp assignments/assign-5-resol.ml resol.ml
	ocamlc -g resol.ml
	ocamldebug a.out

types:
	@prolog -s assignments/assign-7-types.pl -g main -t halt
