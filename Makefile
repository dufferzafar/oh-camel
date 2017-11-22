default: debug

debug:
	cp assignments/assign-5-resol.ml resol.ml
	ocamlc -g resol.ml
	ocamldebug a.out
