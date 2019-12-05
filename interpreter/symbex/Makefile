
default: _tags *.ml
	ocamlbuild -use-ocamlfind run.byte
	./run.byte

interpreter:
	utop -I +batteries engine.ml

.PHONY: z3
z3:
	utop -require "z3"

tags: *.ml
	otags -r -vi

demorgan: demorgan.ml
	ocamlfind ocamlc -o demorgan.byte -package z3 -linkpkg demorgan.ml

.PHONY: interfaces
interfaces: run.ml
	corebuild run.inferred.mli
