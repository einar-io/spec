default: _tags *.ml
	export LD_LIBRARY_PATH=`opam config var z3:lib`; ocamlbuild -lflag -thread -use-ocamlfind run.byte
	./run.byte

interpreter:
	utop -I +batteries engine.ml

.PHONY: z3
z3:
	utop -require "z3"

.PHONY: z3-examples
z3-examples: Z3ml-examples/*.ml
	$(MAKE) --directory Z3ml-examples


tags: *.ml
	otags -r -vi


.PHONY: interfaces
interfaces: run.ml
	corebuild run.inferred.mli

.PHONY: clean
clean:
	ocamlbuild -clean


demorgan: demorgan.ml
	ocamlfind ocamlc -o demorgan.byte -package z3 -linkpkg demorgan.ml
