
default: _tags *.ml
	ocamlbuild -use-ocamlfind run.byte
	./run.byte

interpreter:
	utop -I +batteries engine.ml
