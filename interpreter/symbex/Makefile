
default: _tags *.ml
	ocamlbuild -use-ocamlfind test.byte
	./test.byte

interpreter:
	utop -I +batteries engine.ml
