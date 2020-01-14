Symbolic Executor
=================


Installation
------------

```bash
apt-get install opam otags ocamlbuild
opam create switch 4.09.0
opam import import opam.4.09.0.export
```

Package used: `ocamlfind`, `utop`, `z3`, `merlin`, `ansiterminal`.



Trouble Shooting
----------------

##### `dllz3ml.so`: undefined symbol: `Z3_rcf_del`
```
Error: Error on dynamically loaded library: /home/einar/.opam/4.09.0/lib/z3/dllz3ml.so: /home/einar/.opam/4.09.0/lib/z3/dllz3ml.so: undefined symbol: Z3_rcf_del
```
See [OCaml Linking Problems #2355](https://github.com/Z3Prover/z3/issues/2355)

Downgrade `z3` from newest 4.8.6 to 4.7.1 and add a pin.

```bash
opam install z3 4.7.1
opam pin add z3 4.7.1
```


##### `make` error `libz3.so: cannot open shared object file`
```
Error: Error on dynamically loaded library: /home/einar/.opam/4.09.0/lib/z3/dllz3ml.so: libz3.so: cannot open shared object file: No such file or directory
```
4.09.0  ocaml-base-compiler.4.09.0 
`LD_LIBRARY_PATH=$(opam config var z3:lib) make`
`LD_LIBRARY_PATH=/home/einar/.opam/4.09.0/lib/z3/ make`


##### `opam` uses all system ressources and/or crashes
If `addvs` and/or `gringo` seems to be hogging all resources when issuing `opam`, try add `--solver=mccs`.
