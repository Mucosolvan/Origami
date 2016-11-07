test: origami.cmo tests.cmo
	ocamlc -o test origami.cmo tests.cmo

tests.cmo: tests.ml
	ocamlc -c tests.ml

origami.cmo: origami.ml origami.cmi
	ocamlc -c origami.ml

origami.cmi: origami.mli
	ocamlc -c origami.mli

clean:
	rm -f *.cmi *.cmo ./test
