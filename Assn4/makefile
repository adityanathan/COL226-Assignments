all:
	ocamlc -c a1.ml
	ocamllex a2.mll       # generates lexer.ml
	ocamlyacc a3.mly     # generates parser.ml and parser.mli
	ocamlc -c a3.mli
	ocamlc -c a2.ml
	ocamlc -c a3.ml
	ocamlc -c evaluator.ml
	ocamlc -o calculate a1.cmo a2.cmo a3.cmo evaluator.cmo

clean:
	rm calculate *.cmo *.cmi *.mli lexer.ml parser.ml
