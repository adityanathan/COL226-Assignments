all:
	ocamlc -c expression.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c evaluator.ml
	ocamlc -o calculate expression.cmo lexer.cmo parser.cmo evaluator.cmo

clean:
	rm calculate *.cmo *.cmi *.mli lexer.ml parser.ml
