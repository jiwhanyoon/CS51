all: expression expression_tests mapfold mapfold_tests 

mapfold: mapfold.ml
	ocamlbuild mapfold.byte

mapfold_tests: mapfold_tests.ml
	ocamlbuild mapfold_tests.byte

expression: expression.ml
	ocamlbuild expression.byte

expression_tests: expression_tests.ml
	ocamlbuild expression_tests.byte

clean: 
	rm -rf _build *.byte