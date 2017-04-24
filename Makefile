.PHONY: test
test:
	ocamlc -g -annot \
	  shim.ml \
	  parsing_framework.ml test_parsing_framework.ml \
	  syntax.ml \
	  parser.ml test_parser.ml \
	  docopt.ml test_docopt.ml \
	  -o test && ./test

.PHONY: clean
clean:
	rm -f `cat .gitignore`


