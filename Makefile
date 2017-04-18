.PHONY: test
test:
	ocamlopt -g -annot shim.ml \
	  parsing.ml test_parsing.ml \
          docopt.ml test_docopt.ml \
	  -o test && ./test

.PHONY: clean
clean:
	rm -f `cat .gitignore`


