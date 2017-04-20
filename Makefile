.PHONY: test
test:
	ocamlopt -g -annot shim.ml \
	  parsing.ml test_parsing.ml \
          docopt.ml test_docopt.ml \
	  -o test && ./test

.PHONY: clean
clean:
	cat .gitignore | tr '\n' '\0' | xargs -n1 -0 echo rm -f


