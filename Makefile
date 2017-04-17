.PHONY: test
test:
	ocamlopt -annot shim.ml parsing.ml test_parsing.ml -o test && ./test

.PHONY: clean
clean:
	rm -f `cat .gitignore`


