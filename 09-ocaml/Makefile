INPUTS=input.txt

.PHONY: test
test: main.native main.byte $(INPUTS)
	./main.native $(INPUTS)

main.native: main.ml
	ocamlopt -o main.native main.ml

main.byte: main.ml
	ocamlc -o main.byte main.ml
