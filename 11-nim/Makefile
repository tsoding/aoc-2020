INPUTS=sample.txt input.txt

.PHONY: test
test: main $(INPUTS)
	./main $(INPUTS)

main: main.nim
	nim c main.nim
