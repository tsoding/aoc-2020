INPUTS=sample.txt input.txt

.PHONY: test
test: main $(INPUTS)
	./main $(INPUTS)

main: main.o
	ld -o main main.o

main.o: main.asm
	nasm -felf64 main.asm
