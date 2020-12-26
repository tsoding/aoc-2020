.PHONY: test
test: main
	./main

main: main.o
	ld -o main main.o

main.o: main.asm
	nasm -felf64 main.asm
