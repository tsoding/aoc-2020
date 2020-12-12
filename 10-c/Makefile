CFLAGS=-Wall -Wextra -std=c11 -pedantic
INPUTS=sample-01.txt sample-02.txt input.txt

.PHONY: test
test: main $(INPUTS)
	./main $(INPUTS)

main: main.c
	$(CC) $(CFLAGS) -o main main.c
