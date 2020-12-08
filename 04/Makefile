.PHONY: test
test: main sample-01.txt sample-02.txt sample-03.txt input.txt
	./main ./sample-01.txt
	./main ./sample-02.txt
	./main ./sample-03.txt
	./main ./input.txt

main: main.d
	dmd main.d
