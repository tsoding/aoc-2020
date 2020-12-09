INPUTS=sample-01.txt sample-02.txt input.txt

all: test $(PNGS)

.PHONY: test
test: main $(INPUTS)
	./main $(INPUTS)

main: main.go
	go build -o main main.go
