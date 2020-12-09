.PHONY: test
test: Main sample.txt input.txt
	./Main sample.txt
	./Main input.txt

Main: Main.hs
	ghc --make Main.hs
