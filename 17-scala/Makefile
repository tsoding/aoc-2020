INPUTS=sample.txt input.txt

.PHONY: test $(INPUTS)
test: Main.class $(INPUTS)
	java -cp "$(SCALA_HOME)/lib/scala-library.jar:." Main $(INPUTS)

Main.class: Main.scala
	scalac Main.scala
