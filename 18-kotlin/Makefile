INPUTS=sample.txt input.txt

.PHONY: test
test: main.jar $(INPUTS)
	java -jar main.jar $(INPUTS)

main.jar: main.kt
	kotlinc main.kt -include-runtime -d main.jar
