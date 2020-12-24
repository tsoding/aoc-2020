INPUTS=input.txt

.PHONY: test
test: main.exe $(INPUTS)
	./main.exe $(INPUTS)

main.exe: main.dart
	dart compile exe main.dart
