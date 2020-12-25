INPUTS=input.txt

.PHONY: test
test: day24 $(INPUTS)
	./day24 $(INPUTS)

day24: day24.adb
	gnatmake day24.adb
