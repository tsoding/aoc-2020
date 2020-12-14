INPUTS=sample.txt input.txt

test: Main.exe $(INPUTS)
	./Main.exe $(INPUTS)

Main.exe: Main.cs
	mcs Main.cs
