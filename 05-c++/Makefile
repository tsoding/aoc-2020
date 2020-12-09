CXXFLAGS=-Wall -Wextra -std=c++17 -pedantic
INPUTS=sample.txt input.txt

.PHONY: test
test: main $(INPUTS)
	./main $(INPUTS)

main: main.cpp
	$(CXX) $(CXXFLAGS) -o main main.cpp
