SRC = toy.cpp
ALL = $(SRC)
OBJS = $(SRC:.cpp=.o)

toy:
	clang++ toy.cpp -o toy `llvm-config --cxxflags`
