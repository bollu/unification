unify: unify.cpp parser.h
	clang++ -g -std=c++17 -Wall -Werror -fsanitize=address -fsanitize=undefined unify.cpp -ounify
