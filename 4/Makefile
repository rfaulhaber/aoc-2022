##
# AOC Day 4
#
# @file
# @version 0.1

a.out: ../utils/c/*.c main.c main.h
	clang -I ../utils/c ../utils/c/input.c main.c

run: a.out
	./a.out $(file)

debug: main.c main.h
	clang -L ..utils/c -g -c main.c -o debug.o

# end
