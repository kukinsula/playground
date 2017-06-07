EXEC = cli-parser_test
CFLAGS = -W -Wall -ansi -O3 -g -pedantic -std=c99

all: $(EXEC)

cli-parser_test: cli-parser_test.c cli-parser.o
	gcc $< cli-parser.o -o $@ $(CFLAGS)

cli-parser.o: cli-parser.c cli-parser.h
	gcc -c $< -o $@ $(CFLAGS)

mrproper: clean
	rm -f *.o

clean:
	rm -f $(EXEC)
