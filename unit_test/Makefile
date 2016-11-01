EXEC = unit_test_test
CFLAGS = -W -Wall -ansi -O3 -g -pedantic -std=c99

all: $(EXEC)

unit_test_test: unit_test_test.c unit_test.h
	gcc $< -o $@ $(CFLAGS)

mrproper: clean
	rm -f *.o

clean:
	rm -f $(EXEC)
