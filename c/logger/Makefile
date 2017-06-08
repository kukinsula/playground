EXEC = logger_test
CFLAGS = -W -Wall -ansi -O3 -g -pedantic -std=c99

all: $(EXEC)

logger_test: logger_test.c logger.h
	gcc logger_test.c -o $@ $(CFLAGS)

mrproper: clean
	rm -f *.o

clean:
	rm -f $(EXEC)
