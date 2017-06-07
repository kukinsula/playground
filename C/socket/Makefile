EXEC = server_test client_test
DEBUG = true

LDFLAGS = -pthread

ifeq ($(DEBUG), true)
	CFLAGS = -W -Wall -ansi -O3 -g -pedantic
else
	CFLAGS =
endif

all: $(EXEC)

server_test: server.o server_test.c
	gcc server_test.c $< -o $@ $(CFLAGS) $(LDFLAGS)

client_test: client.o client_test.c
	gcc client_test.c $< -o $@ $(CFLAGS) $(LDFLAGS)

server.o: server.c server.h
	gcc $< -c -o $@ $(CFLAGS) $(LDFLAGS)

client.o: client.c client.h
	gcc $< -c -o $@ $(CFLAGS) $(LDFLAGS)

mrproper: clean
	rm -f *.o

clean:
	rm -f $(EXEC)

stat:
	@wc -l *.c *.h
