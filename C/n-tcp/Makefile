EXEC = n-tcp client
DEBUG = true

LDFLAGS = -pthread

ifeq ($(DEBUG), true)
	CFLAGS = -W -Wall -ansi -O3 -g -pedantic
else
	CFLAGS =
endif

all: $(EXEC)

n-tcp: server.o n-tcp.c
	gcc n-tcp.c $< -o $@ $(CFLAGS) $(LDFLAGS)

client: client.o n-client.c
	gcc n-client.c $< -o $@ $(CFLAGS) $(LDFLAGS)

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
