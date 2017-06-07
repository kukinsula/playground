TEST = list_unit_test hash_list_unit_test
LIB = liblist.so

CC = gcc
DEBUG = true

LIB_SRC = double_linked_circular_list.c hash_list.c
LIB_OBJ = $(patsubst %.c,%.o, $(LIB_SRC) )

ifeq ($(DEBUG), true)
  CFLAGS = -W -Wall -ansi -O3 -g # -pedantic
else
  CFLAGS =
endif

LDFLAGS = -llist

all: lib test

lib: $(LIB)

$(LIB): $(LIB_OBJ)
	$(CC) -shared -o $(LIB) $^ $(CFLAGS)

%.o: %.c %.h
	$(CC) -c -fPIC $< -o $@ $(CFLAGS)

test: $(TEST)

list_unit_test: $(LIB) list_unit_test.c list_unit_test.h unit_test.h
	$(CC) -L. -Wl,-rpath=. -o $@ list_unit_test.c $(LDFLAGS) $(CFLAGS)

hash_list_unit_test: $(LIB) hash_list_unit_test.c hash_list_unit_test.h unit_test.h
	$(CC) -L. -Wl,-rpath=. -o $@ hash_list_unit_test.c $(LDFLAGS) $(CFLAGS)



.PHONY: help clean purge mrproper stat

help:
	@echo "**************************************************************************"
	@echo "*                                                                        *"
	@echo "*                                List                                    *"
	@echo "*                                                                        *"
	@echo "**************************************************************************"
	@echo
	@echo " Règles disponibles:"
	@echo
	@echo " * make [all]            : compilation de tout list"
	@echo " * make lib              : compilation de la librairie $(LIB)"
	@echo " * make test             : compilation des tests $(TEST)"
	@echo
	@echo " * clean                 : suppression de $(LIB_OBJ)"
	@echo " * purge                 : suppression de $(LIB) et de $(TEST)"
	@echo " * mrproper              : clean et purge"
	@echo
	@echo " * help                  : règles disponibles"
	@echo "--------------------------------------------------------------------------"
	@echo

clean:
	rm -f $(LIB_OBJ)

purge:
	rm -f $(LIB)
	rm -f $(TEST)

mrproper: clean purge

stat:
	@wc -l *.c *.h
