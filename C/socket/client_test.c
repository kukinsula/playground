#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdlib.h>

#include "client.h"

void print_usage();

int main (int argc, char *argv[]) {
  Client *client = NULL;
  int port;

  if (strncmp(argv[0], "--help", 6) == 0 || strncmp(argv[0], "-h", 2) == 0) {
    print_usage();
    exit(EXIT_SUCCESS);
  }

  if (argc != 3) {
    print_usage();
    exit(EXIT_SUCCESS);
  }

  port = atoi(argv[2]);
  client = new_client(argv[1], port);

  connect_client(client);
  close_client(client);
  free_client(client);

  return 0;
}

void print_usage() {
  printf("usage: ./client HOST PORT\n");
}
