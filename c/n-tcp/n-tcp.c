#include <stdio.h>
#include <pthread.h>
#include <signal.h>

#include "server.h"

#define LINE_LENGTH 256

static Server *server = NULL;

void echo_multithreaded(ClientConn client);
void* echo(void *data);
void signal_handler(int dummy);
void print_usage();

int main (int argc, char *argv[]) {
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
  server = new_server(argv[1], port);
  if (server == NULL) {
    return 1;
  }

  signal(SIGINT, signal_handler);
  start_server(server, echo_multithreaded);
  /* start_select_server(server); */

  return 0;
}

void echo_multithreaded(ClientConn client) {
  pthread_t thread;

  if (pthread_create(&thread, NULL, echo, &client) != 0) {
    perror("pthread_create failed");
    return ;
  }
}

void* echo(void *data) {
  ClientConn *client;
  char line[LINE_LENGTH];
  ssize_t n;

  client = (ClientConn*) data;

  printf("New incoming connection from %s:%d\n",
    inet_ntoa(client->addr.sin_addr), client->addr.sin_port);

  for (;;) {
    n = read(client->conn_fd, line, LINE_LENGTH);
    if (n == 0) {
      break;
    } else if (n == -1) {
      perror("error on read");
      break;
    }

    printf("%s:%d <- %s",
      inet_ntoa(client->addr.sin_addr), client->addr.sin_port, line);

    n = write(client->conn_fd, line, LINE_LENGTH);
    if (n == 0) {
      break;
    } else if (n == -1) {
      perror("error on write");
      break;
    }

    printf("%s:%d -> %s",
      inet_ntoa(client->addr.sin_addr), client->addr.sin_port, line);

    memset(&line, 0, sizeof(line));
  }

  printf("%s:%d connection closed\n",
    inet_ntoa(client->addr.sin_addr), client->addr.sin_port);

  close(client->conn_fd);

  return NULL;
}

void signal_handler(int dummy) {
  close_server(server);
  exit(EXIT_SUCCESS);
}

void print_usage() {
  printf("usage: n-tcp [--help|-h] HOST PORT");
}
