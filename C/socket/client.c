#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <arpa/inet.h>

#include <sys/socket.h>

#include "client.h"

Client* new_client(char *hostname, int port) {
  Client *client = NULL;

  client = malloc(sizeof(Client));
  if (client == NULL) {
    perror("new_client malloc");
    return NULL;
  }

  memset(&client->addr, 0, sizeof(client->addr));
  client->addr.sin_family = AF_INET;
  client->addr.sin_port = htons(port);
  
  if (inet_pton(AF_INET, hostname, &(client->addr.sin_addr)) != 1) {
    perror("net_pton");
    return NULL;
  }

  client->socket_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (client->socket_fd == -1) {
    perror("new_client socket");
    return NULL;
  }

  return client;
}

void connect_client(Client *client) {
  char line[126];
  int n;

  if (connect(client->socket_fd, (struct sockaddr *) &client->addr, sizeof(client->addr))) {
    perror("connect_client connectt");
    return;
  }

  printf("Client successfully connected to %s\n",
    inet_ntoa(client->addr.sin_addr));

  do {
    memset(&line, 0, sizeof(line));

    n = read(STDIN_FILENO, line, 126);
    if (n == -1) {
      perror("connect_client error read");
      break;
    } else if (n == 0) {
      printf("connect_client read EOF");
      break;
    }

    n = write(client->socket_fd, line, 126);
    if (n == -1) {
      perror("connect_client error write");
      break;
    } else if (n == 0) {
      printf("connect_client write EOF");
      break;
    }

    memset(&line, 0, sizeof(line));

    n = read(client->socket_fd, line, 126);
    if (n == -1) {
      perror("connect_client error read");
      break;
    } else if (n == 0) {
      printf("connect_client read EOF");
      break;
    }

    printf("<- %s", line);

    memset(&line, 0, sizeof(line));
  } while (n > 0);

  printf("Client finished !");
}

int read_line_client(Client *client, char *buffer, size_t len) {
  return read(client->socket_fd, buffer, len);
}

int write_line_client(Client *client, char *buffer, size_t len) {
  return write(client->socket_fd, buffer, len);
}

void close_client(Client *client) {
  close(client->socket_fd);
}

void free_client(Client *client) {
  free(client);
}
