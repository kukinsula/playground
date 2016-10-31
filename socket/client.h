#ifndef __CLIENT__
#define __CLIENT__

#include <netinet/in.h>

typedef struct {
  int socket_fd;
  struct sockaddr_in addr;
} Client;

Client* new_client(char *hostname, int port);
void connect_client(Client *client);
int read_line_client(Client *client, char *buffer, size_t len);
int write_line_client(Client *client, char *buffer, size_t len);
void close_client(Client *client);
void free_client(Client *client);

#endif
