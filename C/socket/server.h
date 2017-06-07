#ifndef __SERVER__
#define __SERVER__

#include <pthread.h>

#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>

typedef struct {
  int socket_fd;
  struct sockaddr_in addr;

  unsigned char closed;
  pthread_mutex_t closed_mutex;

  /* TODO: liste des clients connéctés */
} Server;

/* TODO: ServerGroup = liste de tous les serveurs créés (multiton) */

typedef struct {
  int conn_fd;
  struct sockaddr_in addr;
  Server *server;
} ClientConn;

Server* new_server(char *hostname, int port);
void start_server(Server *server, void (*conn_handler)(ClientConn client_conn));
void start_select_server(Server *server);
void close_server(Server *server);
int is_server_closed(Server *server);

/* TODO: close_all_servers() */

#endif
