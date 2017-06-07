#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "server.h"

Server* new_server(char *hostname, int port) {
  Server *server = NULL;
  int bind_res;

  server = malloc(sizeof(Server));
  if (server == NULL) {
    perror("malloc");
    return NULL;
  }

  memset(&server->addr, 0, sizeof(server->addr));
  server->addr.sin_family = AF_INET;
  server->addr.sin_port = htons(port);
  
  if (inet_pton(AF_INET, hostname, &(server->addr.sin_addr)) != 1) {
    perror("net_pton");
    return NULL;
  }

  server->socket_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (server->socket_fd == -1) {
    perror("socket");
    return NULL;
  }

  bind_res = bind(server->socket_fd, (struct sockaddr*) &server->addr, sizeof(struct sockaddr));
  if (bind_res == -1) {
    perror("bind");
    close(server->socket_fd);
    return NULL;
  }

  server->closed = 0;
  pthread_mutex_init(&server->closed_mutex, NULL);

  return server;
}

void _start_server_listening(Server *server) {
  if (listen(server->socket_fd, 0) == -1) {
    perror("listen");
    return ;
  }

  printf("Server listening at %s\n", inet_ntoa(server->addr.sin_addr));
}

void start_server(Server *server, void (*conn_handler)(ClientConn client)) {
  socklen_t sock_size = sizeof(struct sockaddr_in);
  struct sockaddr_in addr;
  ClientConn client;
  int conn_fd;

  _start_server_listening(server);

  while (!is_server_closed(server)) {
    conn_fd = accept(server->socket_fd, (struct sockaddr*) &addr, &sock_size);
    if (conn_fd == -1) {
      perror("accpet");
      continue;
    }

    client.conn_fd = conn_fd;
    client.addr = addr;
    client.server = server;

    conn_handler(client);
  }
}

void start_server_multithreaded(Server *server, void (*conn_handler)(ClientConn client)) {
  socklen_t sock_size = sizeof(struct sockaddr_in);
  struct sockaddr_in addr;
  ClientConn client;
  int conn_fd;
  pthread_t thread;

  _start_server_listening(server);

  while (!is_server_closed(server)) {
    conn_fd = accept(server->socket_fd, (struct sockaddr*) &addr, &sock_size);
    if (conn_fd == -1) {
      perror("accpet");
      continue;
    }

    client.conn_fd = conn_fd;
    client.addr = addr;
    client.server = server;

    /* if (pthread_create(&thread, NULL, conn_handler_threaded, (void*) &client) != 0) { */
    /*   perror("pthread_create"); */
    /*   continue; */
    /* } */
  }
}

void* conn_handler_threaded(void *data) {
  

  return NULL;
}

void start_select_server(Server *server) {
  socklen_t sock_size = sizeof(struct sockaddr_in);
  struct sockaddr_in addr;
  fd_set readfds; /* , writefds, exeptfds; */
  int nfds, conn_fd, i, n;
  char line[256];

  _start_server_listening(server);

  FD_ZERO(&readfds);
  /* FD_ZERO(&writefds); */
  /* FD_ZERO(&exeptfds); */

  FD_SET(server->socket_fd, &readfds);

  while (!is_server_closed(server)) {
    nfds = select(FD_SETSIZE, &readfds, NULL, NULL, NULL);
    if (nfds == -1) {
      perror("select");
      continue;
    }

    printf("nfds = %d\n", nfds);

    for (i = 0; i < nfds; i++) {
      if (FD_ISSET(i, &readfds)) {
	printf("FD %d is in readfds\n", i);

	if (i == server->socket_fd) {
	  conn_fd = accept(server->socket_fd, (struct sockaddr*) &addr, &sock_size);
	  if (conn_fd == -1) {
	    perror("accpet");
	    continue;
	  }

	  printf("New incoming connection from %s\n",
	    inet_ntoa(addr.sin_addr));

	  FD_SET(conn_fd, &readfds);
	  /* FD_SET(conn_fd, &writefds); */
	} else {
	  n = read(i, line, 256);
	  if (n > 0) {
	    FD_CLR(i, &readfds);
	  }

	  printf("%s <- %s\n",
	    inet_ntoa(addr.sin_addr), line);
	}
      } /* else if (FD_ISSET(i, &writefds)) { */
      /* 	printf("FD %d is in writefds\n", i); */

      /* 	n = write(i, line, 256); */
      /* 	if (n == 0) { */
      /* 	  FD_CLR(i, &writefds); */
      /* 	} */

      /* 	printf("%s -> %s\n", inet_ntoa(addr.sin_addr), line); */
      /* } else if (FD_ISSET(i, &exeptfds)) { */
      /* 	printf("FD %d is in exeptfds\n", i); */
      /* }  */ else {
	printf("FD %d is in no sets\n", i);
      }
    }
  }
}

void close_server(Server *server) {
  pthread_mutex_lock(&server->closed_mutex);

  printf("Closing server listening at %s\n", inet_ntoa(server->addr.sin_addr));

  close(server->socket_fd);
  server->closed = 1;

  pthread_mutex_unlock(&server->closed_mutex);
}

int is_server_closed(Server *server) {
  int is_closed;

  pthread_mutex_lock(&server->closed_mutex);
  is_closed = server->closed;
  pthread_mutex_unlock(&server->closed_mutex);

  return is_closed;
}
