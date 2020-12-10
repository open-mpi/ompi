/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_SOCKADDR_H_INCLUDED
#define MPL_SOCKADDR_H_INCLUDED

#include <sys/socket.h>

#define MPL_SOCKADDR_ANY 0
#define MPL_SOCKADDR_LOOPBACK 1

/* modifies the default behavior of MPL_Listen
 * a - whether listen on loopback (default: listen on all interfaces)
 * b - maximum simultaneous connection (default: SOMAXCONN)
 */
#define MPL_LISTEN_PUSH(a,b) MPL_set_listen_attr(a, b)
#define MPL_LISTEN_POP MPL_set_listen_attr(0, SOMAXCONN)

typedef struct sockaddr_storage MPL_sockaddr_t;

/* The following functions when return an int, it returns 0 on success,
 * non-zero indicates error. It is consistent with posix socket functions.
 */
void MPL_sockaddr_set_aftype(int type);
int MPL_get_sockaddr(const char *s_hostname, MPL_sockaddr_t * p_addr);
int MPL_get_sockaddr_direct(int type, MPL_sockaddr_t * p_addr);
int MPL_get_sockaddr_iface(const char *s_iface, MPL_sockaddr_t * p_addr);
int MPL_socket(void);
int MPL_connect(int socket, MPL_sockaddr_t * p_addr, unsigned short port);
void MPL_set_listen_attr(int use_loopback, int max_conn);
int MPL_listen(int socket, unsigned short port);
int MPL_listen_anyport(int socket, unsigned short *p_port);
int MPL_listen_portrange(int socket, unsigned short *p_port, int low_port, int high_port);
int MPL_sockaddr_to_str(MPL_sockaddr_t * p_addr, char *str, int maxlen);
int MPL_sockaddr_port(MPL_sockaddr_t * p_addr);

#endif /* MPL_SOCKADDR_H_INCLUDED */
