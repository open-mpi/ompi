/*
 * Copyright (c) 2014 Intel Corporation, Inc.  All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdlib.h>
#include <stdio.h>

#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <ifaddrs.h>
#include <poll.h>

#include "sock.h"
#include "sock_util.h"

static int _init_map(struct sock_conn_map *map, int init_size) 
{
	map->table = (struct sock_conn*)calloc(init_size, 
			sizeof(struct sock_conn));
	if (!map->table) 
		return -FI_ENOMEM;
	map->used = 0;
	map->size = init_size;
	return 0;
}

static int _increase_map(struct sock_conn_map *map, int new_size) 
{
	if (map->used + new_size > map->size) {
		void *_table = realloc(map->table, map->size * sizeof(struct
					sock_conn));
		if (!_table)
			return -FI_ENOMEM;

		map->size = MAX(map->size, new_size) * 2;
		map->table = (struct sock_conn*) _table;
	}

	return 0;
}

void sock_conn_map_destroy(struct sock_conn_map *cmap)
{
	free(cmap->table);
	cmap->table = NULL;
	cmap->used = cmap->size = 0;
}

struct sock_conn *sock_conn_map_lookup_key(struct sock_conn_map *conn_map, 
		uint16_t key) 
{
	if (key > conn_map->used) {
		SOCK_LOG_ERROR("requested key is larger than conn_map size\n");
		errno = EINVAL;
		return NULL;
	}

	return &conn_map->table[key-1];
}

#define SOCK_ADDR_IN_PTR(sa)((struct sockaddr_in *)(sa))
#define SOCK_ADDR_IN_FAMILY(sa)SOCK_ADDR_IN_PTR(sa)->sin_family
#define SOCK_ADDR_IN_PORT(sa)SOCK_ADDR_IN_PTR(sa)->sin_port
#define SOCK_ADDR_IN_ADDR(sa)SOCK_ADDR_IN_PTR(sa)->sin_addr

uint16_t sock_conn_map_match_or_connect(struct sock_domain *dom,
					struct sock_conn_map *map, 
					struct sockaddr_in *addr, int match_only)
{
	int i, conn_fd, arg, optval;
	socklen_t optlen;
	char sa_ip[INET_ADDRSTRLEN];
	struct sockaddr_in *entry;
	struct timeval tv;
	fd_set fds;
	struct sock_conn *conn;

	/* match */
	for (i=0; i < map->used; i++) {
		entry = (struct sockaddr_in *)&(map->table[i].addr);
		if ((SOCK_ADDR_IN_ADDR(entry).s_addr == 
		     SOCK_ADDR_IN_ADDR(addr).s_addr) &&
		    (SOCK_ADDR_IN_PORT(entry) == SOCK_ADDR_IN_PORT(addr))) {
			return i+1;
		}
	}

	if (match_only)
		return 0;

	/* no matching entry, connect */
	conn_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (conn_fd < 0) {
		SOCK_LOG_ERROR("failed to create conn_fd, errno: %d\n", errno);
		return 0;
	}
	
	optval = 1;
	setsockopt(conn_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof optval);
	optval = 1;
	setsockopt(conn_fd, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof optval);

	fcntl(conn_fd, F_SETFL, O_NONBLOCK);
	memcpy(sa_ip, inet_ntoa(addr->sin_addr), INET_ADDRSTRLEN);
	SOCK_LOG_INFO("Connecting to: %s:%d\n",
		      sa_ip, ntohs(((struct sockaddr_in*)addr)->sin_port));
	
	if (bind(conn_fd, (struct sockaddr*)&dom->src_addr, 
		 sizeof(struct sockaddr_in)) != 0) {
		SOCK_LOG_ERROR("Cannot bind to src_addr\n");
		return 0;
	}
	
	SOCK_LOG_INFO("Binding conn_fd  to port: %d\n", 
		      ntohs(((struct sockaddr_in*)&dom->src_addr)->sin_port));

	if (connect(conn_fd, addr, sizeof *addr) < 0) {
		if (errno == EINPROGRESS) {
			/* timeout after 5 secs */
			tv.tv_sec = 5;
			tv.tv_usec = 0;
			FD_ZERO(&fds);
			FD_SET(conn_fd, &fds);
			if (select(conn_fd+1, NULL, &fds, NULL, &tv) > 0) {
				optlen = sizeof(int);
				getsockopt(conn_fd, SOL_SOCKET, SO_ERROR, &optval, &optlen);

				if (optval) {
					SOCK_LOG_ERROR("failed to connect %d - %s\n", optval,
							strerror(optval));
					close(conn_fd);
					return 0;
				}
			} else {
				SOCK_LOG_ERROR("Timeout or error to connect %d - %s\n", optval,
						strerror(optval));
				close(conn_fd);
				return 0;
			}
		} else {
			SOCK_LOG_ERROR("Error connecting %d - %s\n", errno,
					strerror(errno));
			close(conn_fd);
			return 0;
		}
	}

	arg = fcntl(conn_fd, F_GETFL, NULL);
	arg &= (~O_NONBLOCK);
	fcntl(conn_fd, F_SETFL, arg);

	memcpy(&map->table[map->used].addr, addr, sizeof *addr);
	map->table[map->used].sock_fd = conn_fd;

	conn = &map->table[map->used];
	sock_comm_buffer_init(conn);

	map->used++;
	return map->used;
}

static void * _sock_conn_listen(void *arg)
{
	struct sock_domain *domain = (struct sock_domain*) arg;
	struct sock_conn_map *map = &domain->r_cmap;
	struct addrinfo *s_res = NULL, *p;
	struct addrinfo hints;
	int optval, flags, tmp;
	int listen_fd = 0, conn_fd;
	struct sockaddr_in remote;
	socklen_t addr_size;
	struct sock_conn *conn;
	struct pollfd poll_fds[2];
	char service[NI_MAXSERV];
	struct sockaddr_in addr;
	char sa_ip[INET_ADDRSTRLEN];

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;

	sprintf(service, "%d", domain->service);
	if(getaddrinfo(NULL, service, &hints, &s_res)) {
		SOCK_LOG_ERROR("no available AF_INET address\n");
		perror("no available AF_INET address");
		return NULL;
	}

	SOCK_LOG_INFO("Binding listener thread to port: %d\n", domain->service);
	for (p=s_res; p; p=p->ai_next) {
		listen_fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
		if (listen_fd >= 0) {
			flags = fcntl(listen_fd, F_GETFL, 0);
			fcntl(listen_fd, F_SETFL, flags | O_NONBLOCK);

			optval = 1;
			setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof
					optval);
			optval = 1;
			setsockopt(listen_fd, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof
					optval);


			if (!bind(listen_fd, s_res->ai_addr, s_res->ai_addrlen))
				break;
			close(listen_fd);
			listen_fd = -1;
		}
	}
	freeaddrinfo(s_res);

	if (listen_fd < 0) {
		SOCK_LOG_ERROR("failed to listen to port: %d\n", domain->service);
		goto err;
	}
	
	if (domain->service == 0) {
		addr_size = sizeof(struct sockaddr_in);
		if (getsockname(listen_fd, (struct sockaddr*)&addr, &addr_size))
			goto err;
		domain->service = ntohs(addr.sin_port);
		SOCK_LOG_INFO("Bound to port: %d\n", domain->service);
	}

	if (listen(listen_fd, 128)) {
		SOCK_LOG_ERROR("failed to listen socket: %d\n", errno);
		goto err;
	}

	((struct sockaddr_in*)&(domain->src_addr))->sin_port = 
		htons(domain->service);
	domain->listening = 1;

	poll_fds[0].fd = listen_fd;
	poll_fds[1].fd = domain->signal_fds[1];
	poll_fds[0].events = poll_fds[1].events = POLLIN;
 	while(domain->listening) {
		if (poll(poll_fds, 2, -1) > 0) {
			if (poll_fds[1].revents & POLLIN) {
				read(domain->signal_fds[1], &tmp, 1);
				continue;
			}
		} else
			goto err;

		addr_size = sizeof(struct sockaddr_in);
		conn_fd = accept(listen_fd, (struct sockaddr *)&remote, &addr_size);
		SOCK_LOG_INFO("CONN: accepted conn-req: %d\n", conn_fd);
		if (conn_fd < 0) {
			SOCK_LOG_ERROR("failed to accept: %d\n", errno);
			goto err;
		}
		
		addr_size = sizeof(struct sockaddr_in);
		getpeername(conn_fd, &remote, &addr_size);

		memcpy(sa_ip, inet_ntoa(remote.sin_addr), INET_ADDRSTRLEN);
		SOCK_LOG_INFO("ACCEPT: %s, %d\n", sa_ip, ntohs(remote.sin_port));

		/* TODO: lock for multi-threads */
		if ((map->size - map->used) == 0) {
			_increase_map(map, map->size*2);
		}
		memcpy(&map->table[map->used].addr, &remote, addr_size);
		map->table[map->used].sock_fd = conn_fd;

		conn = &map->table[map->used];
		sock_comm_buffer_init(conn);

		map->used++;
	}

	close(listen_fd);
	return NULL;

err:
	close(listen_fd);
	perror("listening thread failed");
	return NULL;
}

int sock_conn_listen(struct sock_domain *domain)
{
	_init_map(&domain->r_cmap, 128); /* TODO: init cmap size */
	pthread_create(&domain->listen_thread, 0, _sock_conn_listen, domain);
	return 0;
}
