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

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_CTRL, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_CTRL, __VA_ARGS__)

int sock_conn_map_init(struct sock_conn_map *map, int init_size)
{
	map->table = calloc(init_size, sizeof(*map->table));
	if (!map->table) 
		return -FI_ENOMEM;

	map->used = 0;
	map->size = init_size;
	return 0;
}

static int sock_conn_map_increase(struct sock_conn_map *map, int new_size) 
{
	void *_table;

	if (map->used + new_size > map->size) {
		_table = realloc(map->table, new_size * sizeof(*map->table));
		if (!_table)
			return -FI_ENOMEM;

		map->size = new_size;
		map->table = _table;
	}
	return 0;
}

void sock_conn_map_destroy(struct sock_conn_map *cmap)
{
	free(cmap->table);
	cmap->table = NULL;
	cmap->used = cmap->size = 0;
}

struct sock_conn *
sock_conn_map_lookup_key(struct sock_conn_map *conn_map, uint16_t key)
{
	if (key > conn_map->used) {
		SOCK_LOG_ERROR("requested key is larger than conn_map size\n");
		errno = EINVAL;
		return NULL;
	}

	return &conn_map->table[key - 1];
}

int sock_compare_addr(struct sockaddr_in *addr1,
			     struct sockaddr_in *addr2)
{
	return ((addr1->sin_addr.s_addr == addr2->sin_addr.s_addr) &&
		(addr1->sin_port == addr2->sin_port));
}

uint16_t sock_conn_map_lookup(struct sock_conn_map *map,
			      struct sockaddr_in *addr)
{
	int i;

	for (i = 0; i < map->used; i++) {
		if (sock_compare_addr(&map->table[i].addr, addr)) {
			return i + 1;
		}
	}
	return 0;
}

static int sock_conn_map_insert(struct sock_conn_map *map,
				struct sockaddr_in *addr,
				struct sock_ep *ep,
				int conn_fd)
{
	int index;

	if (map->size == map->used) {
		if (sock_conn_map_increase(map, map->size * 2)) {
			return 0;
		}
	}
	
	index = map->used;
	map->table[index].addr = *addr;
	map->table[index].sock_fd = conn_fd;
	map->table[index].ep = ep;
	sock_comm_buffer_init(&map->table[index]);
	map->used++;
	return index + 1;
}				 

int fd_set_nonblock(int fd)
{
	int flags, ret;

	flags = fcntl(fd, F_GETFL, 0);
	ret = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
	if (ret) {
		SOCK_LOG_ERROR("fcntl failed\n");
		ret = -errno;
	}

	return ret;
}

void sock_set_sockopts(int sock)
{
	int optval;
	optval = 1;
	if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof optval))
		SOCK_LOG_ERROR("setsockopt reuseaddr failed\n");

	if (setsockopt(sock, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof optval))
		SOCK_LOG_ERROR("setsockopt nodelay failed\n");

	fd_set_nonblock(sock);
}

uint16_t sock_conn_map_connect(struct sock_ep *ep,
			       struct sock_domain *dom,
			       struct sock_conn_map *map, 
			       struct sockaddr_in *addr)
{
	int conn_fd, optval = 0, ret;
	char use_conn;
	struct timeval tv;
	socklen_t optlen;
	fd_set fds;

	conn_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (conn_fd < 0) {
		SOCK_LOG_ERROR("failed to create conn_fd, errno: %d\n", errno);
		return 0;
	}
	
	SOCK_LOG_INFO("Connecting to: %s:%d\n", inet_ntoa(addr->sin_addr),
		      ntohs(addr->sin_port));

	if (connect(conn_fd, (struct sockaddr *) addr, sizeof *addr) < 0) {
		if (errno == EINPROGRESS) {
			/* timeout after 5 secs */
			tv.tv_sec = 5;
			tv.tv_usec = 0;
			FD_ZERO(&fds);
			FD_SET(conn_fd, &fds);
			if (select(conn_fd+1, NULL, &fds, NULL, &tv) > 0) {
				optlen = sizeof(int);
				getsockopt(conn_fd, SOL_SOCKET, SO_ERROR,
					   &optval, &optlen);
				if (optval) {
					SOCK_LOG_ERROR("failed to connect %d - %s\n",
							optval, strerror(optval));
					goto err;
				}
			} else {
				SOCK_LOG_ERROR("Timeout or error to connect %d - %s\n",
						optval, strerror(optval));
				goto err;
			}
		} else {
			SOCK_LOG_ERROR("Error connecting %d - %s\n", errno,
				       strerror(errno));
			goto err;
		}
	}
	
	do {
		ret = send(conn_fd, 
			   &((struct sockaddr_in*) ep->src_addr)->sin_port,
			   sizeof(((struct sockaddr_in*) ep->src_addr)->sin_port), 0);
	} while(ret == -1 && (errno == EAGAIN || errno == EWOULDBLOCK));
	if (ret != sizeof(((struct sockaddr_in*) ep->src_addr)->sin_port)) {
		SOCK_LOG_ERROR("Cannot exchange port\n");
		goto err;
	}

	do {
		ret = recv(conn_fd, &use_conn, sizeof(use_conn), 0);
	} while(ret == -1 && (errno == EAGAIN || errno == EWOULDBLOCK));
	if (ret != sizeof(use_conn)) {
		SOCK_LOG_ERROR("Cannot exchange port: %d\n", ret);
		goto err;
	}

	SOCK_LOG_INFO("Connect response: %d\n", use_conn);

	if (use_conn) {
		fastlock_acquire(&map->lock);
		ret = sock_conn_map_insert(map, addr, ep, conn_fd);
		fastlock_release(&map->lock);
	} else {
		close(conn_fd);
		SOCK_LOG_INFO("waiting for an accept\n");
		for (ret = 0; !ret; ) {
			fastlock_acquire(&map->lock);
			ret = sock_conn_map_lookup(map, addr);
			fastlock_release(&map->lock);
		}
		SOCK_LOG_INFO("got accept\n");
	}

	return ret;

err:
	close(conn_fd);
	return 0;
}

uint16_t sock_conn_map_match_or_connect(struct sock_ep *ep,
					struct sock_domain *dom,
					struct sock_conn_map *map, 
					struct sockaddr_in *addr)
{
	uint16_t index;
	fastlock_acquire(&map->lock);
	index = sock_conn_map_lookup(map, addr);
	fastlock_release(&map->lock);

	if (!index)
		index = sock_conn_map_connect(ep, dom, map, addr);
	return index;
}

static void *_sock_conn_listen(void *arg)
{
	uint16_t index;
	int conn_fd, ret;
	char tmp, use_conn;
	socklen_t addr_size;
	struct sockaddr_in remote;
	struct pollfd poll_fds[2];

	struct sock_ep *ep = (struct sock_ep *)arg;
	struct sock_conn_listener *listener = &ep->listener;
	struct sock_conn_map *map = &ep->domain->r_cmap;

	poll_fds[0].fd = listener->sock;
	poll_fds[1].fd = listener->signal_fds[1];
	poll_fds[0].events = poll_fds[1].events = POLLIN;

 	while (listener->do_listen) {
		if (poll(poll_fds, 2, -1) > 0) {
			if (poll_fds[1].revents & POLLIN) {
				ret = read(listener->signal_fds[1], &tmp, 1);
				if (ret != 1) {
					SOCK_LOG_ERROR("Invalid signal\n");
					goto err;
				}
				continue;
			}
		} else {
			goto err;
		}

		addr_size = sizeof(remote);
		conn_fd = accept(listener->sock, (struct sockaddr *) &remote, &addr_size);
		SOCK_LOG_INFO("CONN: accepted conn-req: %d\n", conn_fd);
		if (conn_fd < 0) {
			SOCK_LOG_ERROR("failed to accept: %d\n", errno);
			goto err;
		}

		SOCK_LOG_INFO("ACCEPT: %s, %d\n", inet_ntoa(remote.sin_addr),
				ntohs(remote.sin_port));

		do {
			ret = recv(conn_fd, &remote.sin_port, 
				   sizeof(remote.sin_port), 0);
		} while(ret == -1 && (errno == EAGAIN || errno == EWOULDBLOCK));
		if (ret != sizeof(remote.sin_port))
			SOCK_LOG_ERROR("Cannot exchange port\n");

		SOCK_LOG_INFO("Remote port: %d\n", ntohs(remote.sin_port));

		fastlock_acquire(&map->lock);
		index = sock_conn_map_lookup(map, &remote);
		if (!index) {
			sock_conn_map_insert(map, &remote, ep, conn_fd);
			use_conn = 1;
		} else {
			use_conn = 0;
		}
		fastlock_release(&map->lock);

		do {
			ret = send(conn_fd, &use_conn, sizeof(use_conn), 0);
		} while(ret == -1 && (errno == EAGAIN || errno == EWOULDBLOCK));
		if (ret != sizeof(use_conn))
			SOCK_LOG_ERROR("Cannot exchange port\n");
		
		if (!use_conn) {
			shutdown(conn_fd, SHUT_RDWR);
			close(conn_fd);
		}
	}

err:
	close(listener->sock);
	SOCK_LOG_INFO("Listener thread exited\n");
	return NULL;
}

int sock_conn_listen(struct sock_ep *ep)
{
	struct addrinfo *s_res = NULL, *p;
	struct addrinfo hints;
	int listen_fd = 0, ret;
	socklen_t addr_size;
	struct sockaddr_in addr;
	struct sock_conn_listener *listener = &ep->listener;
	struct sock_domain *domain = ep->domain;	

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;

	if (getnameinfo((void*)ep->src_addr, sizeof (*ep->src_addr),
			NULL, 0, listener->service, 
			sizeof(listener->service), NI_NUMERICSERV)) {
		SOCK_LOG_ERROR("could not resolve src_addr\n");
		return -FI_EINVAL;
	}
	
	if (!sock_fabric_check_service(domain->fab, atoi(listener->service))) {
		memset(listener->service, 0, NI_MAXSERV);
		((struct sockaddr_in*)ep->src_addr)->sin_port = 0;
	}
	
	ret = getaddrinfo(inet_ntoa(((struct sockaddr_in*)ep->src_addr)->sin_addr),
			  listener->service, &hints, &s_res);
	if (ret) {
		SOCK_LOG_ERROR("no available AF_INET address, service %s, %s\n",
			       listener->service, gai_strerror(ret));
		return -FI_EINVAL;
	}

	SOCK_LOG_INFO("Binding listener thread to port: %s\n", listener->service);
	for (p = s_res; p; p = p->ai_next) {
		listen_fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
		if (listen_fd >= 0) {
			sock_set_sockopts(listen_fd);
			
			if (!bind(listen_fd, s_res->ai_addr, s_res->ai_addrlen))
				break;
			close(listen_fd);
			listen_fd = -1;
		}
	}
	freeaddrinfo(s_res);

	if (listen_fd < 0) {
		SOCK_LOG_ERROR("failed to listen to port: %s\n", 
			       listener->service);
		goto err;
	}
	
	if (atoi(listener->service) == 0) {
		addr_size = sizeof(addr);
		if (getsockname(listen_fd, (struct sockaddr *) &addr, &addr_size))
			goto err;
		snprintf(listener->service, sizeof listener->service, "%d",
			 ntohs(addr.sin_port));
		SOCK_LOG_INFO("Bound to port: %s\n", listener->service);
	}

	if (listen(listen_fd, 0)) {
		SOCK_LOG_ERROR("failed to listen socket: %s\n", strerror(errno));
		goto err;
	}

	((struct sockaddr_in *) (ep->src_addr))->sin_port =
		htons(atoi(listener->service));
	listener->do_listen = 1;
	listener->sock = listen_fd;

	sock_fabric_add_service(domain->fab, atoi(listener->service));
	if (socketpair(AF_UNIX, SOCK_STREAM, 0, listener->signal_fds) < 0)
		goto err;

	fd_set_nonblock(listener->signal_fds[1]);
	return pthread_create(&listener->listener_thread, 0, 
			      _sock_conn_listen, ep);
err:
	if (listen_fd >= 0)
		close(listen_fd);
	return -FI_EINVAL;
}
