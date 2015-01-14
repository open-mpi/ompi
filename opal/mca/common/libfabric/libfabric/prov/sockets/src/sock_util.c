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

#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>

#include "sock.h"
#include "sock_util.h"

int sock_log_level = SOCK_ERROR;
char host[128] = {0};
#define SOCK_SENDTO_TIMEOUT 5

int sock_util_sendto(int fd, void *buf, size_t len, struct sockaddr_in *addr,
		socklen_t addrlen, int timeout)
{
	struct timeval tv;
	fd_set writefds;
	socklen_t optlen;
	int optval;

	if (sendto(fd, buf, len, 0, addr, addrlen) < 0) {
		SOCK_LOG_ERROR("sendto failed with error %d - %s\n", errno,
				strerror(errno));
		return -errno;
	}

	if (timeout) {
		tv.tv_sec = 0;
		tv.tv_usec = timeout;
	} else {
		tv.tv_sec = SOCK_SENDTO_TIMEOUT;
		tv.tv_usec = 0;
	}
	FD_ZERO(&writefds);
	FD_SET(fd, &writefds);
	if (select(fd+1, NULL, &writefds, NULL, &tv) > 0) {
		optlen = sizeof(int);
		getsockopt(fd, SOL_SOCKET, SO_ERROR, &optval, &optlen);

		if (optval) {
			SOCK_LOG_ERROR("failed to sendto %d - %s\n", optval,
					strerror(optval));
			close(fd);
			return -errno;
		}
	} else {
		SOCK_LOG_ERROR("Timeout or error to sendto %d - %s\n", optval,
				strerror(optval));
		close(fd);
		errno = ETIMEDOUT;
		return -FI_ETIMEDOUT;
	}

	return 0;
}

int sock_util_recvfrom(int fd, void *buf, size_t len, struct sockaddr_in *addr,
		socklen_t *addrlen, int timeout)
{
	struct timeval tv;
	struct timeval *tv_ptr;
	fd_set readfds;
	socklen_t optlen;
	int optval;
	int ret;

	if (timeout < 0) {
		/* negative timeout means an infinite timeout */
		tv_ptr = NULL;
	} else {
		tv.tv_sec = 0;
		tv.tv_usec = timeout;
		tv_ptr = &tv;
	}

	FD_ZERO(&readfds);
	FD_SET(fd, &readfds);
	if (select(fd+1, &readfds, NULL, NULL, tv_ptr) > 0) {
		optlen = sizeof(int);
		getsockopt(fd, SOL_SOCKET, SO_ERROR, &optval, &optlen);

		if (optval) {
			SOCK_LOG_ERROR("failed to connect %d - %s\n", optval,
					strerror(optval));
			close(fd);
			return 0;
		}

	} else {
		SOCK_LOG_ERROR("Timeout or error to connect %d - %s\n", optval,
				strerror(optval));
		close(fd);
		errno = ETIMEDOUT;
		return 0;
	}

	/* read */
	ret = recvfrom(fd, buf, len, 0, addr, addrlen);
	if (ret < 0) {
		SOCK_LOG_ERROR("error recvfrom for sread: %d - %s\n", errno,
				strerror(errno));
		return 0;
	} 

	return ret;
}
