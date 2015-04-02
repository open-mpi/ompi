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
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <pthread.h>

#include "sock.h"
#include "sock_util.h"

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_DATA, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_DATA, __VA_ARGS__)

static ssize_t sock_comm_send_socket(struct sock_conn *conn, const void *buf, size_t len)
{
	ssize_t ret;

	ret = write(conn->sock_fd, buf, len);
	if (ret < 0) {
		SOCK_LOG_INFO("write %s\n", strerror(errno));
		ret = 0;
	}

	SOCK_LOG_INFO("wrote to network: %lu\n", ret);
	return ret;
}

ssize_t sock_comm_flush(struct sock_conn *conn)
{
	ssize_t ret1, ret2 = 0;
	size_t endlen, len, xfer_len;

	len = rbused(&conn->outbuf);
	endlen = conn->outbuf.size - (conn->outbuf.rcnt & conn->outbuf.size_mask);

	xfer_len = MIN(len, endlen);
	ret1 = sock_comm_send_socket(conn, conn->outbuf.buf +
				     (conn->outbuf.rcnt & conn->outbuf.size_mask),
				     xfer_len);
	if (ret1 > 0)
		conn->outbuf.rcnt += ret1;

	if (ret1 == xfer_len && xfer_len < len) {
		ret2 = sock_comm_send_socket(conn, conn->outbuf.buf +
					     (conn->outbuf.rcnt & conn->outbuf.size_mask),
					     len - xfer_len);
		if (ret2 > 0)
			conn->outbuf.rcnt += ret2;
		else
			ret2 = 0;
	}

	return (ret1 > 0) ? ret1 + ret2 : 0;
}

ssize_t sock_comm_send(struct sock_conn *conn, const void *buf, size_t len)
{
	ssize_t ret, used;

	if (len >= SOCK_COMM_THRESHOLD) {
		used = rbused(&conn->outbuf);
		if (used == sock_comm_flush(conn)) {
			return sock_comm_send_socket(conn, buf, len);
		} else {
			return 0;
		}
	}

	if (rbavail(&conn->outbuf) < len) {
		ret = sock_comm_flush(conn);
		if (ret <= 0)
			return 0;
	}

	ret = MIN(rbavail(&conn->outbuf), len);
	rbwrite(&conn->outbuf, buf, ret);
	rbcommit(&conn->outbuf);
	SOCK_LOG_INFO("buffered %lu\n", ret);
	return ret;
}

static ssize_t sock_comm_recv_socket(struct sock_conn *conn, void *buf, size_t len)
{
	ssize_t ret;
	
	ret = read(conn->sock_fd, buf, len);
	if (ret < 0) {
		SOCK_LOG_INFO("read %s\n", strerror(errno));
		ret = 0;
	}

	if (ret > 0)
		SOCK_LOG_INFO("read from network: %lu\n", ret);
	return ret;
}

static ssize_t sock_comm_recv_buffer(struct sock_conn *conn)
{
	int ret;
	size_t endlen;

	endlen = conn->inbuf.size - (conn->inbuf.wpos & conn->inbuf.size_mask);
	ret = sock_comm_recv_socket(conn,(char*) conn->inbuf.buf +
					 (conn->inbuf.wpos & conn->inbuf.size_mask), 
					 endlen);
	if (ret <= 0)
		return 0;

	conn->inbuf.wpos += ret;
	rbcommit(&conn->inbuf);
	if (ret != endlen)
		return ret;

	ret = sock_comm_recv_socket(conn, conn->inbuf.buf, rbavail(&conn->inbuf));
	if (ret <= 0)
		return 0;

	conn->inbuf.wpos += ret;
	rbcommit(&conn->inbuf);
	return 0;
}

ssize_t sock_comm_recv(struct sock_conn *conn, void *buf, size_t len)
{
	int ret = 0;
	ssize_t used, read_len;

	used = rbused(&conn->inbuf);
	if (used == 0) {
		ret = sock_comm_recv_socket(conn, buf, len);
		sock_comm_recv_buffer(conn);
		return ret;
	}

	read_len = MIN(len, used);
	rbread(&conn->inbuf, buf, read_len);
	if (len > used) {
		ret = sock_comm_recv_socket(conn, (char*) buf + used, len - used);
		if (ret <= 0)
			ret = 0;
		sock_comm_recv_buffer(conn);
	}
	SOCK_LOG_INFO("read from buffer: %lu\n", ret + read_len);
	return ret + read_len;
}

ssize_t sock_comm_peek(struct sock_conn *conn, void *buf, size_t len)
{
	sock_comm_recv_buffer(conn);
	if (rbused(&conn->inbuf) >= len) {
		rbpeek(&conn->inbuf, buf, len);
		return len;
	} 
	return 0;
}

ssize_t sock_comm_data_avail(struct sock_conn *conn)
{
	sock_comm_recv_buffer(conn);
	return rbused(&conn->inbuf);
}

int sock_comm_buffer_init(struct sock_conn *conn)
{
	socklen_t size = SOCK_COMM_BUF_SZ;
	socklen_t optlen = sizeof(socklen_t);

	sock_set_sockopts(conn->sock_fd);
	rbinit(&conn->inbuf, SOCK_COMM_BUF_SZ);
	rbinit(&conn->outbuf, SOCK_COMM_BUF_SZ);

	if (setsockopt(conn->sock_fd, SOL_SOCKET, SO_RCVBUF, &size, optlen))
		SOCK_LOG_ERROR("setsockopt SO_RCVBUF failed\n");

	if (setsockopt(conn->sock_fd, SOL_SOCKET, SO_SNDBUF, &size, optlen))
		SOCK_LOG_ERROR("setsockopt SO_SNDBUF failed\n");

	if (!getsockopt(conn->sock_fd, SOL_SOCKET, SO_RCVBUF, &size, &optlen))
		SOCK_LOG_INFO("SO_RCVBUF: %d\n", size);
	
	optlen = sizeof(socklen_t);
	if (!getsockopt(conn->sock_fd, SOL_SOCKET, SO_SNDBUF, &size, &optlen))
		SOCK_LOG_INFO("SO_SNDBUF: %d\n", size);
	return 0;
}

void sock_comm_buffer_finalize(struct sock_conn *conn)
{
	rbfree(&conn->inbuf);
	rbfree(&conn->outbuf);
}
