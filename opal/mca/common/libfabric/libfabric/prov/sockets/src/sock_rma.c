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

#include <assert.h>
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
#include <arpa/inet.h>
#include <limits.h>

#include "sock.h"
#include "sock_util.h"

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_DATA, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_DATA, __VA_ARGS__)

static ssize_t sock_ep_rma_readmsg(struct fid_ep *ep, 
					const struct fi_msg_rma *msg, 
					uint64_t flags)
{
	int ret, i;
	struct sock_op tx_op;
	union sock_iov tx_iov;
	struct sock_conn *conn;
	struct sock_tx_ctx *tx_ctx;
	uint64_t total_len, src_len, dst_len;
	struct sock_ep *sock_ep;

	switch (ep->fid.fclass) {
	case FI_CLASS_EP:
		sock_ep = container_of(ep, struct sock_ep, ep);
		tx_ctx = sock_ep->tx_ctx;
		break;

	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(ep, struct sock_tx_ctx, fid.ctx);
		sock_ep = tx_ctx->ep;
		break;

	default:
		SOCK_LOG_ERROR("Invalid EP type\n");
		return -FI_EINVAL;
	}

	assert(tx_ctx->enabled && 
	       msg->iov_count <= SOCK_EP_MAX_IOV_LIMIT &&
	       msg->rma_iov_count <= SOCK_EP_MAX_IOV_LIMIT);

	if (sock_ep->connected) {
		conn = sock_ep_lookup_conn(sock_ep);
	} else {
		conn = sock_av_lookup_addr(sock_ep, tx_ctx->av, msg->addr);
	}

	if (!conn)
		return -FI_EAGAIN;

	total_len = sizeof(struct sock_op_send) + 
		(msg->iov_count * sizeof(union sock_iov)) +
		(msg->rma_iov_count * sizeof(union sock_iov));

	sock_tx_ctx_start(tx_ctx);
	if (rbfdavail(&tx_ctx->rbfd) < total_len) {
		ret = -FI_EAGAIN;
		goto err;
	}

	flags |= tx_ctx->attr.op_flags;	
	memset(&tx_op, 0, sizeof(struct sock_op));
	tx_op.op = SOCK_OP_READ;
	tx_op.src_iov_len = msg->rma_iov_count;
	tx_op.dest_iov_len = msg->iov_count;

	sock_tx_ctx_write_op_send(tx_ctx, &tx_op, flags, (uintptr_t) msg->context,
			msg->addr, (uintptr_t) msg->msg_iov[0].iov_base,
			sock_ep, conn);

	if (flags & FI_REMOTE_CQ_DATA) {
		sock_tx_ctx_write(tx_ctx, &msg->data, sizeof(msg->data));
	}

	src_len = 0;
	for (i = 0; i< msg->rma_iov_count; i++) {
		tx_iov.iov.addr = msg->rma_iov[i].addr;
		tx_iov.iov.key = msg->rma_iov[i].key;
		tx_iov.iov.len = msg->rma_iov[i].len;
		sock_tx_ctx_write(tx_ctx, &tx_iov, sizeof(tx_iov));
		src_len += tx_iov.iov.len;
	}

	dst_len = 0;
	for (i = 0; i< msg->iov_count; i++) {
		tx_iov.iov.addr = (uintptr_t) msg->msg_iov[i].iov_base;
		tx_iov.iov.len = msg->msg_iov[i].iov_len;
		tx_iov.iov.key = (uintptr_t) msg->desc[i];
		sock_tx_ctx_write(tx_ctx, &tx_iov, sizeof(tx_iov));
		dst_len += tx_iov.iov.len;
	}

	if (dst_len != src_len) {
		SOCK_LOG_ERROR("Buffer length mismatch\n");
		ret = -FI_EINVAL;
		goto err;
	}
	
	sock_tx_ctx_commit(tx_ctx);
	return 0;

err:
	SOCK_LOG_INFO("Not enough space for TX entry, try again\n");
	sock_tx_ctx_abort(tx_ctx);
	return ret;
}

static ssize_t sock_ep_rma_read(struct fid_ep *ep, void *buf, size_t len,
				 void *desc, fi_addr_t src_addr, uint64_t addr, 
				 uint64_t key, void *context)
{
	struct fi_msg_rma msg;
	struct iovec msg_iov;
	struct fi_rma_iov rma_iov;

	memset(&msg, 0, sizeof msg);
	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.desc = &desc;
	msg.iov_count = 1;

	rma_iov.addr = addr;
	rma_iov.key = key;
	rma_iov.len = len;
	msg.rma_iov_count = 1;
	msg.rma_iov = &rma_iov;

	msg.addr = src_addr;
	msg.context = context;

	return sock_ep_rma_readmsg(ep, &msg, 0);
}

static ssize_t sock_ep_rma_readv(struct fid_ep *ep, const struct iovec *iov,
				  void **desc, size_t count, fi_addr_t src_addr, 
				  uint64_t addr, uint64_t key, void *context)
{
	size_t len, i;
	struct fi_msg_rma msg;
	struct fi_rma_iov rma_iov;

	memset(&msg, 0, sizeof msg);
	msg.msg_iov = iov;
	msg.desc = desc;
	msg.iov_count = count;
	msg.rma_iov_count = 1;

	rma_iov.addr = addr;
	rma_iov.key = key;

	for (i = 0, len = 0; i < count; i++) 
		len += iov[i].iov_len;
	rma_iov.len = len;

	msg.rma_iov = &rma_iov;
	msg.addr = src_addr;
	msg.context = context;

	return sock_ep_rma_readmsg(ep, &msg, 0);
}

static ssize_t sock_ep_rma_writemsg(struct fid_ep *ep, 
				     const struct fi_msg_rma *msg, 
				     uint64_t flags)
{
	int ret, i;
	struct sock_op tx_op;
	union sock_iov tx_iov;
	struct sock_conn *conn;
	struct sock_tx_ctx *tx_ctx;
	uint64_t total_len, src_len, dst_len;
	struct sock_ep *sock_ep;

	switch (ep->fid.fclass) {
	case FI_CLASS_EP:
		sock_ep = container_of(ep, struct sock_ep, ep);
		tx_ctx = sock_ep->tx_ctx;
		break;

	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(ep, struct sock_tx_ctx, fid.ctx);
		sock_ep = tx_ctx->ep;
		break;

	default:
		SOCK_LOG_ERROR("Invalid EP type\n");
		return -FI_EINVAL;
	}

	assert(tx_ctx->enabled && 
	       msg->iov_count <= SOCK_EP_MAX_IOV_LIMIT &&
	       msg->rma_iov_count <= SOCK_EP_MAX_IOV_LIMIT);
	if (sock_ep->connected) {
		conn = sock_ep_lookup_conn(sock_ep);
	} else {
		conn = sock_av_lookup_addr(sock_ep, tx_ctx->av, msg->addr);
	}

	if (!conn)
		return -FI_EAGAIN;

	flags |= tx_ctx->attr.op_flags;
	memset(&tx_op, 0, sizeof(struct sock_op));
	tx_op.op = SOCK_OP_WRITE;
	tx_op.dest_iov_len = msg->rma_iov_count;
	
	total_len = 0;
	if (flags & FI_INJECT) {
		for (i=0; i< msg->iov_count; i++) {
			total_len += msg->msg_iov[i].iov_len;
		}
		assert(total_len <= SOCK_EP_MAX_INJECT_SZ);
		tx_op.src_iov_len = total_len;
	} else {
		total_len += msg->iov_count * sizeof(union sock_iov);
		tx_op.src_iov_len = msg->iov_count;
	}

	total_len += (sizeof(struct sock_op_send) +
		      (msg->rma_iov_count * sizeof(union sock_iov)));

	sock_tx_ctx_start(tx_ctx);
	if (rbfdavail(&tx_ctx->rbfd) < total_len) {
		ret = -FI_EAGAIN;
		goto err;
	}

	sock_tx_ctx_write_op_send(tx_ctx, &tx_op, flags, (uintptr_t) msg->context,
			msg->addr, (uintptr_t) msg->msg_iov[0].iov_base,
			sock_ep, conn);

	if (flags & FI_REMOTE_CQ_DATA) {
		sock_tx_ctx_write(tx_ctx, &msg->data, sizeof(msg->data));
	}

	src_len = 0;
	if (flags & FI_INJECT) {
		for (i=0; i< msg->iov_count; i++) {
			sock_tx_ctx_write(tx_ctx, msg->msg_iov[i].iov_base,
					  msg->msg_iov[i].iov_len);
			src_len += msg->msg_iov[i].iov_len;
		}
	} else {
		for (i = 0; i< msg->iov_count; i++) {
			tx_iov.iov.addr = (uintptr_t) msg->msg_iov[i].iov_base;
			tx_iov.iov.len = msg->msg_iov[i].iov_len;
			tx_iov.iov.key = (uintptr_t) msg->desc[i];
			sock_tx_ctx_write(tx_ctx, &tx_iov, sizeof(tx_iov));
			src_len += tx_iov.iov.len;
		}
	}

	dst_len = 0;
	for (i = 0; i< msg->rma_iov_count; i++) {
		tx_iov.iov.addr = msg->rma_iov[i].addr;
		tx_iov.iov.key = msg->rma_iov[i].key;
		tx_iov.iov.len = msg->rma_iov[i].len;
		sock_tx_ctx_write(tx_ctx, &tx_iov, sizeof(tx_iov));
		dst_len += tx_iov.iov.len;
	}
	
	if (dst_len != src_len) {
		SOCK_LOG_ERROR("Buffer length mismatch\n");
		ret = -FI_EINVAL;
		goto err;
	}
	
	sock_tx_ctx_commit(tx_ctx);
	return 0;

err:
	SOCK_LOG_INFO("Not enough space for TX entry, try again\n");
	sock_tx_ctx_abort(tx_ctx);
	return ret;
}

static ssize_t sock_ep_rma_write(struct fid_ep *ep, const void *buf, 
				  size_t len, void *desc, fi_addr_t dest_addr, 
				  uint64_t addr, uint64_t key, void *context)
{
	struct fi_msg_rma msg;
	struct iovec msg_iov;
	struct fi_rma_iov rma_iov;

	memset(&msg, 0, sizeof msg);
	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;

	msg.msg_iov = &msg_iov;
	msg.desc = &desc;
	msg.iov_count = 1;

	rma_iov.addr = addr;
	rma_iov.key = key;
	rma_iov.len = len;

	msg.rma_iov_count = 1;
	msg.rma_iov = &rma_iov;

	msg.addr = dest_addr;
	msg.context = context;

	return sock_ep_rma_writemsg(ep, &msg, 0);
}

static ssize_t sock_ep_rma_writev(struct fid_ep *ep, 
				   const struct iovec *iov, void **desc,
				   size_t count, fi_addr_t dest_addr, uint64_t addr, 
				   uint64_t key, void *context)
{
	int i;
	size_t len;
	struct fi_msg_rma msg;
	struct fi_rma_iov rma_iov;

	memset(&msg, 0, sizeof msg);
	msg.msg_iov = iov;
	msg.desc = desc;
	msg.iov_count = count;
	msg.rma_iov_count = 1;

	for (i = 0, len = 0; i < count; i++) 
		len += iov[i].iov_len;
	
	rma_iov.addr = addr;
	rma_iov.key = key;
	rma_iov.len = len;
	
	msg.rma_iov = &rma_iov;
	msg.context = context;
	msg.addr = dest_addr;

	return sock_ep_rma_writemsg(ep, &msg, 0);
}

static ssize_t sock_ep_rma_writedata(struct fid_ep *ep, const void *buf, 
				      size_t len, void *desc, uint64_t data, 
				      fi_addr_t dest_addr, uint64_t addr, 
				      uint64_t key, void *context)
{
	struct fi_msg_rma msg;
	struct iovec msg_iov;
	struct fi_rma_iov rma_iov;

	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.desc = &desc;
	msg.iov_count = 1;
	msg.rma_iov_count = 1;

	rma_iov.addr = addr;
	rma_iov.key = key;
	rma_iov.len = len;

	msg.rma_iov = &rma_iov;
	msg.msg_iov = &msg_iov;

	msg.addr = dest_addr;
	msg.context = context;
	msg.data = data;

	return sock_ep_rma_writemsg(ep, &msg, FI_REMOTE_CQ_DATA);
}

static ssize_t sock_ep_rma_inject(struct fid_ep *ep, const void *buf, 
				   size_t len, fi_addr_t dest_addr, uint64_t addr, 
				   uint64_t key)
{
	struct fi_msg_rma msg;
	struct iovec msg_iov;
	struct fi_rma_iov rma_iov;

	memset(&msg, 0, sizeof msg);
	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.iov_count = 1;
	msg.rma_iov_count = 1;

	rma_iov.addr = addr;
	rma_iov.key = key;
	rma_iov.len = len;

	msg.rma_iov = &rma_iov;
	msg.msg_iov = &msg_iov;
	msg.addr = dest_addr;

	return sock_ep_rma_writemsg(ep, &msg, FI_INJECT | SOCK_NO_COMPLETION);
}

static ssize_t sock_ep_rma_injectdata(struct fid_ep *ep, const void *buf, 
				       size_t len, uint64_t data, fi_addr_t dest_addr, 
				       uint64_t addr, uint64_t key)
{
	struct fi_msg_rma msg;
	struct iovec msg_iov;
	struct fi_rma_iov rma_iov;

	memset(&msg, 0, sizeof msg);
	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.iov_count = 1;
	msg.rma_iov_count = 1;

	rma_iov.addr = addr;
	rma_iov.key = key;
	rma_iov.len = len;

	msg.rma_iov = &rma_iov;
	msg.msg_iov = &msg_iov;
	msg.addr = dest_addr;
	msg.data = data;
	return sock_ep_rma_writemsg(ep, &msg, FI_INJECT | FI_REMOTE_CQ_DATA |
		SOCK_NO_COMPLETION);
}


struct fi_ops_rma sock_ep_rma = {
	.size  = sizeof(struct fi_ops_rma),
	.read = sock_ep_rma_read,
	.readv = sock_ep_rma_readv,
	.readmsg = sock_ep_rma_readmsg,
	.write = sock_ep_rma_write,
	.writev = sock_ep_rma_writev,
	.writemsg = sock_ep_rma_writemsg,
	.inject = sock_ep_rma_inject,
	.injectdata = sock_ep_rma_injectdata,
	.writedata = sock_ep_rma_writedata,
};

