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

#include "sock.h"
#include "sock_util.h"


extern const struct fi_domain_attr sock_domain_attr;
extern const struct fi_fabric_attr sock_fabric_attr;

const struct fi_ep_attr sock_rdm_ep_attr = {
	.protocol = FI_PROTO_SOCK_RDS,
	.max_msg_size = SOCK_EP_MAX_MSG_SZ,
	.inject_size = SOCK_EP_MAX_INJECT_SZ,
	.total_buffered_recv = SOCK_EP_MAX_BUFF_RECV,
	.max_order_raw_size = SOCK_EP_MAX_ORDER_RAW_SZ,
	.max_order_war_size = SOCK_EP_MAX_ORDER_WAR_SZ,
	.max_order_waw_size = SOCK_EP_MAX_ORDER_WAW_SZ,
	.mem_tag_format = SOCK_EP_MEM_TAG_FMT,
	.msg_order = SOCK_EP_MSG_ORDER,
	.tx_ctx_cnt = SOCK_EP_MAX_TX_CNT,
	.rx_ctx_cnt = SOCK_EP_MAX_RX_CNT,
};

const struct fi_tx_ctx_attr sock_rdm_tx_attr = {
	.caps = SOCK_EP_RDM_CAP,
	.op_flags = SOCK_OPS_CAP,
	.msg_order = 0,
	.inject_size = SOCK_EP_MAX_INJECT_SZ,
	.size = SOCK_EP_MAX_TX_CTX_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

const struct fi_rx_ctx_attr sock_rdm_rx_attr = {
	.caps = SOCK_EP_RDM_CAP,
	.op_flags = SOCK_OPS_CAP,
	.msg_order = 0,
	.total_buffered_recv = 0,
	.size = SOCK_EP_MAX_MSG_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

static int sock_rdm_verify_rx_attr(const struct fi_rx_ctx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | sock_rdm_rx_attr.caps) != sock_rdm_rx_attr.caps)
		return -FI_ENODATA;

	if ((attr->op_flags | sock_rdm_rx_attr.op_flags) != 
	   sock_rdm_rx_attr.op_flags)
		return -FI_ENODATA;

	if (attr->msg_order != sock_rdm_rx_attr.msg_order)
		return -FI_ENODATA;

	if (attr->total_buffered_recv > sock_rdm_rx_attr.total_buffered_recv)
		return -FI_ENODATA;

	if (attr->size > sock_rdm_rx_attr.size)
		return -FI_ENODATA;

	if (attr->iov_limit > sock_rdm_rx_attr.iov_limit)
		return -FI_ENODATA;

	return 0;
}

static int sock_rdm_verify_tx_attr(const struct fi_tx_ctx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | sock_rdm_tx_attr.caps) != sock_rdm_tx_attr.caps)
		return -FI_ENODATA;

	if ((attr->op_flags | sock_rdm_tx_attr.op_flags) != 
	   sock_rdm_tx_attr.op_flags)
		return -FI_ENODATA;

	if (attr->msg_order != sock_rdm_tx_attr.msg_order)
		return -FI_ENODATA;

	if (attr->inject_size > sock_rdm_tx_attr.inject_size)
		return -FI_ENODATA;

	if (attr->size > sock_rdm_tx_attr.size)
		return -FI_ENODATA;

	if (attr->iov_limit > sock_rdm_tx_attr.iov_limit)
		return -FI_ENODATA;

	return 0;
}

int sock_rdm_verify_ep_attr(struct fi_ep_attr *ep_attr,
			    struct fi_tx_ctx_attr *tx_attr,
			    struct fi_rx_ctx_attr *rx_attr)
{
	if (ep_attr) {
		switch (ep_attr->protocol) {
		case FI_PROTO_UNSPEC:
		case FI_PROTO_SOCK_RDS:
			break;
		default:
			return -FI_ENODATA;
		}

		if (ep_attr->max_msg_size > sock_rdm_ep_attr.max_msg_size)
			return -FI_ENODATA;

		if (ep_attr->inject_size > sock_rdm_ep_attr.inject_size)
			return -FI_ENODATA;

		if (ep_attr->total_buffered_recv > 
		   sock_rdm_ep_attr.total_buffered_recv)
			return -FI_ENODATA;

		if (ep_attr->max_order_raw_size >
		   sock_rdm_ep_attr.max_order_raw_size)
			return -FI_ENODATA;

		if (ep_attr->max_order_war_size >
		   sock_rdm_ep_attr.max_order_war_size)
			return -FI_ENODATA;

		if (ep_attr->max_order_waw_size > 
		   sock_rdm_ep_attr.max_order_waw_size)
			return -FI_ENODATA;

		if (ep_attr->msg_order !=
		   sock_rdm_ep_attr.msg_order)
			return -FI_ENODATA;

		if (ep_attr->tx_ctx_cnt > sock_rdm_ep_attr.tx_ctx_cnt)
			return -FI_ENODATA;

		if (ep_attr->rx_ctx_cnt > sock_rdm_ep_attr.rx_ctx_cnt)
			return -FI_ENODATA;
	}

	if (sock_rdm_verify_tx_attr(tx_attr) || sock_rdm_verify_rx_attr(rx_attr))
		return -FI_ENODATA;

	return 0;
}


static struct fi_info *allocate_fi_info(enum fi_ep_type ep_type, 
					int addr_format, struct fi_info *hints,
					void *src_addr, void *dest_addr)
{
	struct fi_info *_info = fi_allocinfo_internal();
	if (!_info)
		return NULL;

	_info->src_addr = calloc(1, sizeof(struct sockaddr_in));
	_info->dest_addr = calloc(1, sizeof(struct sockaddr_in));
	
	_info->next = NULL;	
	_info->ep_type = ep_type;
	_info->addr_format = addr_format;
	_info->dest_addrlen =_info->src_addrlen = sizeof(struct sockaddr_in);

	if (src_addr) {
		memcpy(_info->src_addr, src_addr, sizeof(struct sockaddr_in));
	}
	
	if (dest_addr) {
		memcpy(_info->dest_addr, dest_addr, sizeof(struct sockaddr_in));
	}

	if (hints->caps) {
		_info->caps = hints->caps;
	} else {
		_info->caps = SOCK_EP_RDM_CAP;
	}

	*(_info->tx_attr) = sock_rdm_tx_attr;
	*(_info->rx_attr) = sock_rdm_rx_attr;
	*(_info->ep_attr) = sock_rdm_ep_attr;

	*(_info->domain_attr) = sock_domain_attr;
	_info->domain_attr->name = strdup(sock_dom_name);

	*(_info->fabric_attr) = sock_fabric_attr;
	_info->fabric_attr->name = strdup(sock_fab_name);
	_info->fabric_attr->prov_name = strdup(sock_fab_name);

	return _info;
}

int sock_rdm_getinfo(uint32_t version, const char *node, const char *service,
		     uint64_t flags, struct fi_info *hints, struct fi_info **info)
{
	int ret;
	struct fi_info *_info;
	void *src_addr = NULL, *dest_addr = NULL;

	if (!info)
		return -FI_EBADFLAGS;

	*info = NULL;
	
	if (!node && !service && !hints)
		return -FI_EBADFLAGS;

	if (version != FI_VERSION(SOCK_MAJOR_VERSION, 
				 SOCK_MINOR_VERSION))
		return -FI_ENODATA;

	if (hints) {
		if ((SOCK_EP_RDM_CAP | hints->caps) != SOCK_EP_RDM_CAP) {
			SOCK_LOG_INFO(
				   "Cannot support requested options!\n");
			return -FI_ENODATA;
		}
		
		ret = sock_rdm_verify_rx_attr(hints->rx_attr);
		if (ret)
			return ret;

		ret = sock_rdm_verify_tx_attr(hints->tx_attr);
		if (ret)
			return ret;
	}

	if (node || service) {
		struct addrinfo sock_hints;
		struct addrinfo *result = NULL;
	
		src_addr = calloc(1, sizeof(struct sockaddr_in));
		dest_addr = calloc(1, sizeof(struct sockaddr_in));
			
		memset(&sock_hints, 0, sizeof(struct sockaddr_in));
		sock_hints.ai_family = AF_INET;
		sock_hints.ai_protocol = 0;
		sock_hints.ai_canonname = NULL;
		sock_hints.ai_addr = NULL;
		sock_hints.ai_next = NULL;

		if (flags & FI_SOURCE)
			sock_hints.ai_flags = AI_PASSIVE;

		if (flags & FI_NUMERICHOST)
			sock_hints.ai_flags |= AI_NUMERICHOST;

		
		ret = getaddrinfo(node, service, &sock_hints, &result);
		if (ret != 0) {
			ret = FI_ENODATA;
			SOCK_LOG_INFO("getaddrinfo failed!\n");
			goto err;
		}
		memcpy(src_addr, result->ai_addr, sizeof(struct sockaddr_in));

		if (!(FI_SOURCE & flags)) {
			socklen_t len;
			int udp_sock = socket(AF_INET, SOCK_DGRAM, 0);
			if (0 != connect(udp_sock, result->ai_addr, 
					 result->ai_addrlen)) {
				SOCK_LOG_ERROR(
					   "Failed to get dest_addr\n");
				ret = FI_ENODATA;
				goto err;
			}
			if (0!= getsockname(udp_sock, (struct sockaddr*)dest_addr, 
					    &len)) {
				SOCK_LOG_ERROR(
					   "Failed to get dest_addr\n");
				close(udp_sock);
				ret = FI_ENODATA;
				goto err;
			}
			close(udp_sock);
		}
		freeaddrinfo(result); 
	}

	_info = allocate_fi_info(FI_EP_RDM, FI_SOCKADDR_IN, hints, src_addr, 
				 dest_addr);
	if (!_info) {
		ret = FI_ENOMEM;
		goto err;
	}

	*info = _info;
	free(src_addr);
	free(dest_addr);
	return 0;

err:
	free(src_addr);
	free(dest_addr);
	SOCK_LOG_ERROR("fi_getinfo failed\n");
	return ret;	
}

ssize_t sock_rdm_ctx_recvmsg(struct fid_ep *ep, const struct fi_msg *msg,
		   uint64_t flags)
{
	int i;
	struct sock_rx_ctx *rx_ctx;
	struct sock_rx_entry *rx_entry;

	rx_ctx = container_of(ep, struct sock_rx_ctx, ctx);
	assert(rx_ctx->enabled && msg->iov_count <= SOCK_EP_MAX_IOV_LIMIT);

	/* FIXME: pool of rx_entry */
	rx_entry = calloc(1, sizeof(struct sock_rx_entry));
	if (!rx_entry)
		return -FI_ENOMEM;
	
	dlist_init(&rx_entry->entry);

	rx_entry->rx_op.op = SOCK_OP_RECV;
	rx_entry->rx_op.dest_iov_len = msg->iov_count;

	rx_entry->flags = flags;
	rx_entry->context = (uint64_t)msg->context;
	rx_entry->addr = msg->addr;
	rx_entry->data = msg->data;

	for (i=0; i< msg->iov_count; i++) {
		rx_entry->iov[i].iov.addr = (uint64_t)msg->msg_iov[i].iov_base;
		rx_entry->iov[i].iov.len = (uint64_t)msg->msg_iov[i].iov_len;
	}

	fastlock_acquire(&rx_ctx->lock);
	dlist_insert_tail(&rx_entry->entry, &rx_ctx->rx_entry_list);
	fastlock_release(&rx_ctx->lock);
	return 0;
}

ssize_t sock_rdm_ctx_recv(struct fid_ep *ep, void *buf, size_t len, void *desc,
		    fi_addr_t src_addr, void *context)
{
	struct fi_msg msg;
	struct iovec msg_iov;

	msg_iov.iov_base = buf;
	msg_iov.iov_len = len;

	msg.msg_iov = &msg_iov;
	msg.desc = desc;
	msg.iov_count = 1;
	msg.addr = src_addr;
	msg.context = context;

	return sock_rdm_ctx_recvmsg(ep, &msg, 0);
}

ssize_t sock_rdm_ctx_recvv(struct fid_ep *ep, const struct iovec *iov, 
			   void **desc, size_t count, fi_addr_t src_addr,
			   void *context)
{
	struct fi_msg msg;

	msg.msg_iov = iov;
	msg.desc = desc;
	msg.iov_count = count;
	msg.addr = src_addr;
	msg.context = context;
	return sock_rdm_ctx_recvmsg(ep, &msg, 0);
}

static ssize_t sock_rdm_sendmsg(struct sock_tx_ctx *tx_ctx, struct sock_av *av,
				const struct fi_msg *msg, uint64_t flags)
{
	int ret, i;
	struct sock_op tx_op;
	union sock_iov tx_iov;
	struct sock_conn *conn;
	uint64_t tmp=0, total_len;

	assert(tx_ctx->enabled && msg->iov_count <= SOCK_EP_MAX_IOV_LIMIT);

	if ((ret = sock_av_lookup_addr(av, msg->addr, &conn)))
		return ret;

	total_len = 0;
	if (flags & FI_INJECT) {
		for (i=0; i< msg->iov_count; i++) {
			total_len += msg->msg_iov[i].iov_len;
		}
		assert(total_len <= SOCK_EP_MAX_INJECT_SZ);
	} else {
		total_len = msg->iov_count * sizeof(union sock_iov);
	}

	total_len += sizeof(struct sock_op_send);
	
	if (flags & FI_REMOTE_CQ_DATA)
		total_len += sizeof(uint64_t);

	sock_tx_ctx_start(tx_ctx);
	if (rbfdavail(&tx_ctx->rbfd) < total_len)
		goto err;

	memset(&tx_op, 0, sizeof(struct sock_op));
	tx_op.op = (flags & FI_INJECT) ? SOCK_OP_SEND_INJECT : SOCK_OP_SEND;
	tx_op.src_iov_len = msg->iov_count;

	sock_tx_ctx_write(tx_ctx, &tx_op, sizeof(struct sock_op));
	sock_tx_ctx_write(tx_ctx, &flags, sizeof(uint64_t));
	sock_tx_ctx_write(tx_ctx, msg->context ? msg->context: &tmp, 
			  sizeof(uint64_t));
	sock_tx_ctx_write(tx_ctx, &msg->addr, sizeof(uint64_t));
	sock_tx_ctx_write(tx_ctx, &conn, sizeof(uint64_t));
	if (flags & FI_REMOTE_CQ_DATA) {
		sock_tx_ctx_write(tx_ctx, &msg->data, sizeof(uint64_t));
	}

	if (flags & FI_INJECT) {
		for (i=0; i< msg->iov_count; i++) {
			sock_tx_ctx_write(tx_ctx, msg->msg_iov[i].iov_base, 
					  msg->msg_iov[i].iov_len);
		}
	} else {
		for (i=0; i< msg->iov_count; i++) {
			tx_iov.iov.addr = (uint64_t)msg->msg_iov[i].iov_base;
			tx_iov.iov.len = msg->msg_iov[i].iov_len;
			sock_tx_ctx_write(tx_ctx, &tx_iov, sizeof(union sock_iov));
		}
	}

	sock_tx_ctx_commit(tx_ctx);
	return 0;

err:
	sock_tx_ctx_abort(tx_ctx);
	return -FI_EAGAIN;
}

ssize_t sock_rdm_ctx_sendmsg(struct fid_ep *ep, const struct fi_msg *msg,
		   uint64_t flags)
{
	struct sock_tx_ctx *tx_ctx;
	tx_ctx = container_of(ep, struct sock_tx_ctx, ctx);
	return sock_rdm_sendmsg(tx_ctx, tx_ctx->ep->av, msg, flags);
}

ssize_t sock_rdm_ctx_send(struct fid_ep *ep, const void *buf, size_t len,
			  void *desc, fi_addr_t dest_addr, void *context)
{
	struct fi_msg msg;
	struct iovec msg_iov;

	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.desc = desc;
	msg.iov_count = 1;
	msg.addr = dest_addr;
	msg.context = context;

	return sock_rdm_ctx_sendmsg(ep, &msg, 0);
}

ssize_t sock_rdm_ctx_sendv(struct fid_ep *ep, const struct iovec *iov, 
			   void **desc, size_t count, fi_addr_t dest_addr,
			   void *context)
{
	struct fi_msg msg;
	msg.msg_iov = iov;
	msg.desc = desc;
	msg.iov_count = count;
	msg.addr = dest_addr;
	msg.context = context;
	return sock_rdm_ctx_sendmsg(ep, &msg, 0);
}


ssize_t sock_rdm_ctx_senddata(struct fid_ep *ep, const void *buf,
				size_t len, void *desc, uint64_t data, 
				fi_addr_t dest_addr, void *context)
{
	struct fi_msg msg;
	struct iovec msg_iov;

	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	
	msg.msg_iov = &msg_iov;
	msg.desc = desc;
	msg.iov_count = 1;
	msg.addr = dest_addr;
	msg.context = context;
	msg.data = data;

	return sock_rdm_ctx_sendmsg(ep, &msg, FI_REMOTE_CQ_DATA);
}

static ssize_t sock_rdm_inject(struct sock_tx_ctx *tx_ctx, struct sock_av *av,
				const void *buf, size_t len, fi_addr_t dest_addr)
{
	struct fi_msg msg;
	struct iovec msg_iov;

	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.iov_count = 1;
	msg.addr = dest_addr;

	return sock_rdm_sendmsg(tx_ctx, av, &msg, FI_INJECT);
}

ssize_t sock_rdm_ctx_inject(struct fid_ep *ep, const void *buf, size_t len,
			    fi_addr_t dest_addr)
{
	struct sock_tx_ctx *tx_ctx;
	tx_ctx = container_of(ep, struct sock_tx_ctx, ctx);
	return sock_rdm_inject(tx_ctx, tx_ctx->ep->av, buf, len, dest_addr);
}

struct fi_ops_msg sock_rdm_ctx_msg_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = sock_rdm_ctx_recv,
	.recvv = sock_rdm_ctx_recvv,
	.recvmsg = sock_rdm_ctx_recvmsg,
	.send = sock_rdm_ctx_send,
	.sendv = sock_rdm_ctx_sendv,
	.sendmsg = sock_rdm_ctx_sendmsg,
	.inject = sock_rdm_ctx_inject,
	.senddata = sock_rdm_ctx_senddata,
};

ssize_t sock_rdm_ctx_trecvmsg(struct fid_ep *ep, const struct fi_msg_tagged *msg,
		   uint64_t flags)
{
	int i;
	struct sock_rx_ctx *rx_ctx;
	struct sock_rx_entry *rx_entry;

	rx_ctx = container_of(ep, struct sock_rx_ctx, ctx);
	assert(rx_ctx->enabled && msg->iov_count <= SOCK_EP_MAX_IOV_LIMIT);

	/* FIXME: pool of rx_entry */
	rx_entry = calloc(1, sizeof(struct sock_rx_entry));
	if (!rx_entry)
		return -FI_ENOMEM;
	
	dlist_init(&rx_entry->entry);

	rx_entry->rx_op.op = SOCK_OP_TRECV;
	rx_entry->rx_op.dest_iov_len = msg->iov_count;

	rx_entry->flags = flags;
	rx_entry->context = (uint64_t)msg->context;
	rx_entry->addr = msg->addr;
	rx_entry->data = msg->data;
	rx_entry->tag = msg->tag;
	rx_entry->ignore = msg->ignore;

	for (i=0; i< msg->iov_count; i++) {
		rx_entry->iov[i].iov.addr = (uint64_t)msg->msg_iov[i].iov_base;
		rx_entry->iov[i].iov.len = (uint64_t)msg->msg_iov[i].iov_len;
	}

	fastlock_acquire(&rx_ctx->lock);
	dlist_insert_tail(&rx_entry->entry, &rx_ctx->rx_entry_list);
	fastlock_release(&rx_ctx->lock);
	return 0;
}

ssize_t sock_rdm_ctx_trecv(struct fid_ep *ep, void *buf, size_t len,
			   void *desc, fi_addr_t src_addr, uint64_t tag,
			   uint64_t ignore, void *context)
{
	struct fi_msg_tagged msg;
	struct iovec msg_iov;

	msg_iov.iov_base = buf;
	msg_iov.iov_len = len;

	msg.msg_iov = &msg_iov;
	msg.desc = desc;
	msg.iov_count = 1;
	msg.addr = src_addr;
	msg.context = context;
	msg.tag = tag;
	msg.ignore = ignore;

	return sock_rdm_ctx_trecvmsg(ep, &msg, 0);
}

ssize_t sock_rdm_ctx_trecvv(struct fid_ep *ep, const struct iovec *iov, 
			    void **desc, size_t count, fi_addr_t src_addr,
			    uint64_t tag, uint64_t ignore, void *context)
{
	struct fi_msg_tagged msg;

	msg.msg_iov = iov;
	msg.desc = desc;
	msg.iov_count = count;
	msg.addr = src_addr;
	msg.context = context;
	msg.tag = tag;
	msg.ignore = ignore;
	return sock_rdm_ctx_trecvmsg(ep, &msg, 0);
}

static ssize_t sock_rdm_tsendmsg(struct sock_tx_ctx *tx_ctx, struct sock_av *av,
				 const struct fi_msg_tagged *msg, uint64_t flags)
{
	int ret, i;
	struct sock_op tx_op;
	union sock_iov tx_iov;
	struct sock_conn *conn;
	uint64_t tmp=0, total_len;

	assert(tx_ctx->enabled && msg->iov_count <= SOCK_EP_MAX_IOV_LIMIT);

	if ((ret = sock_av_lookup_addr(av, msg->addr, &conn)))
		return ret;

	total_len = 0;
	if (flags & FI_INJECT) {
		for (i=0; i< msg->iov_count; i++) {
			total_len += msg->msg_iov[i].iov_len;
		}
		assert(total_len <= SOCK_EP_MAX_INJECT_SZ);
	} else {
		total_len = msg->iov_count * sizeof(union sock_iov);
	}

	total_len += sizeof(struct sock_op_tsend);
	if (flags & FI_REMOTE_CQ_DATA)
		total_len += sizeof(uint64_t);
	
	sock_tx_ctx_start(tx_ctx);
	if (rbfdavail(&tx_ctx->rbfd) < total_len)
		goto err;

	memset(&tx_op, 0, sizeof(struct sock_op));
	tx_op.op = (flags & FI_INJECT) ? SOCK_OP_TSEND_INJECT : SOCK_OP_TSEND;
	tx_op.src_iov_len = msg->iov_count;

	sock_tx_ctx_write(tx_ctx, &tx_op, sizeof(struct sock_op));
	sock_tx_ctx_write(tx_ctx, &flags, sizeof(uint64_t));
	sock_tx_ctx_write(tx_ctx, msg->context ? msg->context: &tmp, 
			  sizeof(uint64_t));
	sock_tx_ctx_write(tx_ctx, &msg->addr, sizeof(uint64_t));
	sock_tx_ctx_write(tx_ctx, &conn, sizeof(uint64_t));
	if (flags & FI_REMOTE_CQ_DATA) {
		sock_tx_ctx_write(tx_ctx, &msg->data, sizeof(uint64_t));
	}
	sock_tx_ctx_write(tx_ctx, &msg->tag, sizeof(uint64_t));

	if (flags & FI_INJECT) {
		for (i=0; i< msg->iov_count; i++) {
			sock_tx_ctx_write(tx_ctx, msg->msg_iov[i].iov_base,
					  msg->msg_iov[i].iov_len);
		}
	} else {
		for (i=0; i< msg->iov_count; i++) {
			tx_iov.iov.addr = (uint64_t)msg->msg_iov[i].iov_base;
			tx_iov.iov.len = msg->msg_iov[i].iov_len;
			sock_tx_ctx_write(tx_ctx, &tx_iov, sizeof(union sock_iov));
		}
	}
	
	sock_tx_ctx_commit(tx_ctx);
	return 0;

err:
	sock_tx_ctx_abort(tx_ctx);
	return -FI_EAGAIN;
}

ssize_t sock_rdm_ctx_tsendmsg(struct fid_ep *ep, const struct fi_msg_tagged *msg,
		   uint64_t flags)
{
	struct sock_tx_ctx *tx_ctx;
	tx_ctx = container_of(ep, struct sock_tx_ctx, ctx);
	return sock_rdm_tsendmsg(tx_ctx, tx_ctx->ep->av, msg, flags);
}

ssize_t sock_rdm_ctx_tsend(struct fid_ep *ep, const void *buf, size_t len,
			   void *desc, fi_addr_t dest_addr, uint64_t tag,
			   void *context)
{
	struct fi_msg_tagged msg;
	struct iovec msg_iov;

	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.desc = desc;
	msg.iov_count = 1;
	msg.addr = dest_addr;
	msg.context = context;
	msg.tag = tag;

	return sock_rdm_ctx_tsendmsg(ep, &msg, 0);
}

ssize_t sock_rdm_ctx_tsendv(struct fid_ep *ep, const struct iovec *iov, 
			    void **desc, size_t count, fi_addr_t dest_addr,
			    uint64_t tag, void *context)
{
	struct fi_msg_tagged msg;
	msg.msg_iov = iov;
	msg.desc = desc;
	msg.iov_count = count;
	msg.addr = dest_addr;
	msg.context = context;
	msg.tag = tag;
	return sock_rdm_ctx_tsendmsg(ep, &msg, 0);
}

ssize_t sock_rdm_ctx_tsenddata(struct fid_ep *ep, const void *buf, size_t len,
				void *desc, uint64_t data, fi_addr_t dest_addr,
				uint64_t tag, void *context)
{
	struct fi_msg_tagged msg;
	struct iovec msg_iov;

	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.desc = desc;
	msg.iov_count = 1;
	msg.addr = dest_addr;
	msg.context = context;
	msg.data = data;
	msg.tag = tag;

	return sock_rdm_ctx_tsendmsg(ep, &msg, FI_REMOTE_CQ_DATA);
}

static ssize_t sock_rdm_tinject(struct sock_tx_ctx *tx_ctx, struct sock_av *av,
				const void *buf, size_t len,
				fi_addr_t dest_addr, uint64_t tag)
{
	struct fi_msg_tagged msg;
	struct iovec msg_iov;

	msg_iov.iov_base = (void*)buf;
	msg_iov.iov_len = len;
	msg.msg_iov = &msg_iov;
	msg.iov_count = 1;
	msg.addr = dest_addr;
	msg.tag = tag;
	return sock_rdm_tsendmsg(tx_ctx, av, &msg, FI_INJECT);
}

ssize_t sock_rdm_ctx_tinject(struct fid_ep *ep, const void *buf, size_t len,
			     fi_addr_t dest_addr, uint64_t tag)
{
	struct sock_tx_ctx *tx_ctx;
	tx_ctx = container_of(ep, struct sock_tx_ctx, ctx);
	return sock_rdm_tinject(tx_ctx, tx_ctx->ep->av, buf, len, dest_addr, tag);
}

ssize_t sock_rdm_ctx_tsearch(struct fid_ep *ep, uint64_t *tag, uint64_t ignore,
			     uint64_t flags, fi_addr_t *src_addr, size_t *len, 
			     void *context)
{
	return -FI_ENOSYS;
}


struct fi_ops_tagged sock_rdm_ctx_tagged = {
	.size = sizeof(struct fi_ops_tagged),
	.recv = sock_rdm_ctx_trecv,
	.recvv = sock_rdm_ctx_trecvv,
	.recvmsg = sock_rdm_ctx_trecvmsg,
	.send = sock_rdm_ctx_tsend,
	.sendv = sock_rdm_ctx_tsendv,
	.sendmsg = sock_rdm_ctx_tsendmsg,
	.inject = sock_rdm_ctx_tinject,
	.senddata = sock_rdm_ctx_tsenddata,
	.search = sock_rdm_ctx_tsearch,
};

int sock_rdm_ctx_close(struct fid *fid)
{
	struct sock_ep *ep;
	struct dlist_entry *entry;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	switch (fid->fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx);
		
		for (entry = tx_ctx->ep_list.next; entry != &tx_ctx->ep_list;
		    entry = entry->next) {
			ep = container_of(entry, struct sock_ep, tx_ctx_entry);
			atomic_dec(&ep->num_tx_ctx);
		}
		sock_tx_ctx_free(tx_ctx);
		break;

	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx);
		
		for (entry = rx_ctx->ep_list.next; entry != &rx_ctx->ep_list;
		    entry = entry->next) {
			ep = container_of(entry, struct sock_ep, rx_ctx_entry);
			atomic_dec(&ep->num_rx_ctx);
		}
		sock_rx_ctx_free(rx_ctx);
		break;

	default:
		SOCK_LOG_ERROR("Invalid fid\n");
		return -FI_EINVAL;
	}
	return 0;
}

int sock_rdm_ctx_bind_cq(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct sock_cq *sock_cq;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	sock_cq = container_of(bfid, struct sock_cq, cq_fid.fid);
	switch (fid->fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx);
		if (flags & FI_SEND) {
			tx_ctx->send_cq = sock_cq;
			if (flags & FI_EVENT)
				tx_ctx->send_cq_event = 1;
		}

		if (flags & FI_READ) {
			tx_ctx->read_cq = sock_cq;
			if (flags & FI_EVENT)
				tx_ctx->read_cq_event = 1;
		}

		if (flags & FI_WRITE) {
			tx_ctx->write_cq = sock_cq;
			if (flags & FI_EVENT)
				tx_ctx->write_cq_event = 1;
		}

		if (!tx_ctx->progress) {
			tx_ctx->progress = 1;
			sock_pe_add_tx_ctx(tx_ctx->domain->pe, tx_ctx);
		}
		break;
		
	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx);
		if (flags & FI_RECV) {
			rx_ctx->recv_cq = sock_cq;
			if (flags & FI_EVENT)
				rx_ctx->recv_cq_event = 1;
		}

		if (flags & FI_REMOTE_READ) {
			rx_ctx->rem_read_cq = sock_cq;
			if (flags & FI_EVENT)
				rx_ctx->rem_read_cq_event = 1;
		}

		if (flags & FI_REMOTE_WRITE) {
			rx_ctx->rem_write_cq = sock_cq;
			if (flags & FI_EVENT)
				rx_ctx->rem_write_cq_event = 1;
		}

		if (!rx_ctx->progress) {
			rx_ctx->progress = 1;
			sock_pe_add_rx_ctx(rx_ctx->domain->pe, rx_ctx);
		}
		break;
			
	default:
		SOCK_LOG_ERROR("Invalid fid\n");
		return -FI_EINVAL;
	}
	return 0;
}

int sock_rdm_ctx_bind_cntr(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct sock_cntr *cntr;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	cntr = container_of(bfid, struct sock_cntr, cntr_fid.fid);
	switch (fid->fclass) {
	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(fid, struct sock_tx_ctx, ctx);
		if (flags & FI_SEND)
			tx_ctx->send_cntr = cntr;
		
		if (flags & FI_READ)
			tx_ctx->read_cntr = cntr;

		if (flags & FI_WRITE)
			tx_ctx->write_cntr = cntr;

		if (!tx_ctx->progress) {
			tx_ctx->progress = 1;
			sock_pe_add_tx_ctx(tx_ctx->domain->pe, tx_ctx);
		}
		break;
		
	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(fid, struct sock_rx_ctx, ctx);
		if (flags & FI_RECV) 
			rx_ctx->recv_cntr = cntr;

		if (flags & FI_REMOTE_READ) 
			rx_ctx->rem_read_cntr = cntr;

		if (flags & FI_REMOTE_WRITE) 
			rx_ctx->rem_write_cntr = cntr;
		
		if (!rx_ctx->progress) {
			rx_ctx->progress = 1;
			sock_pe_add_rx_ctx(rx_ctx->domain->pe, rx_ctx);
		}
		break;
			
	default:
		SOCK_LOG_ERROR("Invalid fid\n");
		return -FI_EINVAL;
	}
	return 0;
}

int sock_rdm_ctx_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	switch (bfid->fclass) {
	case FI_CLASS_CQ:
		return sock_rdm_ctx_bind_cq(fid, bfid, flags);

	case FI_CLASS_CNTR:
		return sock_rdm_ctx_bind_cntr(fid, bfid, flags);

	default:
		SOCK_LOG_ERROR("Invalid bind()\n");
		return -FI_EINVAL;
	}

}

struct fi_ops sock_rdm_ctx_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_rdm_ctx_close,
	.bind = sock_rdm_ctx_bind,
	.control = fi_no_control,
};

int sock_rdm_ctx_enable(struct fid_ep *ep)
{
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;

	switch (ep->fid.fclass) {
	case FI_CLASS_RX_CTX:
		rx_ctx = container_of(ep, struct sock_rx_ctx, ctx);
		rx_ctx->enabled = 1;
		return 0;

	case FI_CLASS_TX_CTX:
		tx_ctx = container_of(ep, struct sock_tx_ctx, ctx);
		tx_ctx->enabled = 1;
		return 0;

	default:
		SOCK_LOG_ERROR("Invalid CTX\n");
		break;
	}
	return -FI_EINVAL;
}

int sock_rdm_ctx_getopt(fid_t fid, int level, int optname,
			void *optval, size_t *optlen)
{
	switch (level) {
	case FI_OPT_ENDPOINT:
		return -FI_ENOPROTOOPT;
	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

int sock_rdm_ctx_setopt(fid_t fid, int level, int optname,
			const void *optval, size_t optlen)
{
	switch (level) {
	case FI_OPT_ENDPOINT:
		return -FI_ENOPROTOOPT;
	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

struct fi_ops_ep sock_rdm_ctx_ep_ops = {
	.size = sizeof(struct fi_ops_ep),
	.enable = sock_rdm_ctx_enable,
	.cancel = fi_no_cancel,
	.getopt = sock_rdm_ctx_getopt,
	.setopt = sock_rdm_ctx_setopt,
	.tx_ctx = fi_no_tx_ctx,
	.rx_ctx = fi_no_rx_ctx,
};

int sock_rdm_ep_fi_close(struct fid *fid)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(fid, struct sock_ep, ep.fid);

	if (atomic_get(&sock_ep->ref) || atomic_get(&sock_ep->num_rx_ctx) ||
	   atomic_get(&sock_ep->num_tx_ctx))
		return -FI_EBUSY;

	sock_tx_ctx_free(sock_ep->tx_array[sock_ep->ep_attr.tx_ctx_cnt]);
	sock_rx_ctx_free(sock_ep->rx_array[sock_ep->ep_attr.rx_ctx_cnt]);

	free(sock_ep->tx_array);
	free(sock_ep->rx_array);

	if (sock_ep->src_addr)
		free(sock_ep->src_addr);
	if (sock_ep->dest_addr)
		free(sock_ep->dest_addr);
	
	free(sock_ep);
	return 0;
}

int sock_rdm_ep_fi_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	int ret, i;
	struct sock_ep *ep;
	struct sock_cq *cq;
	struct sock_cntr *cntr;
	struct sock_rx_ctx *rx_ctx;
	struct sock_tx_ctx *tx_ctx;

	ep = container_of(fid, struct sock_ep, ep.fid);
	
	switch (bfid->fclass) {
	case FI_CLASS_EQ:
		return -FI_ENOSYS;

	case FI_CLASS_CQ:
		cq = container_of(bfid, struct sock_cq, cq_fid.fid);
		if (ep->domain != cq->domain)
			return -EINVAL;

		if (flags & FI_SEND) {
			ep->send_cq = cq;
			if (flags & FI_EVENT)
				ep->send_cq_event = 1;
		}

		if (flags & FI_READ) {
			ep->read_cq = cq;
			if (flags & FI_EVENT)
				ep->read_cq_event = 1;
		}

		if (flags & FI_WRITE) {
			ep->write_cq = cq;
			if (flags & FI_EVENT)
				ep->write_cq_event = 1;
		}

		if (flags & FI_RECV) {
			ep->recv_cq = cq;
			if (flags & FI_EVENT)
				ep->recv_cq_event = 1;
		}

		if (flags & FI_REMOTE_READ) {
			ep->rem_read_cq = cq;
			if (flags & FI_EVENT)
				ep->rem_read_cq_event = 1;
		}

		if (flags & FI_REMOTE_WRITE) {
			ep->rem_write_cq = cq;
			if (flags & FI_EVENT)
				ep->rem_write_cq_event = 1;
		}

		for (i=0; i<=ep->ep_attr.tx_ctx_cnt; i++) {
			tx_ctx = ep->tx_array[i];

			if (!tx_ctx)
				continue;

			if ((ret = sock_rdm_ctx_bind_cq(&tx_ctx->ctx.fid, 
							bfid, flags)))
				return ret;
		}

		for (i=0; i<=ep->ep_attr.rx_ctx_cnt; i++) {
			rx_ctx = ep->rx_array[i];

			if (!rx_ctx)
				continue;

			if ((ret = sock_rdm_ctx_bind_cq(&rx_ctx->ctx.fid, 
							bfid, flags)))
				return ret;
		}
		break;

	case FI_CLASS_CNTR:
		cntr = container_of(bfid, struct sock_cntr, cntr_fid.fid);
		if (ep->domain != cntr->dom)
			return -EINVAL;

		if (flags & FI_SEND)
			ep->send_cntr = cntr;

		if (flags & FI_RECV)
			ep->recv_cntr = cntr;

		if (flags & FI_READ)
			ep->read_cntr = cntr;

		if (flags & FI_WRITE)
			ep->write_cntr = cntr;

		if (flags & FI_REMOTE_READ)
			ep->rem_read_cntr = cntr;
		
		if (flags & FI_REMOTE_WRITE)
			ep->rem_write_cntr = cntr;
		
		for (i=0; i<=ep->ep_attr.tx_ctx_cnt; i++) {
			tx_ctx = ep->tx_array[i];
			
			if (!tx_ctx)
				continue;

			if ((ret = sock_rdm_ctx_bind_cntr(&tx_ctx->ctx.fid, 
							  bfid, flags)))
				return ret;
		}

		for (i=0; i<=ep->ep_attr.rx_ctx_cnt; i++) {
			rx_ctx = ep->rx_array[i];

			if (!rx_ctx)
				continue;

			if ((ret = sock_rdm_ctx_bind_cntr(&rx_ctx->ctx.fid, 
							  bfid, flags)))
				return ret;
		}
		break;

	case FI_CLASS_AV:
		return -FI_ENOSYS;
/*
		av = container_of(bfid,
				struct sock_av, av_fid.fid);
		if (ep->domain != av->dom)
			return -EINVAL;
		ep->av = av;
		av->connect_fn = sock_rdm_connect_conn_map;
		av->cmap = &av->dom->r_cmap;
		av->port_num = ep->port_num;
		break;
*/

	case FI_CLASS_MR:
		return -FI_ENOSYS;
/*
		if (!bfid->ops || !bfid->ops->bind)
			return -EINVAL;
		err = bfid->ops->bind(bfid, fid, flags);
		if (err)
			return err;
		break;
*/

	default:
		return -ENOSYS;
	}

	return 0;
}

struct fi_ops sock_rdm_ep_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_rdm_ep_fi_close,
	.bind = sock_rdm_ep_fi_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

int sock_rdm_ep_enable(struct fid_ep *ep)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	sock_ep->enabled = 1;
	return 0;
}

int sock_rdm_ep_getopt(fid_t fid, int level, int optname,
		       void *optval, size_t *optlen)
{
	switch (level) {
	case FI_OPT_ENDPOINT:
		return -FI_ENOPROTOOPT;
	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

int sock_rdm_ep_setopt(fid_t fid, int level, int optname,
		       const void *optval, size_t optlen)
{
	switch (level) {
	case FI_OPT_ENDPOINT:
		return -FI_ENOPROTOOPT;
	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

int sock_rdm_ep_tx_ctx(struct fid_sep *sep, int index, struct fi_tx_ctx_attr *attr, 
		       struct fid_ep **tx_ep, void *context)
{
	struct sock_ep *sock_ep;
	struct sock_tx_ctx *tx_ctx;

	sock_ep = container_of(sep, struct sock_ep, ep.fid);
	if (index >= sock_ep->ep_attr.tx_ctx_cnt)
		return -FI_EINVAL;

	tx_ctx = sock_tx_ctx_alloc(&sock_ep->tx_attr, context);
	if (!tx_ctx)
		return -FI_ENOMEM;

	tx_ctx->tx_id = index;
	tx_ctx->ep = sock_ep;
	tx_ctx->domain = sock_ep->domain;
	sock_tx_ctx_add_ep(tx_ctx, sock_ep);

	tx_ctx->ctx.ops = &sock_rdm_ctx_ep_ops;
	tx_ctx->ctx.msg = &sock_rdm_ctx_msg_ops;

	/* TODO */
	tx_ctx->ctx.rma = NULL;
	tx_ctx->ctx.tagged = NULL;
	tx_ctx->ctx.atomic = NULL;

	*tx_ep = &tx_ctx->ctx;
	sock_ep->tx_array[index] = tx_ctx;
	atomic_inc(&sock_ep->num_tx_ctx);
	return 0;
}

int sock_rdm_ep_rx_ctx(struct fid_sep *sep, int index, struct fi_rx_ctx_attr *attr,
		       struct fid_ep **rx_ep, void *context)
{
	struct sock_ep *sock_ep;
	struct sock_rx_ctx *rx_ctx;

	sock_ep = container_of(sep, struct sock_ep, ep.fid);
	if (index >= sock_ep->ep_attr.rx_ctx_cnt)
		return -FI_EINVAL;

	rx_ctx = sock_rx_ctx_alloc(attr, context);
	if (!rx_ctx)
		return -FI_ENOMEM;

	rx_ctx->rx_id = index;
	rx_ctx->ep = sock_ep;
	rx_ctx->domain = sock_ep->domain;
	sock_rx_ctx_add_ep(rx_ctx, sock_ep);

	rx_ctx->ctx.ops = &sock_rdm_ctx_ep_ops;
	rx_ctx->ctx.msg = &sock_rdm_ctx_msg_ops;

	/* TODO */
	rx_ctx->ctx.rma = NULL;
	rx_ctx->ctx.tagged = NULL;
	rx_ctx->ctx.atomic = NULL;

	*rx_ep = &rx_ctx->ctx;
	sock_ep->rx_array[index] = rx_ctx;
	atomic_inc(&sock_ep->num_rx_ctx);
	return 0;
}

struct fi_ops_ep sock_rdm_ep_ops ={
	.size = sizeof(struct fi_ops_ep),
	.enable = sock_rdm_ep_enable,
	.cancel = fi_no_cancel,
	.getopt = sock_rdm_ep_getopt,
	.setopt = sock_rdm_ep_setopt,
	.tx_ctx = sock_rdm_ep_tx_ctx,
	.rx_ctx = sock_rdm_ep_rx_ctx,
};

int sock_rdm_ep_cm_getname(fid_t fid, void *addr, size_t *addrlen)
{
	struct sock_ep *sock_ep;
	if (*addrlen == 0) {
		*addrlen = sizeof(struct sockaddr_in);
		return -FI_ETOOSMALL;
	}

	sock_ep = container_of(fid, struct sock_ep, ep.fid);
	*addrlen = MIN(*addrlen, sizeof(struct sockaddr_in));
	memcpy(addr, sock_ep->src_addr, *addrlen);
	return 0;
}

int sock_rdm_ep_cm_getpeer(struct fid_ep *ep, void *addr, size_t *addrlen)
{
	struct sock_ep *sock_ep;

	if (*addrlen == 0) {
		*addrlen = sizeof(struct sockaddr_in);
		return -FI_ETOOSMALL;
	}

	sock_ep = container_of(ep, struct sock_ep, ep);
	*addrlen = MIN(*addrlen, sizeof(struct sockaddr));
	memcpy(addr, sock_ep->dest_addr, *addrlen);
	return 0;
}

int sock_rdm_ep_cm_connect(struct fid_ep *ep, const void *addr,
			   const void *param, size_t paramlen)
{
	struct sock_ep *sock_ep;

	sock_ep = container_of(ep, struct sock_ep, ep);
	if (sock_ep->info.addr_format == FI_SOCKADDR) {
		if (memcmp((void*)sock_ep->dest_addr,
			  addr, sizeof(struct sockaddr_in)) != 0) {
			memcpy(sock_ep->dest_addr, addr, sizeof(struct sockaddr));
		}
	} else {
		return -FI_EINVAL;
	}
	
	if (paramlen > 0) {
		int ret;
		struct iovec msg_iov ={
			.iov_base = (void*) param,
			.iov_len = paramlen,
		};
		
		struct msghdr msg = {
			.msg_name = NULL,
			.msg_namelen = 0,
			.msg_iov = &msg_iov,
			.msg_iovlen = 1,
			.msg_control = NULL,
			.msg_controllen = 0,
			.msg_flags = 0,
		};
		ret = sendmsg(sock_ep->sock_fd, &msg, 0);
		if (ret)
			return -FI_EINVAL;
	}
	sock_ep->enabled = 1;
	return 0;
}

struct fi_ops_cm sock_rdm_ep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = sock_rdm_ep_cm_getname,
	.getpeer = sock_rdm_ep_cm_getpeer,
	.connect = sock_rdm_ep_cm_connect,
	.listen = fi_no_listen,
	.accept = fi_no_accept,
	.reject = fi_no_reject,
	.shutdown = fi_no_shutdown,
	.join = fi_no_join,
	.leave = fi_no_leave,
};

ssize_t sock_rdm_ep_msg_recvmsg(struct fid_ep *ep, const struct fi_msg *msg,
				uint64_t flags)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_recvmsg(&sock_ep->rx_ctx->ctx,msg, flags);
}

ssize_t sock_rdm_ep_msg_recv(struct fid_ep *ep, void *buf, size_t len, 
			     void *desc, fi_addr_t src_addr, void *context)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_recv(&sock_ep->rx_ctx->ctx, buf, len, desc,
				 src_addr, context);
}

ssize_t sock_rdm_ep_msg_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
			      size_t count, fi_addr_t src_addr, void *context)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_recvv(&sock_ep->rx_ctx->ctx, iov, desc, 
				  count, src_addr, context);
}

ssize_t sock_rdm_ep_msg_sendmsg(struct fid_ep *ep, const struct fi_msg *msg,
				uint64_t flags)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_sendmsg(&sock_ep->tx_ctx->ctx, msg, flags);
}

ssize_t sock_rdm_ep_msg_send(struct fid_ep *ep, const void *buf, size_t len, 
			     void *desc, fi_addr_t dest_addr, void *context)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_send(&sock_ep->tx_ctx->ctx, buf, len, desc,
				   dest_addr, context);
}

ssize_t sock_rdm_ep_msg_sendv(struct fid_ep *ep, const struct iovec *iov, 
			      void **desc, size_t count, fi_addr_t dest_addr,
			      void *context)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_sendv(&sock_ep->tx_ctx->ctx, iov, desc,
				  count, dest_addr, context);
}


ssize_t sock_rdm_ep_msg_inject(struct fid_ep *ep, const void *buf, size_t len,
			       fi_addr_t dest_addr)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_inject(&sock_ep->tx_ctx->ctx, buf, len, dest_addr);
}

ssize_t sock_rdm_ep_msg_senddata(struct fid_ep *ep, const void *buf, size_t len, 
				 void *desc, uint64_t data, fi_addr_t dest_addr, 
				 void *context)
{
	struct sock_ep *sock_ep;
	sock_ep = container_of(ep, struct sock_ep, ep);
	return sock_rdm_ctx_senddata(&sock_ep->tx_ctx->ctx, buf, len, 
				     desc, data, dest_addr, context);
}

struct fi_ops_msg sock_rdm_ep_msg_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = sock_rdm_ep_msg_recv,
	.recvv = sock_rdm_ep_msg_recvv,
	.recvmsg = sock_rdm_ep_msg_recvmsg,
	.send = sock_rdm_ep_msg_send,
	.sendv = sock_rdm_ep_msg_sendv,
	.sendmsg = sock_rdm_ep_msg_sendmsg,
	.inject = sock_rdm_ep_msg_inject,
	.senddata = sock_rdm_ep_msg_senddata,
};

int sock_rdm_ep(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context)
{
	int ret;
	struct sock_ep *sock_ep;
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;
	struct sock_domain *sock_dom;

	if (info) {
		ret = sock_verify_info(info);
		if (ret) {
			SOCK_LOG_INFO(
				   "Cannot support requested options!\n");
			return -FI_EINVAL;
		}
	}
	
	sock_dom = container_of(domain, struct sock_domain, dom_fid);
	if (!sock_dom)
		return -FI_EINVAL;

	sock_ep = (struct sock_ep*)calloc(1, sizeof(*sock_ep));
	if (!sock_ep)
		return -FI_ENOMEM;

	atomic_init(&sock_ep->ref, 0);
	sock_ep->ep.fid.fclass = FI_CLASS_EP;
	sock_ep->ep.fid.context = context;	
	sock_ep->ep.fid.ops = &sock_rdm_ep_fi_ops;
	
	sock_ep->ep.ops = &sock_rdm_ep_ops;
	sock_ep->ep.cm = &sock_rdm_ep_cm_ops;
	sock_ep->ep.msg = &sock_rdm_ep_msg_ops;
	
	/* TODO */
	sock_ep->ep.rma = NULL;
	sock_ep->ep.tagged = NULL;
	sock_ep->ep.atomic = NULL;

	sock_ep->sock_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (sock_ep->sock_fd <0) {
		goto err;
	}

	*ep = &sock_ep->ep;	
	if (info) {
		sock_ep->info.caps = info->caps;
		sock_ep->info.addr_format = FI_SOCKADDR_IN;
		
		if (info->src_addr) {
			sock_ep->src_addr = calloc(1, sizeof(struct sockaddr_in));
			memcpy(sock_ep->src_addr, info->src_addr, 
			       sizeof(struct sockaddr_in));
		}

		if (info->dest_addr) {
			sock_ep->dest_addr = calloc(1, sizeof(struct sockaddr_in));
			memcpy(sock_ep->dest_addr, info->dest_addr, 
			       sizeof(struct sockaddr_in));
		}

		if (info->ep_attr) {
			ret = sock_rdm_verify_ep_attr(info->ep_attr, 
						      info->tx_attr, 
						      info->rx_attr);
			if (ret)
				goto err;
			sock_ep->ep_attr = *info->ep_attr;
		}

		if (info->tx_attr)
			sock_ep->tx_attr = *info->tx_attr;
		else
			sock_ep->tx_attr = sock_rdm_tx_attr;

		if (info->rx_attr)
			sock_ep->rx_attr = *info->rx_attr;
		else
			sock_ep->rx_attr = sock_rdm_rx_attr;
	} else {
		sock_ep->ep_attr = sock_rdm_ep_attr;
		sock_ep->tx_attr = sock_rdm_tx_attr;
		sock_ep->rx_attr = sock_rdm_rx_attr;
	}

	atomic_init(&sock_ep->ref, 0);
	atomic_init(&sock_ep->num_tx_ctx, 0);
	atomic_init(&sock_ep->num_rx_ctx, 0);

	sock_ep->tx_array = calloc(sock_ep->ep_attr.tx_ctx_cnt + 1, 
				 sizeof(struct sock_tx_ctx *));
	sock_ep->rx_array = calloc(sock_ep->ep_attr.rx_ctx_cnt + 1,
				 sizeof(struct sock_rx_ctx *));
	
	/* default tx ctx */
	tx_ctx = sock_tx_ctx_alloc(&sock_ep->tx_attr, context);
	tx_ctx->ep = sock_ep;
	tx_ctx->domain = sock_dom;
	tx_ctx->tx_id = sock_ep->ep_attr.tx_ctx_cnt;
	sock_tx_ctx_add_ep(tx_ctx, sock_ep);
	sock_ep->tx_array[sock_ep->ep_attr.tx_ctx_cnt] = tx_ctx;
	sock_ep->tx_ctx = tx_ctx;
	
	/* default rx_ctx */
	rx_ctx = sock_rx_ctx_alloc(&sock_ep->rx_attr, context);
	rx_ctx->ep = sock_ep;
	rx_ctx->domain = sock_dom;
	rx_ctx->rx_id = sock_ep->ep_attr.rx_ctx_cnt;
	sock_rx_ctx_add_ep(rx_ctx, sock_ep);
	sock_ep->rx_array[sock_ep->ep_attr.rx_ctx_cnt] = rx_ctx;
	sock_ep->rx_ctx = rx_ctx;
	
  	sock_ep->domain = sock_dom;
	atomic_inc(&sock_dom->ref);
	return 0;
	
err:
	free(sock_ep);
	return -FI_EAVAIL;
}

int sock_rdm_pep(struct fid_fabric *fabric, struct fi_info *info,
			struct fid_pep **pep, void *context)
{
	return -FI_EINVAL;
}

/* place holder */
int sock_pe_add_tx_ctx(struct sock_pe *pe, struct sock_tx_ctx *ctx)
{
	return -FI_ENOSYS;
}

int sock_pe_add_rx_ctx(struct sock_pe *pe, struct sock_rx_ctx *ctx)
{
	return -FI_ENOSYS;
}

