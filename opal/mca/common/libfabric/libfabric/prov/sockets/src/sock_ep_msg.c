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

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_CTRL, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_CTRL, __VA_ARGS__)

static const struct fi_ep_attr sock_msg_ep_attr = {
	.type = FI_EP_MSG,
	.protocol = FI_PROTO_SOCK_TCP,
	.max_msg_size = SOCK_EP_MAX_MSG_SZ,
	.max_order_raw_size = SOCK_EP_MAX_ORDER_RAW_SZ,
	.max_order_war_size = SOCK_EP_MAX_ORDER_WAR_SZ,
	.max_order_waw_size = SOCK_EP_MAX_ORDER_WAW_SZ,
	.mem_tag_format = SOCK_EP_MEM_TAG_FMT,
	.tx_ctx_cnt = SOCK_EP_MAX_TX_CNT,
	.rx_ctx_cnt = SOCK_EP_MAX_RX_CNT,
};

static const struct fi_tx_attr sock_msg_tx_attr = {
	.caps = SOCK_EP_MSG_CAP,
	.op_flags = FI_TRANSMIT_COMPLETE,
	.msg_order = SOCK_EP_MSG_ORDER,
	.inject_size = SOCK_EP_MAX_INJECT_SZ,
	.size = SOCK_EP_TX_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

static const struct fi_rx_attr sock_msg_rx_attr = {
	.caps = SOCK_EP_MSG_CAP,
	.op_flags = 0,
	.msg_order = SOCK_EP_MSG_ORDER,
	.total_buffered_recv = SOCK_EP_MAX_BUFF_RECV,
	.size = SOCK_EP_RX_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

static int sock_msg_verify_rx_attr(const struct fi_rx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | SOCK_EP_MSG_CAP) != SOCK_EP_MSG_CAP)
		return -FI_ENODATA;

	if ((attr->msg_order | SOCK_EP_MSG_ORDER) != SOCK_EP_MSG_ORDER)
		return -FI_ENODATA;

	if (attr->total_buffered_recv > sock_msg_rx_attr.total_buffered_recv)
		return -FI_ENODATA;

	if (attr->size > sock_msg_rx_attr.size)
		return -FI_ENODATA;

	if (attr->iov_limit > sock_msg_rx_attr.iov_limit)
		return -FI_ENODATA;

	return 0;
}

static int sock_msg_verify_tx_attr(const struct fi_tx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | SOCK_EP_MSG_CAP) != SOCK_EP_MSG_CAP)
		return -FI_ENODATA;

	if ((attr->msg_order | SOCK_EP_MSG_ORDER) != SOCK_EP_MSG_ORDER)
		return -FI_ENODATA;

	if (attr->inject_size > sock_msg_tx_attr.inject_size)
		return -FI_ENODATA;

	if (attr->size > sock_msg_tx_attr.size)
		return -FI_ENODATA;

	if (attr->iov_limit > sock_msg_tx_attr.iov_limit)
		return -FI_ENODATA;

	return 0;
}

int sock_msg_verify_ep_attr(struct fi_ep_attr *ep_attr,
			    struct fi_tx_attr *tx_attr,
			    struct fi_rx_attr *rx_attr)
{
	if (ep_attr) {
		switch (ep_attr->protocol) {
		case FI_PROTO_UNSPEC:
		case FI_PROTO_SOCK_TCP:
			break;
		default:
			return -FI_ENODATA;
		}

		if (ep_attr->max_msg_size > sock_msg_ep_attr.max_msg_size)
			return -FI_ENODATA;

		if (ep_attr->max_order_raw_size >
		   sock_msg_ep_attr.max_order_raw_size)
			return -FI_ENODATA;

		if (ep_attr->max_order_war_size >
		   sock_msg_ep_attr.max_order_war_size)
			return -FI_ENODATA;

		if (ep_attr->max_order_waw_size > 
		   sock_msg_ep_attr.max_order_waw_size)
			return -FI_ENODATA;

		if ((ep_attr->tx_ctx_cnt > SOCK_EP_MAX_TX_CNT) &&
		    ep_attr->tx_ctx_cnt != FI_SHARED_CONTEXT)
			return -FI_ENODATA;

		if ((ep_attr->rx_ctx_cnt > SOCK_EP_MAX_RX_CNT) &&
		    ep_attr->rx_ctx_cnt != FI_SHARED_CONTEXT)
			return -FI_ENODATA;
	}

	if (sock_msg_verify_tx_attr(tx_attr) || sock_msg_verify_rx_attr(rx_attr))
		return -FI_ENODATA;

	return 0;
}

int sock_msg_fi_info(void *src_addr, void *dest_addr, struct fi_info *hints,
		     struct fi_info **info)
{
	*info = sock_fi_info(FI_EP_MSG, hints, src_addr, dest_addr);
	if (!*info)
		return -FI_ENOMEM;
	
	*(*info)->tx_attr = sock_msg_tx_attr;
	*(*info)->rx_attr = sock_msg_rx_attr;
	*(*info)->ep_attr = sock_msg_ep_attr;

	(*info)->caps = SOCK_EP_MSG_CAP |
			(*info)->rx_attr->caps | (*info)->tx_attr->caps;
	return 0;
}

static int sock_ep_cm_getname(fid_t fid, void *addr, size_t *addrlen)
{
	struct sock_ep *sock_ep = NULL;
	struct sock_pep *sock_pep = NULL;

	if (*addrlen == 0) {
		*addrlen = sizeof(struct sockaddr_in);
		return -FI_ETOOSMALL;
	}

	*addrlen = MIN(*addrlen, sizeof(struct sockaddr_in));

	switch(fid->fclass) {
	case FI_CLASS_EP:
	case FI_CLASS_SEP:
		sock_ep = container_of(fid, struct sock_ep, ep.fid);
		memcpy(addr, sock_ep->src_addr, *addrlen);
		break;
	case FI_CLASS_PEP:
		sock_pep = container_of(fid, struct sock_pep, pep.fid);
		memcpy(addr, &sock_pep->src_addr, *addrlen);
		break;
	default:
		SOCK_LOG_ERROR("Invalid argument\n");
		return -FI_EINVAL;
	}
	return 0;
}

static int sock_ep_cm_getpeer(struct fid_ep *ep, void *addr, size_t *addrlen)
{
	struct sock_ep *sock_ep;

	if (*addrlen == 0) {
		*addrlen = sizeof(struct sockaddr_in);
		return -FI_ETOOSMALL;
	}

	sock_ep = container_of(ep, struct sock_ep, ep);
	*addrlen = MIN(*addrlen, sizeof(struct sockaddr_in));
	memcpy(addr, sock_ep->dest_addr, *addrlen);
	return 0;
}

static int sock_ep_cm_create_socket(void)
{
	int sock, optval;
	sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0)
		return 0;
	
	optval = 1;
	if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, 
		       &optval, sizeof optval))
		SOCK_LOG_ERROR("setsockopt failed\n");
	return sock;
}

static int sock_ep_cm_enqueue_msg(struct sock_cm_entry *cm,
				  const struct sockaddr_in *addr, 
				  void *msg, size_t len, 
				  fid_t fid, struct sock_eq *eq)
{
	char c = 0;
	int ret = 0;
	struct sock_cm_msg_list_entry *list_entry;

	list_entry = calloc(1, sizeof(*list_entry) + len);
	if (!list_entry)
		return -FI_ENOMEM;

	list_entry->msg_len = len;
	memcpy(&list_entry->msg[0], msg, len);
	memcpy(&list_entry->addr, addr, sizeof(*addr));
	list_entry->fid = fid;
	list_entry->eq = eq;

	fastlock_acquire(&cm->lock);
	dlist_insert_tail(&list_entry->entry, &cm->msg_list);
	fastlock_release(&cm->lock);

	ret = write(cm->signal_fds[0], &c, 1);
	if (ret != 1) {
		SOCK_LOG_INFO("failed to signal\n");
		ret = -FI_EIO;
	} else {
		ret = 0;
		SOCK_LOG_INFO("Enqueued CM Msg\n");
	}
	return ret;
}

static int sock_ep_cm_send_msg(struct sock_cm_entry *cm, 
			       const struct sockaddr_in *addr, 
			       void *msg, size_t len)
{
	int ret;
	char sa_ip[INET_ADDRSTRLEN] = {0};
	
	memcpy(sa_ip, inet_ntoa(addr->sin_addr), INET_ADDRSTRLEN);
	SOCK_LOG_INFO("Sending message to %s:%d\n", sa_ip, ntohs(addr->sin_port));

	ret = sendto(cm->sock, (char *) msg, len, 0,
		     (struct sockaddr *) addr, sizeof *addr);
	SOCK_LOG_INFO("Total Sent: %d\n", ret);
	return (ret == len) ? 0 : -1;
}

static void sock_ep_cm_release_entry(struct sock_cm_msg_list_entry *msg_entry)
{
	struct sock_conn_hdr *msg_hdr;
	struct fi_eq_cm_entry cm_entry;
	struct sock_ep *sock_ep;

	msg_hdr = (struct sock_conn_hdr*)msg_entry->msg;
	if (msg_hdr->type == SOCK_CONN_SHUTDOWN) {
		memset(&cm_entry, 0, sizeof cm_entry);
		cm_entry.fid = msg_entry->fid;
		sock_ep = container_of(cm_entry.fid, struct sock_ep, ep.fid);
		if (sock_eq_report_event(msg_entry->eq, FI_SHUTDOWN, &cm_entry,
					 sizeof(cm_entry), 0))
			SOCK_LOG_ERROR("Error in writing to EQ\n");
		sock_ep->cm.shutdown_received = 1;
		sock_ep_disable(&sock_ep->ep);
	} else {
		if (sock_eq_report_error(msg_entry->eq, msg_entry->fid, NULL,
					 0, FI_ETIMEDOUT, -FI_ETIMEDOUT, NULL, 0))
			SOCK_LOG_ERROR("failed to report error\n");
	}

	dlist_remove(&msg_entry->entry);
	free(msg_entry);
}

static void sock_ep_cm_flush_msg(struct sock_cm_entry *cm)
{
	struct dlist_entry *entry, *next_entry;
	struct sock_cm_msg_list_entry *msg_entry;
	fastlock_acquire(&cm->lock);
	for (entry = cm->msg_list.next; entry != &cm->msg_list;) {
		msg_entry = container_of(entry, 
					 struct sock_cm_msg_list_entry, entry);
		next_entry = entry->next;

		if (msg_entry->timestamp_ms != 0 &&
		    fi_gettime_ms() - msg_entry->timestamp_ms < SOCK_CM_COMM_TIMEOUT) {
			entry = next_entry;
			continue;
		}
		
		msg_entry->timestamp_ms = fi_gettime_ms();
		msg_entry->retry++;

		if (msg_entry->retry > SOCK_EP_MAX_RETRY) {
			sock_ep_cm_release_entry(msg_entry);
			entry = next_entry;
			continue;
		}
		
		if (sock_ep_cm_send_msg(cm, &msg_entry->addr, 
					&msg_entry->msg, msg_entry->msg_len))
			SOCK_LOG_INFO("Failed to send out cm message\n");
		entry = next_entry;
	}
	fastlock_release(&cm->lock);
}

static int sock_ep_cm_send_ack(struct sock_cm_entry *cm, struct sockaddr_in *addr,
			       uint64_t msg_id)
{
	int ret;
	struct sock_conn_response conn_response;
	
	memset(&conn_response, 0, sizeof(conn_response));
	conn_response.hdr.type = SOCK_CONN_ACK;
	conn_response.hdr.msg_id = msg_id;

	ret = sendto(cm->sock, &conn_response, sizeof conn_response, 0,
		     (struct sockaddr *) addr, sizeof *addr);
	SOCK_LOG_INFO("Total Sent: %d\n", ret);
	sock_ep_cm_flush_msg(cm);
	return (ret == sizeof conn_response) ? 0 : -1;
}

static void sock_ep_cm_handle_ack(struct sock_cm_entry *cm,
				  struct sock_conn_hdr *hdr)
{
	struct sock_conn_hdr *msg_hdr;
	struct dlist_entry *entry;
	struct sock_cm_msg_list_entry *msg_entry;
	struct fi_eq_cm_entry cm_entry;
	struct sock_ep *sock_ep;

	sock_ep = container_of(cm, struct sock_ep, cm);
	fastlock_acquire(&cm->lock);
	for (entry = cm->msg_list.next; entry != &cm->msg_list;) {

		msg_entry = container_of(entry, struct sock_cm_msg_list_entry, 
					 entry);
		msg_hdr = (struct sock_conn_hdr*)msg_entry->msg;

		if (msg_hdr->msg_id == hdr->msg_id) {
			if (msg_hdr->type == SOCK_CONN_SHUTDOWN) {
				memset(&cm_entry, 0, sizeof cm_entry);
				cm_entry.fid = &sock_ep->ep.fid;
				if (sock_ep->cm.shutdown_received)
					break;

				if (sock_eq_report_event(sock_ep->eq, FI_SHUTDOWN, &cm_entry,
							 sizeof(cm_entry), 0))
					SOCK_LOG_ERROR("Error in writing to EQ\n");
			}
			dlist_remove(entry);
			free(msg_entry);
			break;
		}
		entry = entry->next;
	}
	fastlock_release(&cm->lock);
}


static void *sock_msg_ep_listener_thread(void *data)
{
	struct pollfd poll_fds[2];
	struct sock_ep *ep = (struct sock_ep*)data;
	struct sock_conn_response *conn_response;
	struct fi_eq_cm_entry *cm_entry;

	struct sockaddr_in from_addr;
	socklen_t addr_len;
	int ret, user_data_sz, entry_sz, timeout;
	char tmp = 0;

	ep->cm.sock = sock_ep_cm_create_socket();
	if (!ep->cm.sock) {
		SOCK_LOG_ERROR("Cannot open socket\n");
		return NULL;
	}

	SOCK_LOG_INFO("Starting listener thread for EP: %p\n", ep);
	conn_response = calloc(1, sizeof(*conn_response) + SOCK_EP_MAX_CM_DATA_SZ);
	if (!conn_response) {
		SOCK_LOG_ERROR("cannot allocate\n");
		return NULL;
	}

	cm_entry = calloc(1, sizeof(*cm_entry) + SOCK_EP_MAX_CM_DATA_SZ);
	if (!cm_entry) {
		free (conn_response);
		SOCK_LOG_ERROR("cannot allocate\n");
		return NULL;
	}

	poll_fds[0].fd = ep->cm.sock;
	poll_fds[1].fd = ep->cm.signal_fds[1];
	poll_fds[0].events = poll_fds[1].events = POLLIN;

	while (*((volatile int*) &ep->cm.do_listen)) {
		timeout = dlist_empty(&ep->cm.msg_list) ? -1 : SOCK_CM_COMM_TIMEOUT;
		if ((ret = poll(poll_fds, 2, timeout)) > 0) {
			if (poll_fds[1].revents & POLLIN) {
				ret = read(ep->cm.signal_fds[1], &tmp, 1);
				if (ret != 1) {
					SOCK_LOG_INFO("Invalid signal\n");
					break;
				}
				sock_ep_cm_flush_msg(&ep->cm);
				continue;
			}
		} else {
			if (ret == 0) {
				sock_ep_cm_flush_msg(&ep->cm);
				continue;
			} else {
				break;
			}
		}

		addr_len = sizeof(from_addr);
		ret = recvfrom(ep->cm.sock, (char*) conn_response,
			       sizeof(*conn_response) + SOCK_EP_MAX_CM_DATA_SZ,
			       0, (struct sockaddr *) &from_addr, &addr_len);
		if (ret <= 0)
			continue;
		
		SOCK_LOG_INFO("Total received: %d\n", ret);
		
		if (ret < sizeof(*conn_response))
			continue;
		
		if (conn_response->hdr.type != SOCK_CONN_ACK) 
			sock_ep_cm_send_ack(&ep->cm, &from_addr, conn_response->hdr.msg_id);

		user_data_sz = ret - sizeof(*conn_response);
		switch (conn_response->hdr.type) {

		case SOCK_CONN_ACK:
			SOCK_LOG_INFO("Received SOCK_CONN_ACK\n");
			sock_ep_cm_handle_ack(&ep->cm, &conn_response->hdr);
			break;

		case SOCK_CONN_ACCEPT:
			SOCK_LOG_INFO("Received SOCK_CONN_ACCEPT\n");

			entry_sz = sizeof(*cm_entry) + user_data_sz;
			memset(cm_entry, 0, sizeof *cm_entry);
			cm_entry->fid = &ep->ep.fid;

			memcpy(&ep->cm_addr, &from_addr, sizeof(from_addr));
			memcpy(&cm_entry->data, &conn_response->user_data,
			       user_data_sz);

			if (ep->is_disabled || ep->cm.shutdown_received)
				break;

			ep->connected = 1;
			((struct sockaddr_in*) ep->dest_addr)->sin_port =
				conn_response->hdr.s_port;

			sock_ep_enable(&ep->ep);
			if (sock_eq_report_event(ep->eq, FI_CONNECTED, cm_entry,
						 entry_sz, 0))
				SOCK_LOG_ERROR("Error in writing to EQ\n");
			break;
		case SOCK_CONN_REJECT:
			SOCK_LOG_INFO("Received SOCK_CONN_REJECT\n");
			
			if (ep->is_disabled || ep->cm.shutdown_received)
				break;

			if (sock_eq_report_error(ep->eq, &ep->ep.fid, NULL, 0,
						 FI_ECONNREFUSED, -FI_ECONNREFUSED,
						 &conn_response->user_data, 
						 user_data_sz))
				SOCK_LOG_ERROR("Error in writing to EQ\n");
			goto out;

		case SOCK_CONN_SHUTDOWN:
			SOCK_LOG_INFO("Received SOCK_CONN_SHUTDOWN\n");

			entry_sz = sizeof(*cm_entry);
			memset(cm_entry, 0, sizeof *cm_entry);
			cm_entry->fid = &ep->ep.fid;

			if (ep->cm.shutdown_received)
				break;

			sock_ep_disable(&ep->ep);
			ep->cm.shutdown_received = 1;
			if (sock_eq_report_event(ep->eq, FI_SHUTDOWN, cm_entry,
						 entry_sz, 0))
				SOCK_LOG_ERROR("Error in writing to EQ\n");
			goto out;

		default:
			SOCK_LOG_ERROR("Invalid event: %d\n", conn_response->hdr.type);
			break;
		}
	}

out:
	free(conn_response);
	free(cm_entry);
	close(ep->cm.sock);
	ep->cm.listener_thread = 0L;
	return NULL;
}

static int sock_ep_cm_connect(struct fid_ep *ep, const void *addr,
			   const void *param, size_t paramlen)
{
	struct sock_conn_req *req;
	struct sock_ep *_ep;
	struct sock_eq *_eq;
	int ret = 0;

	_ep = container_of(ep, struct sock_ep, ep);
	_eq = _ep->eq;
	if (!_eq || paramlen > SOCK_EP_MAX_CM_DATA_SZ) 
		return -FI_EINVAL;

	req = calloc(1, sizeof(*req) + paramlen);
	if (!req)
		return -FI_ENOMEM;

	((struct sockaddr *) addr)->sa_family = AF_INET;

	req->hdr.type = SOCK_CONN_REQ;
	req->hdr.msg_id = _ep->cm.next_msg_id++;
	req->info = _ep->info;
	memcpy(&req->src_addr, _ep->src_addr, sizeof(req->src_addr));
	memcpy(&req->dest_addr, _ep->info.dest_addr, sizeof(req->dest_addr));
	req->tx_attr = *_ep->info.tx_attr;
	req->rx_attr = *_ep->info.rx_attr;
	req->ep_attr = *_ep->info.ep_attr;
	req->domain_attr = *_ep->info.domain_attr;
	req->fabric_attr = *_ep->info.fabric_attr;

	if (param && paramlen)
		memcpy(&req->user_data, param, paramlen);
	
	memcpy(&_ep->cm_addr, addr, sizeof(struct sockaddr_in));
	if (sock_ep_cm_enqueue_msg(&_ep->cm, addr, req, 
				   sizeof (*req) + paramlen, 
				   &_ep->ep.fid, _eq)) {
		ret = -FI_EIO;
		goto err;
	}

	free (req);
	return 0;

err:
	free(req);
	return ret;
}

static int sock_ep_cm_accept(struct fid_ep *ep, const void *param, size_t paramlen)
{
	struct sock_conn_req *req;
	struct fi_eq_cm_entry cm_entry;
	struct sock_conn_response *response;
	struct sockaddr_in *addr;
	struct sock_ep *_ep;
	struct sock_eq *_eq;
	int ret = 0;

	_ep = container_of(ep, struct sock_ep, ep);
	_eq = _ep->eq;
	if (!_eq || paramlen > SOCK_EP_MAX_CM_DATA_SZ) 
		return -FI_EINVAL;

	if (_ep->is_disabled || _ep->cm.shutdown_received)
		return -FI_EINVAL;

	response = calloc(1, sizeof(*response) + paramlen);
	if (!response)
		return -FI_ENOMEM;

	req = (struct sock_conn_req *) _ep->info.connreq;
	if (!req) {
		SOCK_LOG_ERROR("invalid connreq for cm_accept\n");
		free(response);
		return -FI_EINVAL;
	}
	
	memcpy(&response->hdr, &req->hdr, sizeof(response->hdr));
	if (param && paramlen)
		memcpy(&response->user_data, param, paramlen);

	addr = &req->from_addr;
	memcpy(&_ep->cm_addr, addr, sizeof(*addr));

	response->hdr.type = SOCK_CONN_ACCEPT;
	req->hdr.msg_id = _ep->cm.next_msg_id++;
	response->hdr.s_port = htons(atoi(_ep->listener.service));

	if (sock_ep_cm_enqueue_msg(&_ep->cm, addr, response, 
				   sizeof (*response) + paramlen,
				   &_ep->ep.fid, _ep->eq)) {
		ret = -FI_EIO;
		goto out;
	}
	    
	sock_ep_enable(ep);
	memset(&cm_entry, 0, sizeof(cm_entry));
	cm_entry.fid = &ep->fid;
	_ep->connected = 1;
	ret = sock_eq_report_event(_eq, FI_CONNECTED, &cm_entry, 
				   sizeof(cm_entry), 0);
out:
	free(req);
	free(response);
	_ep->info.connreq = NULL;
	return ret;
}

static int sock_ep_cm_shutdown(struct fid_ep *ep, uint64_t flags)
{
	struct sock_conn_response response;
	struct sock_ep *_ep;

	_ep = container_of(ep, struct sock_ep, ep);
	memset(&response, 0, sizeof(response));

	response.hdr.type = SOCK_CONN_SHUTDOWN;
	response.hdr.msg_id = _ep->cm.next_msg_id++;

	if (sock_ep_cm_enqueue_msg(&_ep->cm, &_ep->cm_addr, &response, 
				   sizeof response, &_ep->ep.fid, _ep->eq)) {
		return -FI_EIO;
	}

	sock_ep_disable(ep);
	return 0;
}

struct fi_ops_cm sock_ep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = sock_ep_cm_getname,
	.getpeer = sock_ep_cm_getpeer,
	.connect = sock_ep_cm_connect,
	.listen = fi_no_listen,
	.accept = sock_ep_cm_accept,
	.reject = fi_no_reject,
	.shutdown = sock_ep_cm_shutdown,
};

static int sock_msg_endpoint(struct fid_domain *domain, struct fi_info *info,
		struct sock_ep **ep, void *context, size_t fclass)
{
	int ret;

	if (info) {
		if (info->ep_attr) {
			ret = sock_msg_verify_ep_attr(info->ep_attr,
						      info->tx_attr,
						      info->rx_attr);
			if (ret)
				return ret;
		}

		if (info->tx_attr) {
			ret = sock_msg_verify_tx_attr(info->tx_attr);
			if (ret)
				return ret;
		}

		if (info->rx_attr) {
			ret = sock_msg_verify_rx_attr(info->rx_attr);
			if (ret)
				return ret;
		}
	}
	
	ret = sock_alloc_endpoint(domain, info, ep, context, fclass);
	if (ret)
		return ret;

	if (!info || !info->ep_attr) 
		(*ep)->ep_attr = sock_msg_ep_attr;

	if (!info || !info->tx_attr)
		(*ep)->tx_attr = sock_msg_tx_attr;

	if (!info || !info->rx_attr)
		(*ep)->rx_attr = sock_msg_rx_attr;
	
	return 0;
}

int sock_msg_ep(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context)
{
	int ret;
	struct sock_ep *endpoint;
	
	ret = sock_msg_endpoint(domain, info, &endpoint, context, FI_CLASS_EP);
	if (ret)
		return ret;

	endpoint->cm.do_listen = 1;	
	if (pthread_create(&endpoint->cm.listener_thread, NULL, 
			   sock_msg_ep_listener_thread, endpoint)) {
		SOCK_LOG_ERROR("Couldn't create listener thread\n");
		return -FI_EINVAL;
	}
		
	*ep = &endpoint->ep;
	return 0;
}

static int sock_pep_fi_bind(fid_t fid, struct fid *bfid, uint64_t flags)
{
	struct sock_pep *pep;
	struct sock_eq *eq;

	pep = container_of(fid, struct sock_pep, pep.fid);

	if (bfid->fclass != FI_CLASS_EQ)
		return -FI_EINVAL;

	eq = container_of(bfid, struct sock_eq, eq.fid);
	if (pep->sock_fab != eq->sock_fab) {
		SOCK_LOG_ERROR("Cannot bind Passive EP and EQ on different fabric\n");
		return -FI_EINVAL;
	}
	pep->eq = eq;
	return 0;
}

static int sock_pep_fi_close(fid_t fid)
{
	int ret;
	char c = 0;
	struct sock_pep *pep;

	pep = container_of(fid, struct sock_pep, pep.fid);
	pep->cm.do_listen = 0;
	ret = write(pep->cm.signal_fds[0], &c, 1);
	if (ret != 1) 
		SOCK_LOG_INFO("Failed to signal\n");

	if (pep->cm.listener_thread && 
	    pthread_join(pep->cm.listener_thread, NULL)) {
		SOCK_LOG_INFO("pthread join failed\n");
	}

	close(pep->cm.signal_fds[0]);
	close(pep->cm.signal_fds[1]);
	fastlock_destroy(&pep->cm.lock);

	free(pep);
	return 0;
}

static struct fi_ops sock_pep_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_pep_fi_close,
	.bind = sock_pep_fi_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_info * sock_ep_msg_process_info(struct sock_conn_req *req)
{
	req->info.src_addr = &req->src_addr;
	req->info.dest_addr = &req->dest_addr;
	req->info.tx_attr = &req->tx_attr;
	req->info.rx_attr = &req->rx_attr;
	req->info.ep_attr = &req->ep_attr;
	req->info.domain_attr = &req->domain_attr;
	req->info.fabric_attr = &req->fabric_attr;
	req->info.domain_attr->name = NULL;
	req->info.fabric_attr->name = NULL;
	req->info.fabric_attr->prov_name = NULL;
	if (sock_verify_info(&req->info)) {
		SOCK_LOG_INFO("incoming conn_req not supported\n");
		errno = EINVAL;
		return NULL;
	}

	return sock_fi_info(FI_EP_MSG, &req->info, 
			    req->info.dest_addr, req->info.src_addr);
}

static void *sock_pep_listener_thread (void *data)
{
	struct sock_pep *pep = (struct sock_pep *)data;
	struct sock_conn_req *conn_req = NULL;
	struct fi_eq_cm_entry *cm_entry;
	struct sockaddr_in from_addr;
	struct pollfd poll_fds[2];

	socklen_t addr_len;
	int ret = 0, user_data_sz, entry_sz, timeout;
	char tmp = 0;

	SOCK_LOG_INFO("Starting listener thread for PEP: %p\n", pep);
	cm_entry = calloc(1, sizeof(*cm_entry) + SOCK_EP_MAX_CM_DATA_SZ);
	if (!cm_entry) {
		SOCK_LOG_ERROR("cannot allocate\n");
		return NULL;
	}

	poll_fds[0].fd = pep->cm.sock;
	poll_fds[1].fd = pep->cm.signal_fds[1];
	poll_fds[0].events = poll_fds[1].events = POLLIN;
	while(*((volatile int*)&pep->cm.do_listen)) {
		timeout = dlist_empty(&pep->cm.msg_list) ? -1 : SOCK_CM_COMM_TIMEOUT;
		if (poll(poll_fds, 2, timeout) > 0) {
			if (poll_fds[1].revents & POLLIN) {
				ret = read(pep->cm.signal_fds[1], &tmp, 1);
				if (ret != 1)
					SOCK_LOG_INFO("Invalid signal\n");
				sock_ep_cm_flush_msg(&pep->cm);
				continue;
			}
		} else {
			if (ret == 0) {
				sock_ep_cm_flush_msg(&pep->cm);
				continue;
			} else {
				break;
			}
		}

		if (conn_req == NULL) {
			conn_req = calloc(1, sizeof(*conn_req) + SOCK_EP_MAX_CM_DATA_SZ);
			if (!conn_req) {
				SOCK_LOG_ERROR("cannot allocate\n");
				break;
			}
		}

		addr_len = sizeof(struct sockaddr_in);
		ret = recvfrom(pep->cm.sock, (char*)conn_req, 
			       sizeof(*conn_req) + SOCK_EP_MAX_CM_DATA_SZ, 0, 
			       (struct sockaddr *) &from_addr, &addr_len);
		SOCK_LOG_INFO("Total received: %d\n", ret);

		if (ret <= 0)
			continue;
		memcpy(&conn_req->from_addr, &from_addr, sizeof(struct sockaddr_in));
		SOCK_LOG_INFO("CM msg received: %d\n", ret);
		memset(cm_entry, 0, sizeof *cm_entry);

		if (conn_req->hdr.type != SOCK_CONN_ACK)
			sock_ep_cm_send_ack(&pep->cm, &from_addr, conn_req->hdr.msg_id);
		
		switch (conn_req->hdr.type) {
		case SOCK_CONN_ACK:
			SOCK_LOG_INFO("Received SOCK_CONN_ACK\n");
			sock_ep_cm_handle_ack(&pep->cm, &conn_req->hdr);
			break;

		case SOCK_CONN_REQ:
			SOCK_LOG_INFO("Received SOCK_CONN_REQ\n");

			user_data_sz = ret - sizeof(*conn_req);
			entry_sz = sizeof(*cm_entry) + user_data_sz;

			if (ret < sizeof(*conn_req)) {
				SOCK_LOG_ERROR("Invalid connection request\n");
				break;
			}
			
			cm_entry->fid = &pep->pep.fid;
			cm_entry->info = sock_ep_msg_process_info(conn_req);
			cm_entry->info->connreq = (fi_connreq_t) conn_req;

			memcpy(&cm_entry->data, &conn_req->user_data,
			       user_data_sz);
			conn_req = NULL;
			
			if (sock_eq_report_event(pep->eq, FI_CONNREQ, cm_entry,
						 entry_sz, 0))
				SOCK_LOG_ERROR("Error in writing to EQ\n");
			break;

		default:
			SOCK_LOG_ERROR("Invalid event: %d\n", conn_req->hdr.type);
			goto out;
		}
	}

out:
	if (conn_req)
		free(conn_req);
	free(cm_entry);
	close(pep->cm.sock);
	return NULL;
}

static int sock_pep_create_listener_thread(struct sock_pep *pep)
{
	int optval, ret;
	socklen_t addr_size;
	struct sockaddr_in addr;
	struct addrinfo *s_res = NULL, *p;
	struct addrinfo hints;
	char sa_ip[INET_ADDRSTRLEN] = {0};
	char sa_port[NI_MAXSERV] = {0};

	pep->cm.do_listen = 1;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_DGRAM;
	hints.ai_flags = AI_PASSIVE;
	hints.ai_protocol = IPPROTO_UDP;

	memcpy(sa_ip, inet_ntoa(pep->src_addr.sin_addr), INET_ADDRSTRLEN);
	sprintf(sa_port, "%d", ntohs(pep->src_addr.sin_port));

	ret = getaddrinfo(sa_ip, sa_port, &hints, &s_res);
	if (ret) {
		SOCK_LOG_ERROR("no available AF_INET address service:%s, %s\n",
			       sa_port, gai_strerror(ret));
		return -FI_EINVAL;
	}

	for (p=s_res; p; p=p->ai_next) {
		pep->cm.sock = socket(p->ai_family, p->ai_socktype,
				     p->ai_protocol);
		if (pep->cm.sock >= 0) {
			optval = 1;
			if (setsockopt(pep->cm.sock, SOL_SOCKET, SO_REUSEADDR, &optval, 
				       sizeof optval))
				SOCK_LOG_ERROR("setsockopt failed\n");
			
			if (!bind(pep->cm.sock, s_res->ai_addr, s_res->ai_addrlen))
				break;
			close(pep->cm.sock);
			pep->cm.sock = -1;
		}
	}

	freeaddrinfo(s_res);
	if (pep->cm.sock < 0)
		return -FI_EIO;
	
	optval = 1;
	if (setsockopt(pep->cm.sock, SOL_SOCKET, SO_REUSEADDR, &optval, 
		       sizeof optval))
		SOCK_LOG_ERROR("setsockopt failed\n");
	
	if (pep->src_addr.sin_port == 0) {
		addr_size = sizeof(addr);
		if (getsockname(pep->cm.sock, (struct sockaddr*)&addr, &addr_size))
			return -FI_EINVAL;
		pep->src_addr.sin_port = addr.sin_port;
	}
	
	SOCK_LOG_INFO("Listener thread bound to %s:%d\n",
		      sa_ip, ntohs(pep->src_addr.sin_port));
	
	if (pthread_create(&pep->cm.listener_thread, NULL, 
			   sock_pep_listener_thread, (void *)pep)) {
		SOCK_LOG_ERROR("Couldn't create listener thread\n");
		return -FI_EINVAL;
	}
	return 0;
}

static int sock_pep_listen(struct fid_pep *pep)
{
	struct sock_pep *_pep;
	_pep = container_of(pep, struct sock_pep, pep);
	return sock_pep_create_listener_thread(_pep);
}

static int sock_pep_reject(struct fid_pep *pep, fi_connreq_t connreq,
		const void *param, size_t paramlen)
{
	struct sock_conn_req *req;
	struct sockaddr_in *addr;
	struct sock_pep *_pep;
	struct sock_conn_response *response;
	int ret = 0;

	_pep = container_of(pep, struct sock_pep, pep);
	req = (struct sock_conn_req *)connreq;
	if (!req)
		return 0;
	
	response = (struct sock_conn_response*)
		calloc(1, sizeof(*response) + paramlen);
	if (!response)
		return -FI_ENOMEM;

	memcpy(&response->hdr, &req->hdr, sizeof(struct sock_conn_hdr));
	if (param && paramlen)
		memcpy(&response->user_data, param, paramlen);
	
	addr = &req->from_addr;
	response->hdr.type = SOCK_CONN_REJECT;
	req->hdr.msg_id = _pep->cm.next_msg_id++;

	if (sock_ep_cm_enqueue_msg(&_pep->cm, addr, req, 
				   sizeof(struct sock_conn_response),
				   &_pep->pep.fid, _pep->eq)) {
		ret = -FI_EIO;
		goto out;
	}
	ret = 0;

out:	
	free(req);
	free(response);
	return ret;
}

static struct fi_ops_cm sock_pep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = sock_ep_cm_getname,
	.getpeer = fi_no_getpeer,
	.connect = fi_no_connect,
	.listen = sock_pep_listen,
	.accept = fi_no_accept,
	.reject = sock_pep_reject,
	.shutdown = fi_no_shutdown,
};

int sock_msg_sep(struct fid_domain *domain, struct fi_info *info,
		 struct fid_ep **sep, void *context)
{
	int ret;
	struct sock_ep *endpoint;
	
	ret = sock_msg_endpoint(domain, info, &endpoint, context, FI_CLASS_SEP);
	if (ret)
		return ret;
	
	*sep = &endpoint->ep;
	return 0;
}

int sock_msg_passive_ep(struct fid_fabric *fabric, struct fi_info *info,
			struct fid_pep **pep, void *context)
{
	int ret = 0;
	struct sock_pep *_pep;
	struct addrinfo hints, *result;

	if (info) {
		ret = sock_verify_info(info);
		if (ret) {
			SOCK_LOG_INFO("Cannot support requested options!\n");
			return ret;
		}
	}

	_pep = calloc(1, sizeof(*_pep));
	if (!_pep)
		return -FI_ENOMEM;

	if (info) {
		if (info->src_addr) {
			memcpy(&_pep->src_addr, info->src_addr, info->src_addrlen);
		} else {
			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_INET;
			hints.ai_socktype = SOCK_STREAM;

			ret = getaddrinfo("localhost", NULL, &hints, &result);
			if (ret) {
				ret = -FI_EINVAL;
				SOCK_LOG_INFO("getaddrinfo failed!\n");
				goto err;
			}
			memcpy(&_pep->src_addr, result->ai_addr, result->ai_addrlen);
			freeaddrinfo(result);
		}
		_pep->info = *info;
	} else {
		SOCK_LOG_ERROR("invalid fi_info\n");
		goto err;
	}

	ret = socketpair(AF_UNIX, SOCK_STREAM, 0, _pep->cm.signal_fds);
	if (ret) {
		ret = -errno;
		goto err;
	}

	fd_set_nonblock(_pep->cm.signal_fds[1]);
	dlist_init(&_pep->cm.msg_list);

	_pep->pep.fid.fclass = FI_CLASS_PEP;
	_pep->pep.fid.context = context;
	_pep->pep.fid.ops = &sock_pep_fi_ops;
	_pep->pep.cm = &sock_pep_cm_ops;
	_pep->pep.ops = NULL;
	fastlock_init(&_pep->cm.lock);

	_pep->sock_fab = container_of(fabric, struct sock_fabric, fab_fid);
	*pep = &_pep->pep;
	return 0;
err:
	free(_pep);
	return ret;
}

