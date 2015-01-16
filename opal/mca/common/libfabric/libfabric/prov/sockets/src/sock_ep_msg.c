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
#include <arpa/inet.h>
#include <limits.h>

#include "sock.h"
#include "sock_util.h"

const struct fi_ep_attr sock_msg_ep_attr = {
	.protocol = FI_PROTO_SOCK_TCP,
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

const struct fi_tx_attr sock_msg_tx_attr = {
	.caps = SOCK_EP_MSG_CAP,
	.op_flags = SOCK_DEF_OPS,
	.msg_order = SOCK_EP_MSG_ORDER,
	.inject_size = SOCK_EP_MAX_INJECT_SZ,
	.size = SOCK_EP_MAX_TX_CTX_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

const struct fi_rx_attr sock_msg_rx_attr = {
	.caps = SOCK_EP_MSG_CAP,
	.op_flags = SOCK_DEF_OPS,
	.msg_order = SOCK_EP_MSG_ORDER,
	.total_buffered_recv = SOCK_EP_MAX_BUFF_RECV,
	.size = SOCK_EP_MAX_MSG_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

static int sock_msg_verify_rx_attr(const struct fi_rx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | SOCK_EP_MSG_CAP) != SOCK_EP_MSG_CAP)
		return -FI_ENODATA;

	if ((attr->op_flags | SOCK_EP_MSG_CAP) != SOCK_EP_MSG_CAP)
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

	if ((attr->op_flags | SOCK_EP_MSG_CAP) != SOCK_EP_MSG_CAP)
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

		if (ep_attr->inject_size > sock_msg_ep_attr.inject_size)
			return -FI_ENODATA;

		if (ep_attr->total_buffered_recv > 
		   sock_msg_ep_attr.total_buffered_recv)
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

		if ((ep_attr->msg_order | SOCK_EP_MSG_ORDER) != SOCK_EP_MSG_ORDER)
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

static struct fi_info *sock_msg_fi_info(struct fi_info *hints, 
					  void *src_addr, void *dest_addr)
{
	struct fi_info *_info = sock_fi_info(FI_EP_MSG, hints, 
					     src_addr, dest_addr);
	if (!_info)
		return NULL;
	
	if (!hints->caps) 
		_info->caps = SOCK_EP_MSG_CAP;
	
	if (!hints->tx_attr)
		*(_info->tx_attr) = sock_msg_tx_attr;

	if (!hints->rx_attr)
		*(_info->rx_attr) = sock_msg_rx_attr;

	if (!hints->ep_attr)
		*(_info->ep_attr) = sock_msg_ep_attr;

	return _info;
}

int sock_msg_getinfo(uint32_t version, const char *node, const char *service,
		     uint64_t flags, struct fi_info *hints, struct fi_info **info)
{
	int ret;
	int udp_sock;
	socklen_t len;
	struct fi_info *_info;
	struct addrinfo sock_hints;
	struct addrinfo *result = NULL;
	struct sockaddr_in *src_addr = NULL, *dest_addr = NULL;
	char sa_ip[INET_ADDRSTRLEN];
	char hostname[HOST_NAME_MAX];

	if (!info)
		return -FI_EBADFLAGS;

	*info = NULL;
	
	if (!node && !service && !hints)
		return -FI_EBADFLAGS;

	if (version != FI_VERSION(SOCK_MAJOR_VERSION, 
				 SOCK_MINOR_VERSION))
		return -FI_ENODATA;

	if (hints) {
		if ((SOCK_EP_MSG_CAP | hints->caps) != SOCK_EP_MSG_CAP) {
			SOCK_LOG_INFO(
				   "Cannot support requested options!\n");
			return -FI_ENODATA;
		}
		
		ret = sock_msg_verify_rx_attr(hints->rx_attr);
		if (ret)
			return ret;

		ret = sock_msg_verify_tx_attr(hints->tx_attr);
		if (ret)
			return ret;
	}

	src_addr = calloc(1, sizeof(struct sockaddr_in));
	dest_addr = calloc(1, sizeof(struct sockaddr_in));

	memset(&sock_hints, 0, sizeof(struct addrinfo));
	sock_hints.ai_family = AF_INET;
	sock_hints.ai_socktype = SOCK_STREAM;

	if (flags & FI_NUMERICHOST)
		sock_hints.ai_flags |= AI_NUMERICHOST;

	if ((flags & FI_SOURCE) || !node) {

		if (!node) {
			gethostname(hostname, HOST_NAME_MAX);
		}

		ret = getaddrinfo(node ? node : hostname, service, 
				  &sock_hints, &result);
		if (ret != 0) {
			ret = FI_ENODATA;
			SOCK_LOG_INFO("getaddrinfo failed!\n");
			goto err;
		}

		while (result) {
			if (result->ai_family == AF_INET && 
			    result->ai_addrlen == sizeof(struct sockaddr_in))
				break;
			result = result->ai_next;
		}

		if (!result) {
			SOCK_LOG_ERROR("getaddrinfo failed\n");
			ret = -FI_EINVAL;
			goto err;
		}
		
		memcpy(src_addr, result->ai_addr, result->ai_addrlen);
		freeaddrinfo(result); 
	} else if (node || service) {

		ret = getaddrinfo(node, service, &sock_hints, &result);
		if (ret != 0) {
			ret = FI_ENODATA;
			SOCK_LOG_INFO("getaddrinfo failed!\n");
			goto err;
		}
		
		while (result) {
			if (result->ai_family == AF_INET && 
			    result->ai_addrlen == sizeof(struct sockaddr_in))
				break;
			result = result->ai_next;
		}

		if (!result) {
			SOCK_LOG_ERROR("getaddrinfo failed\n");
			ret = -FI_EINVAL;
			goto err;
		}
		
		memcpy(dest_addr, result->ai_addr, result->ai_addrlen);
		
		udp_sock = socket(AF_INET, SOCK_DGRAM, 0);
		ret = connect(udp_sock, result->ai_addr, 
			      result->ai_addrlen);
		if ( ret != 0) {
			SOCK_LOG_ERROR("Failed to create udp socket\n");
			ret = FI_ENODATA;
			goto err;
		}

		len = sizeof(struct sockaddr_in);				
		ret = getsockname(udp_sock, (struct sockaddr*)src_addr, &len);
		if (ret != 0) {
			SOCK_LOG_ERROR("getsockname failed\n");
			close(udp_sock);
			ret = FI_ENODATA;
			goto err;
		}
		close(udp_sock);
		freeaddrinfo(result); 
	}

	if (hints->src_addr) {
		assert(hints->src_addrlen == sizeof(struct sockaddr_in));
		memcpy(src_addr, hints->src_addr, hints->src_addrlen);
	}

	if (hints->dest_addr) {
		assert(hints->dest_addrlen == sizeof(struct sockaddr_in));
		memcpy(dest_addr, hints->dest_addr, hints->dest_addrlen);
	}

	if (dest_addr) {
		memcpy(sa_ip, inet_ntoa(dest_addr->sin_addr), INET_ADDRSTRLEN);
		SOCK_LOG_INFO("dest_addr: family: %d, IP is %s\n",
			      ((struct sockaddr_in*)dest_addr)->sin_family, sa_ip);
	}
	
	if (src_addr) {
		memcpy(sa_ip, inet_ntoa(src_addr->sin_addr), INET_ADDRSTRLEN);
		SOCK_LOG_INFO("src_addr: family: %d, IP is %s\n",
			      ((struct sockaddr_in*)src_addr)->sin_family, sa_ip);
	}

	_info = sock_msg_fi_info(hints, src_addr, dest_addr);
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

static int sock_ep_cm_getname(fid_t fid, void *addr, size_t *addrlen)
{
	struct sock_ep *sock_ep;
	if (*addrlen == 0) {
		*addrlen = sizeof(struct sockaddr_in);
		return -FI_ETOOSMALL;
	}

	sock_ep = container_of(fid, struct sock_ep, fid.ep.fid);
	*addrlen = MIN(*addrlen, sizeof(struct sockaddr_in));
	memcpy(addr, sock_ep->src_addr, *addrlen);
	return 0;
}

static int sock_ep_cm_getpeer(struct fid_ep *ep, void *addr, size_t *addrlen)
{
	struct sock_ep *sock_ep;

	if (*addrlen == 0) {
		*addrlen = sizeof(struct sockaddr_in);
		return -FI_ETOOSMALL;
	}

	sock_ep = container_of(ep, struct sock_ep, fid.ep);
	*addrlen = MIN(*addrlen, sizeof(struct sockaddr_in));
	memcpy(addr, sock_ep->dest_addr, *addrlen);
	return 0;
}

static int sock_ep_cm_connect(struct fid_ep *ep, const void *addr,
			   const void *param, size_t paramlen)
{
	struct sock_conn_req req;
	struct sock_ep *_ep;
	struct sock_eq *_eq;
	_ep = container_of(ep, struct sock_ep, fid.ep);
	_eq = _ep->eq;
	if (!_eq) {
		SOCK_LOG_ERROR("no EQ bound with this ep\n");
		return -FI_EINVAL;
	}

	if(((struct sockaddr *)addr)->sa_family != AF_INET) {
		SOCK_LOG_ERROR("invalid address type to connect: only IPv4 supported\n");
		return -FI_EINVAL;
	}

	req.type = SOCK_CONNREQ;
	req.c_fid = &ep->fid;
	req.s_fid = 0;
	memcpy(&req.info, &_ep->info, sizeof(struct fi_info));
	memcpy(&req.src_addr, _ep->info.src_addr, sizeof(struct sockaddr_in));
	memcpy(&req.dest_addr, _ep->info.dest_addr, sizeof(struct sockaddr_in));
	memcpy(&req.tx_attr, _ep->info.tx_attr, sizeof(struct fi_tx_attr));
	memcpy(&req.rx_attr, _ep->info.rx_attr, sizeof(struct fi_rx_attr));
	memcpy(&req.ep_attr, _ep->info.ep_attr, sizeof(struct fi_ep_attr));
	memcpy(&req.domain_attr, _ep->info.domain_attr, sizeof(struct fi_domain_attr));
	memcpy(&req.fabric_attr, _ep->info.fabric_attr, sizeof(struct fi_fabric_attr));

	if (sock_util_sendto(_eq->wait_fd, &req, sizeof(struct sock_conn_req),
				(struct sockaddr_in *)addr, sizeof(struct sockaddr_in), 0))
		return -errno;

	return 0;
}

static int sock_ep_cm_accept(struct fid_ep *ep, const void *param, size_t paramlen)
{
	struct sock_conn_req *req;
	struct sock_domain *_dom;
	struct sockaddr_in *addr;
	socklen_t addrlen;
	struct sock_ep *_ep;
	struct sock_eq *_eq;

	_ep = container_of(ep, struct sock_ep, fid.ep);
	_eq = _ep->eq;
	if (!_eq) {
		SOCK_LOG_ERROR("no EQ bound with this ep\n");
		return -FI_EINVAL;
	}

	_dom = _ep->domain;
	addr = _dom->info.dest_addr;
	addrlen = _dom->info.dest_addrlen;
	req = (struct sock_conn_req *)_dom->info.connreq;
	if (!req) {
		SOCK_LOG_ERROR("invalid connreq for cm_accept\n");
		return -FI_EINVAL;
	}

	if (((struct sockaddr *)addr)->sa_family != AF_INET) {
		SOCK_LOG_ERROR("invalid address type to connect: only IPv4 supported\n");
		return -FI_EINVAL;
	}

	req->type = SOCK_ACCEPT;
	req->s_fid = &ep->fid;

	if (sock_util_sendto(_eq->wait_fd, req, sizeof(req->type) +
				sizeof(req->c_fid) + sizeof(req->s_fid), addr, addrlen, 0))
		return -errno;

	free(req);
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
	.shutdown = fi_no_shutdown,
};

int sock_msg_endpoint(struct fid_domain *domain, struct fi_info *info,
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
	
	*ep = &endpoint->fid.ep;
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
	if ((eq->attr.wait_obj == FI_WAIT_FD) && (eq->wait_fd < 0))
		sock_eq_openwait(eq, (char *)&pep->service);

	return 0;
}

static int sock_pep_fi_close(fid_t fid)
{
	struct sock_pep *pep;

	pep = container_of(fid, struct sock_pep, pep.fid);
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

static int sock_pep_listen(struct fid_pep *pep)
{
	return 0;
}

static int sock_pep_reject(struct fid_pep *pep, fi_connreq_t connreq,
		const void *param, size_t paramlen)
{
	struct sock_conn_req *req;
	struct sockaddr_in *addr;
	socklen_t addrlen;
	struct sock_pep *_pep;
	struct sock_eq *_eq;

	_pep = container_of(pep, struct sock_pep, pep);
	_eq = _pep->eq;
	if (!_eq) {
		SOCK_LOG_ERROR("no EQ bound with this pep\n");
		return -FI_EINVAL;
	}

	req = (struct sock_conn_req *)connreq;
	if (!req) {
		SOCK_LOG_ERROR("invalid connreq for cm_accept\n");
		return -FI_EINVAL;
	}
	addr = &req->src_addr;
	addrlen = sizeof(struct sockaddr_in);
	if (((struct sockaddr *)addr)->sa_family != AF_INET) {
		SOCK_LOG_ERROR("invalid address type to connect: only IPv4 supported\n");
		return -FI_EINVAL;
	}

	req->type = SOCK_REJECT;
	req->s_fid = NULL;

	if (sock_util_sendto(_eq->wait_fd, req, sizeof(req->type) +
				sizeof(req->c_fid), addr, addrlen, 0))
		return -errno;

	free(req);
	return 0;
}

static struct fi_ops_cm sock_pep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = fi_no_getname,
	.getpeer = fi_no_getpeer,
	.connect = fi_no_connect,
	.listen = sock_pep_listen,
	.accept = fi_no_accept,
	.reject = sock_pep_reject,
	.shutdown = fi_no_shutdown,
};

int sock_msg_sep(struct fid_domain *domain, struct fi_info *info,
		 struct fid_sep **sep, void *context)
{
	int ret;
	struct sock_ep *endpoint;
	
	ret = sock_msg_endpoint(domain, info, &endpoint, context, FI_CLASS_SEP);
	if (ret)
		return ret;
	
	*sep = &endpoint->fid.sep;
	return 0;
}

int sock_msg_pep(struct fid_fabric *fabric, struct fi_info *info,
			struct fid_pep **pep, void *context)
{
	int ret;
	struct sock_ep *endpoint;

	ret = sock_msg_endpoint(NULL, info, &endpoint, context, FI_CLASS_PEP);
	if (ret)
		return ret;
	
	*pep = &endpoint->fid.pep;
	return 0;
}

int sock_msg_passive_ep(struct fid_fabric *fabric, struct fi_info *info,
		    struct fid_pep **pep, void *context)
{
	struct sock_pep *_pep;
	int ret;

	if (info) {
		ret = sock_verify_info(info);
		if (ret) {
			SOCK_LOG_INFO("Cannot support requested options!\n");
			return -FI_EINVAL;
		}
	}

	_pep = (struct sock_pep*)calloc(1, sizeof(*_pep));
	if (!_pep)
		return -FI_ENOMEM;

	if(info) {
		struct sockaddr *dest_addr = (struct sockaddr *)info->dest_addr;
		struct sockaddr *src_addr = (struct sockaddr *)info->src_addr;
		if (!dest_addr || !src_addr) {
			SOCK_LOG_ERROR("invalid dest_addr or src_addr\n");
			goto err;
		}

		if (!dest_addr->sa_family) {
			if(getnameinfo(src_addr, sizeof(*src_addr), NULL, 0,
				       _pep->service, 
				       sizeof(_pep->service),
				       NI_NUMERICSERV)) {
				SOCK_LOG_ERROR("could not resolve src_addr\n");
				goto err;
			}
		} else {
			if(getnameinfo(dest_addr, sizeof(*dest_addr), NULL, 0,
				       _pep->service, 
				       sizeof(_pep->service),
				       NI_NUMERICSERV)) {
				SOCK_LOG_ERROR("could not resolve dest_addr\n");
				goto err;
			}
		}
		_pep->info = *info;
	} else {
		SOCK_LOG_ERROR("invalid fi_info\n");
		goto err;
	}
	
	_pep->pep.fid.fclass = FI_CLASS_PEP;
	_pep->pep.fid.context = context;
	_pep->pep.fid.ops = &sock_pep_fi_ops;
	_pep->pep.cm = &sock_pep_cm_ops;
	_pep->pep.ops = NULL;

	_pep->sock_fab = container_of(fabric, struct sock_fabric, fab_fid);
	*pep = &_pep->pep;

	return 0;

err:
	free(_pep);
	return -errno;
}

struct fi_info * sock_ep_msg_process_info(struct sock_conn_req *req)
{
	req->info.src_addr = &req->src_addr;
	req->info.dest_addr = &req->dest_addr;
	req->info.tx_attr = &req->tx_attr;
	req->info.rx_attr = &req->rx_attr;
	req->info.ep_attr = &req->ep_attr;
	req->info.domain_attr = &req->domain_attr;
	req->info.fabric_attr = &req->fabric_attr;
	if (sock_verify_info(&req->info)) {
		SOCK_LOG_INFO("incoming conn_req not supported\n");
		errno = EINVAL;
		return NULL;
	}

	/* reverse src_addr and dest_addr */
	return sock_fi_info(FI_EP_MSG, &req->info, 
			req->info.dest_addr, req->info.src_addr);
}
