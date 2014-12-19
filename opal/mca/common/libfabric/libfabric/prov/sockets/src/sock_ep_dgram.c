/*
 * Copyright (c) 2014 Intel Corporation, Inc.  All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * OpenIB.org BSD license below:
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

#include "sock_util.h"
#include "sock.h"

const struct fi_ep_attr sock_dgram_ep_attr = {
	.protocol = FI_PROTO_SOCK_TCP,
	.max_msg_size = SOCK_EP_MAX_MSG_SZ,
	.inject_size = SOCK_EP_MAX_INJECT_SZ,
	.total_buffered_recv = SOCK_EP_MAX_BUFF_RECV,
	.max_order_raw_size = SOCK_EP_MAX_ORDER_RAW_SZ,
	.max_order_war_size = SOCK_EP_MAX_ORDER_WAR_SZ,
	.max_order_waw_size = SOCK_EP_MAX_ORDER_WAW_SZ,
	.mem_tag_format = SOCK_EP_MEM_TAG_FMT,
	.msg_order = SOCK_EP_MSG_ORDER,
	.tx_ctx_cnt = 0,
	.rx_ctx_cnt = 0,
};

const struct fi_tx_attr sock_dgram_tx_attr = {
	.caps = SOCK_EP_DGRAM_CAP,
	.op_flags = SOCK_DEF_OPS,
	.msg_order = SOCK_EP_MSG_ORDER,
	.inject_size = SOCK_EP_MAX_INJECT_SZ,
	.size = SOCK_EP_MAX_TX_CTX_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

const struct fi_rx_attr sock_dgram_rx_attr = {
	.caps = SOCK_EP_DGRAM_CAP,
	.op_flags = SOCK_DEF_OPS,
	.msg_order = SOCK_EP_MSG_ORDER,
	.total_buffered_recv = SOCK_EP_MAX_BUFF_RECV,
	.size = SOCK_EP_MAX_MSG_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

static int sock_dgram_verify_rx_attr(const struct fi_rx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | SOCK_EP_DGRAM_CAP) != SOCK_EP_DGRAM_CAP)
		return -FI_ENODATA;

	if ((attr->op_flags | SOCK_EP_DGRAM_CAP) != SOCK_EP_DGRAM_CAP)
		return -FI_ENODATA;

	if ((attr->msg_order | SOCK_EP_MSG_ORDER) != SOCK_EP_MSG_ORDER)
		return -FI_ENODATA;

	if (attr->total_buffered_recv > sock_dgram_rx_attr.total_buffered_recv)
		return -FI_ENODATA;

	if (attr->size > sock_dgram_rx_attr.size)
		return -FI_ENODATA;

	if (attr->iov_limit > sock_dgram_rx_attr.iov_limit)
		return -FI_ENODATA;

	return 0;
}

static int sock_dgram_verify_tx_attr(const struct fi_tx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | SOCK_EP_DGRAM_CAP) != SOCK_EP_DGRAM_CAP)
		return -FI_ENODATA;

	if ((attr->op_flags | SOCK_EP_DGRAM_CAP) != SOCK_EP_DGRAM_CAP)
		return -FI_ENODATA;

	if ((attr->msg_order | SOCK_EP_MSG_ORDER) != SOCK_EP_MSG_ORDER)
		return -FI_ENODATA;

	if (attr->inject_size > sock_dgram_tx_attr.inject_size)
		return -FI_ENODATA;

	if (attr->size > sock_dgram_tx_attr.size)
		return -FI_ENODATA;

	if (attr->iov_limit > sock_dgram_tx_attr.iov_limit)
		return -FI_ENODATA;

	return 0;
}

int sock_dgram_verify_ep_attr(struct fi_ep_attr *ep_attr,
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

		if (ep_attr->max_msg_size > sock_dgram_ep_attr.max_msg_size)
			return -FI_ENODATA;

		if (ep_attr->inject_size > sock_dgram_ep_attr.inject_size)
			return -FI_ENODATA;

		if (ep_attr->total_buffered_recv > 
		   sock_dgram_ep_attr.total_buffered_recv)
			return -FI_ENODATA;

		if (ep_attr->max_order_raw_size >
		   sock_dgram_ep_attr.max_order_raw_size)
			return -FI_ENODATA;

		if (ep_attr->max_order_war_size >
		   sock_dgram_ep_attr.max_order_war_size)
			return -FI_ENODATA;

		if (ep_attr->max_order_waw_size > 
		   sock_dgram_ep_attr.max_order_waw_size)
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

	if (sock_dgram_verify_tx_attr(tx_attr) || sock_dgram_verify_rx_attr(rx_attr))
		return -FI_ENODATA;

	return 0;
}

static struct fi_info *sock_dgram_fi_info(struct fi_info *hints, 
					  void *src_addr, void *dest_addr)
{
	struct fi_info *_info = sock_fi_info(FI_EP_DGRAM, hints, 
					     src_addr, dest_addr);
	if (!_info)
		return NULL;
	
	if (!hints->caps) 
		_info->caps = SOCK_EP_DGRAM_CAP;
	
	if (!hints->tx_attr)
		*(_info->tx_attr) = sock_dgram_tx_attr;

	if (!hints->rx_attr)
		*(_info->rx_attr) = sock_dgram_rx_attr;

	if (!hints->ep_attr)
		*(_info->ep_attr) = sock_dgram_ep_attr;

	return _info;
}

int sock_dgram_getinfo(uint32_t version, const char *node, const char *service,
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
		if ((SOCK_EP_DGRAM_CAP | hints->caps) != SOCK_EP_DGRAM_CAP) {
			SOCK_LOG_INFO(
				   "Cannot support requested options!\n");
			return -FI_ENODATA;
		}
		
		ret = sock_dgram_verify_rx_attr(hints->rx_attr);
		if (ret)
			return ret;

		ret = sock_dgram_verify_tx_attr(hints->tx_attr);
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

	_info = sock_dgram_fi_info(hints, src_addr, dest_addr);
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

int sock_dgram_endpoint(struct fid_domain *domain, struct fi_info *info,
		struct sock_ep **ep, void *context, size_t fclass)
{
	int ret;

	if (info) {
		if (info->ep_attr) {
			ret = sock_dgram_verify_ep_attr(info->ep_attr, 
						      info->tx_attr, 
						      info->rx_attr);
			if (ret)
				return ret;
		}
			
		if (info->tx_attr) {
			ret = sock_dgram_verify_tx_attr(info->tx_attr);
			if (ret)
				return ret;
		}
		
		if (info->rx_attr) {
			ret = sock_dgram_verify_rx_attr(info->rx_attr);
			if (ret)
				return ret;
		}
	}
	
	ret = sock_alloc_endpoint(domain, info, ep, context, fclass);
	if (ret)
		return ret;

	if (!info || !info->ep_attr) 
		(*ep)->ep_attr = sock_dgram_ep_attr;

	if (!info || !info->tx_attr)
		(*ep)->tx_attr = sock_dgram_tx_attr;

	if (!info || !info->rx_attr)
		(*ep)->rx_attr = sock_dgram_rx_attr;
	
	return 0;
}

int sock_dgram_ep(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context)
{
	int ret;
	struct sock_ep *endpoint;
	
	ret = sock_dgram_endpoint(domain, info, &endpoint, context, FI_CLASS_EP);
	if (ret)
		return ret;

	*ep = &endpoint->ep;
	return 0;
}

int sock_dgram_sep(struct fid_domain *domain, struct fi_info *info,
		struct fid_sep **sep, void *context)
{
	int ret;
	struct sock_ep *endpoint;
	
	ret = sock_dgram_endpoint(domain, info, &endpoint, context, FI_CLASS_SEP);
	if (ret)
		return ret;

	*sep = &endpoint->sep;
	return 0;
}
