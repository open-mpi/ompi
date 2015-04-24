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

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_EP_CTRL, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_EP_CTRL, __VA_ARGS__)

const struct fi_ep_attr sock_rdm_ep_attr = {
	.type = FI_EP_RDM,
	.protocol = FI_PROTO_SOCK_TCP,
	.max_msg_size = SOCK_EP_MAX_MSG_SZ,
	.max_order_raw_size = SOCK_EP_MAX_ORDER_RAW_SZ,
	.max_order_war_size = SOCK_EP_MAX_ORDER_WAR_SZ,
	.max_order_waw_size = SOCK_EP_MAX_ORDER_WAW_SZ,
	.mem_tag_format = SOCK_EP_MEM_TAG_FMT,
	.tx_ctx_cnt = SOCK_EP_MAX_TX_CNT,
	.rx_ctx_cnt = SOCK_EP_MAX_RX_CNT,
};

const struct fi_tx_attr sock_rdm_tx_attr = {
	.caps = SOCK_EP_RDM_CAP,
	.op_flags = FI_TRANSMIT_COMPLETE,
	.msg_order = SOCK_EP_MSG_ORDER,
	.inject_size = SOCK_EP_MAX_INJECT_SZ,
	.size = SOCK_EP_TX_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

const struct fi_rx_attr sock_rdm_rx_attr = {
	.caps = SOCK_EP_RDM_CAP,
	.op_flags = 0,
	.msg_order = SOCK_EP_MSG_ORDER,
	.total_buffered_recv = SOCK_EP_MAX_BUFF_RECV,
	.size = SOCK_EP_RX_SZ,
	.iov_limit = SOCK_EP_MAX_IOV_LIMIT,
};

static int sock_rdm_verify_rx_attr(const struct fi_rx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | SOCK_EP_RDM_CAP) != SOCK_EP_RDM_CAP) {
		SOCK_LOG_INFO("Unsupported RDM rx caps\n");
		return -FI_ENODATA;
	}

	if ((attr->msg_order | SOCK_EP_MSG_ORDER) != SOCK_EP_MSG_ORDER) {
		SOCK_LOG_INFO("Unsuported rx message order\n");
		return -FI_ENODATA;
	}

	if (attr->total_buffered_recv > sock_rdm_rx_attr.total_buffered_recv) {
		SOCK_LOG_INFO("Buffered receive size too large\n");
		return -FI_ENODATA;
	}

	if (attr->size > sock_rdm_rx_attr.size) {
		SOCK_LOG_INFO("Rx size too large\n");
		return -FI_ENODATA;
	}

	if (attr->iov_limit > sock_rdm_rx_attr.iov_limit) {
		SOCK_LOG_INFO("Rx iov limit too large\n");
		return -FI_ENODATA;
	}

	return 0;
}

static int sock_rdm_verify_tx_attr(const struct fi_tx_attr *attr)
{
	if (!attr)
		return 0;

	if ((attr->caps | SOCK_EP_RDM_CAP) != SOCK_EP_RDM_CAP) {
		SOCK_LOG_INFO("Unsupported RDM tx caps\n");
		return -FI_ENODATA;
	}

	if ((attr->msg_order | SOCK_EP_MSG_ORDER) != SOCK_EP_MSG_ORDER) {
		SOCK_LOG_INFO("Unsupported tx message order\n");
		return -FI_ENODATA;
	}

	if (attr->inject_size > sock_rdm_tx_attr.inject_size) {
		SOCK_LOG_INFO("Inject size too large\n");
		return -FI_ENODATA;
	}

	if (attr->size > sock_rdm_tx_attr.size) {
		SOCK_LOG_INFO("Tx size too large\n");
		return -FI_ENODATA;
	}

	if (attr->iov_limit > sock_rdm_tx_attr.iov_limit) {
		SOCK_LOG_INFO("Tx iov limit too large\n");
		return -FI_ENODATA;
	}

	return 0;
}

int sock_rdm_verify_ep_attr(struct fi_ep_attr *ep_attr,
			    struct fi_tx_attr *tx_attr,
			    struct fi_rx_attr *rx_attr)
{
	int ret;

	if (ep_attr) {
		switch (ep_attr->protocol) {
		case FI_PROTO_UNSPEC:
		case FI_PROTO_SOCK_TCP:
			break;
		default:
			SOCK_LOG_INFO("Unsupported protocol\n");
			return -FI_ENODATA;
		}

		if (ep_attr->max_msg_size > sock_rdm_ep_attr.max_msg_size) {
			SOCK_LOG_INFO("Message size too large\n");
			return -FI_ENODATA;
		}

		if (ep_attr->max_order_raw_size >
		   sock_rdm_ep_attr.max_order_raw_size) {
			SOCK_LOG_INFO("RAW order size too large\n");
			return -FI_ENODATA;
		}

		if (ep_attr->max_order_war_size >
		   sock_rdm_ep_attr.max_order_war_size) {
			SOCK_LOG_INFO("WAR order size too large\n");
			return -FI_ENODATA;
		}

		if (ep_attr->max_order_waw_size > 
		   sock_rdm_ep_attr.max_order_waw_size) {
			SOCK_LOG_INFO("WAW order size too large\n");
			return -FI_ENODATA;
		}

		if ((ep_attr->tx_ctx_cnt > SOCK_EP_MAX_TX_CNT) &&
		    ep_attr->tx_ctx_cnt != FI_SHARED_CONTEXT)
			return -FI_ENODATA;

		if ((ep_attr->rx_ctx_cnt > SOCK_EP_MAX_RX_CNT) &&
		    ep_attr->rx_ctx_cnt != FI_SHARED_CONTEXT)
			return -FI_ENODATA;
	}

	ret = sock_rdm_verify_tx_attr(tx_attr);
	if (ret)
		return ret;

	ret = sock_rdm_verify_rx_attr(rx_attr);
	if (ret)
		return ret;

	return 0;
}

int sock_rdm_fi_info(void *src_addr, void *dest_addr, struct fi_info *hints,
		     struct fi_info **info)
{
	*info = sock_fi_info(FI_EP_RDM, hints, src_addr, dest_addr);
	if (!*info)
		return -FI_ENOMEM;
	
	*(*info)->tx_attr = sock_rdm_tx_attr;
	*(*info)->rx_attr = sock_rdm_rx_attr;
	*(*info)->ep_attr = sock_rdm_ep_attr;

	(*info)->caps = SOCK_EP_RDM_CAP |
			(*info)->rx_attr->caps | (*info)->tx_attr->caps;
	return 0;
}

static int sock_rdm_endpoint(struct fid_domain *domain, struct fi_info *info,
		struct sock_ep **ep, void *context, size_t fclass)
{
	int ret;

	if (info) {
		if (info->ep_attr) {
			ret = sock_rdm_verify_ep_attr(info->ep_attr, 
						      info->tx_attr, 
						      info->rx_attr);
			if (ret)
				return ret;
		}
			
		if (info->tx_attr) {
			ret = sock_rdm_verify_tx_attr(info->tx_attr);
			if (ret)
				return ret;
		}
		
		if (info->rx_attr) {
			ret = sock_rdm_verify_rx_attr(info->rx_attr);
			if (ret)
				return ret;
		}
	}
	
	ret = sock_alloc_endpoint(domain, info, ep, context, fclass);
	if (ret)
		return ret;

	if (!info || !info->ep_attr) 
		(*ep)->ep_attr = sock_rdm_ep_attr;

	if (!info || !info->tx_attr)
		(*ep)->tx_attr = sock_rdm_tx_attr;

	if (!info || !info->rx_attr)
		(*ep)->rx_attr = sock_rdm_rx_attr;
	
	return 0;
}

int sock_rdm_ep(struct fid_domain *domain, struct fi_info *info,
		struct fid_ep **ep, void *context)
{
	int ret;
	struct sock_ep *endpoint;
	
	ret = sock_rdm_endpoint(domain, info, &endpoint, context, FI_CLASS_EP);
	if (ret)
		return ret;

	*ep = &endpoint->ep;
	return 0;
}

int sock_rdm_sep(struct fid_domain *domain, struct fi_info *info,
		 struct fid_ep **sep, void *context)
{
	int ret;
	struct sock_ep *endpoint;
	
	ret = sock_rdm_endpoint(domain, info, &endpoint, context, FI_CLASS_SEP);
	if (ret)
		return ret;

	*sep = &endpoint->ep;
	return 0;
}

