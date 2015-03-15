/*
 * Copyright (c) 2013-2015 Intel Corporation, Inc.  All rights reserved.
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

#include <asm/types.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <infiniband/ib.h>
#include <infiniband/verbs.h>
#include <rdma/rdma_cma.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>

#include "fi.h"
#include "fi_enosys.h"
#include "fi_log.h"
#include "prov.h"
#include "fi_log.h"

#define VERBS_PROV_NAME "verbs"
#define VERBS_PROV_VERS FI_VERSION(1,0)

#define VERBS_WARN(...) FI_WARN(VERBS_PROV_NAME, __VA_ARGS__)
#define VERBS_INFO(...) FI_LOG(2, VERBS_PROV_NAME, __VA_ARGS__)
#define VERBS_DEBUG(...) FI_DEBUG(VERBS_PROV_NAME, __VA_ARGS__)

#define VERBS_MSG_SIZE (1ULL << 31)
#define VERBS_IB_PREFIX "IB-0x"
#define VERBS_IWARP_FABRIC "Ethernet-iWARP"
#define VERBS_ANY_FABRIC "Any RDMA fabric"
#define VERBS_CM_DATA_SIZE 56

#define VERBS_CAPS (FI_MSG | FI_RMA | FI_ATOMICS | FI_READ | FI_WRITE | \
		FI_SEND | FI_RECV | FI_REMOTE_READ | FI_REMOTE_WRITE | \
		FI_REMOTE_SIGNAL)
#define VERBS_MODE (FI_LOCAL_MR | FI_PROV_MR_ATTR)
#define VERBS_MSG_ORDER (FI_ORDER_RAR | FI_ORDER_RAW | FI_ORDER_RAS | \
		FI_ORDER_WAW | FI_ORDER_WAS | FI_ORDER_SAW | FI_ORDER_SAS )

struct fi_ibv_fabric {
	struct fid_fabric	fabric_fid;
};

struct fi_ibv_eq {
	struct fid_eq		eq_fid;
	struct fi_ibv_fabric	*fab;
	struct rdma_event_channel *channel;
	uint64_t		flags;
	struct fi_eq_err_entry	err;
};

struct fi_ibv_pep {
	struct fid_pep		pep_fid;
	struct fi_ibv_eq	*eq;
	struct rdma_cm_id	*id;
};

struct fi_ibv_domain {
	struct fid_domain	domain_fid;
	struct ibv_context	*verbs;
	struct ibv_pd		*pd;
};

struct fi_ibv_cq {
	struct fid_cq		cq_fid;
	struct fi_ibv_domain	*domain;
	struct ibv_comp_channel	*channel;
	struct ibv_cq		*cq;
	size_t			entry_size;
	uint64_t		flags;
	enum fi_cq_wait_cond	wait_cond;
	struct ibv_wc		wc;
};

struct fi_ibv_mem_desc {
	struct fid_mr		mr_fid;
	struct ibv_mr		*mr;
	struct fi_ibv_domain	*domain;
};

struct fi_ibv_msg_ep {
	struct fid_ep		ep_fid;
	struct rdma_cm_id	*id;
	struct fi_ibv_eq	*eq;
	struct fi_ibv_cq	*rcq;
	struct fi_ibv_cq	*scq;
	uint32_t		inline_size;
};

static const char *local_node = "localhost";
static char def_send_wr[16] = "384";
static char def_recv_wr[16] = "384";
static char def_send_sge[16] = "4";
static char def_recv_sge[16] = "4";
static char def_inline_data[16] = "64";

const struct fi_fabric_attr verbs_fabric_attr = {
	.name			= VERBS_PROV_NAME,
	.prov_version		= VERBS_PROV_VERS,
};

const struct fi_domain_attr verbs_domain_attr = {
	.threading		= FI_THREAD_SAFE,
	.control_progress	= FI_PROGRESS_AUTO,
	.data_progress		= FI_PROGRESS_AUTO,
	.mr_key_size		= sizeof_field(struct ibv_sge, lkey),
	.cq_data_size		= sizeof_field(struct ibv_send_wr, imm_data),
	.max_ep_tx_ctx		= 1,
	.max_ep_rx_ctx		= 1,
};

const struct fi_ep_attr verbs_ep_attr = {
	.type			= FI_EP_MSG,
	.protocol_version	= 1,
	.max_msg_size		= VERBS_MSG_SIZE,
	.msg_prefix_size	= 0,
	.max_order_raw_size	= VERBS_MSG_SIZE,
	.max_order_war_size	= 0,
	.max_order_waw_size	= VERBS_MSG_SIZE,
	.mem_tag_format		= 0,
	.msg_order		= VERBS_MSG_ORDER,
	.tx_ctx_cnt		= 1,
	.rx_ctx_cnt		= 1,
};

const struct fi_rx_attr verbs_rx_attr = {
	.caps			= VERBS_CAPS,
	.mode			= VERBS_MODE,
	.msg_order		= VERBS_MSG_ORDER,
	.total_buffered_recv	= 0,
	.size			= 256,
	.iov_limit		= 8,
};

const struct fi_tx_attr verbs_tx_attr = {
	.caps			= VERBS_CAPS,
	.mode			= VERBS_MODE,
	.msg_order		= VERBS_MSG_ORDER,
	.inject_size		= 0,
	.size			= 256,
	.iov_limit		= 8,
};

static int fi_ibv_sockaddr_len(struct sockaddr *addr)
{
	if (!addr)
		return 0;

	switch (addr->sa_family) {
	case AF_INET:
		return sizeof(struct sockaddr_in);
	case AF_INET6:
		return sizeof(struct sockaddr_in6);
	case AF_IB:
		return sizeof(struct sockaddr_ib);
	default:
		return 0;
	}
}

static int fi_ibv_check_fabric_attr(struct fi_fabric_attr *attr)
{
	if (attr->name && !(!strcmp(attr->name, VERBS_ANY_FABRIC) ||
	    !strncmp(attr->name, VERBS_IB_PREFIX, strlen(VERBS_IB_PREFIX)) ||
	    !strcmp(attr->name, VERBS_IWARP_FABRIC))) {
		VERBS_INFO("Unknown fabric name\n");
		return -FI_ENODATA;
	}

	if (attr->prov_version > VERBS_PROV_VERS) {
		VERBS_INFO("Unsupported provider version\n");
		return -FI_ENODATA;
	}

	return 0;
}

static int fi_ibv_check_domain_attr(struct fi_domain_attr *attr)
{
	switch (attr->threading) {
	case FI_THREAD_UNSPEC:
	case FI_THREAD_SAFE:
	case FI_THREAD_FID:
	case FI_THREAD_DOMAIN:
	case FI_THREAD_COMPLETION:
		break;
	default:
		VERBS_INFO("Invalid threading model\n");
		return -FI_ENODATA;
	}

	switch (attr->control_progress) {
	case FI_PROGRESS_UNSPEC:
	case FI_PROGRESS_AUTO:
	case FI_PROGRESS_MANUAL:
		break;
	default:
		VERBS_INFO("Given control progress mode not supported\n");
		return -FI_ENODATA;
	}

	switch (attr->data_progress) {
	case FI_PROGRESS_UNSPEC:
	case FI_PROGRESS_AUTO:
	case FI_PROGRESS_MANUAL:
		break;
	default:
		VERBS_INFO("Given data progress mode not supported!\n");
		return -FI_ENODATA;
	}

	if (attr->mr_key_size > sizeof_field(struct ibv_sge, lkey)) {
		VERBS_INFO("MR key size too large\n");
		return -FI_ENODATA;
	}

	if (attr->cq_data_size > sizeof_field(struct ibv_send_wr, imm_data)) {
		VERBS_INFO("CQ data size too large\n");
		return -FI_ENODATA;
	}

	return 0;
}

static int fi_ibv_check_ep_attr(struct fi_ep_attr *attr)
{
	switch (attr->type) {
	case FI_EP_UNSPEC:
	case FI_EP_MSG:
		break;
	default:
		VERBS_INFO("Unsupported endpoint type\n");
		return -FI_ENODATA;
	}

	switch (attr->protocol) {
	case FI_PROTO_UNSPEC:
	case FI_PROTO_RDMA_CM_IB_RC:
	case FI_PROTO_IWARP:
	case FI_PROTO_IB_UD:
		break;
	default:
		VERBS_INFO("Unsupported protocol\n");
		return -FI_ENODATA;
	}

	if (attr->protocol_version > 1) {
		VERBS_INFO("Unsupported protocol version\n");
		return -FI_ENODATA;
	}

	if (attr->max_msg_size > verbs_ep_attr.max_msg_size) {
		VERBS_INFO("Max message size too large\n");
		return -FI_ENODATA;
	}

	if (attr->max_order_raw_size > verbs_ep_attr.max_order_raw_size) {
		VERBS_INFO("max_order_raw_size exceeds supported size\n");
		return -FI_ENODATA;
	}

	if (attr->max_order_war_size) {
		VERBS_INFO("max_order_war_size exceeds supported size\n");
		return -FI_ENODATA;
	}

	if (attr->max_order_waw_size > verbs_ep_attr.max_order_waw_size) {
		VERBS_INFO("max_order_waw_size exceeds supported size\n");
		return -FI_ENODATA;
	}

	if (attr->msg_order & ~(verbs_ep_attr.msg_order)) {
		VERBS_INFO("Given msg ordering not supported\n");
		return -FI_ENODATA;
	}

	if (attr->tx_ctx_cnt > verbs_ep_attr.tx_ctx_cnt) {
		VERBS_INFO("tx_ctx_cnt exceeds supported size\n");
		return -FI_ENODATA;
	}

	if (attr->rx_ctx_cnt > verbs_ep_attr.rx_ctx_cnt) {
		VERBS_INFO("rx_ctx_cnt exceeds supported size\n");
		return -FI_ENODATA;
	}

	return 0;
}

static int fi_ibv_check_rx_attr(struct fi_rx_attr *attr, struct fi_info *info)
{
	if (attr->caps & ~(verbs_rx_attr.caps)) {
		VERBS_INFO("Given rx_attr->caps not supported\n");
		return -FI_ENODATA;
	}

	if (((attr->mode ? attr->mode : info->mode) & 
				verbs_rx_attr.mode) != verbs_rx_attr.mode) {
		VERBS_INFO("Given rx_attr->mode not supported\n");
		return -FI_ENODATA;
	}

	if (attr->msg_order & ~(verbs_rx_attr.msg_order)) {
		VERBS_INFO("Given rx_attr->msg_order not supported\n");
		return -FI_ENODATA;
	}

	if (attr->total_buffered_recv > verbs_rx_attr.total_buffered_recv) {
		VERBS_INFO("Given rx_attr->total_buffered_recv exceeds supported size\n");
		return -FI_ENODATA;
	}

	return 0;
}

static int fi_ibv_check_tx_attr(struct fi_tx_attr *attr, struct fi_info *info)
{
	if (attr->caps & ~(verbs_tx_attr.caps)) {
		VERBS_INFO("Given tx_attr->caps not supported\n");
		return -FI_ENODATA;
	}

	if (((attr->mode ? attr->mode : info->mode) & 
				verbs_tx_attr.mode) != verbs_tx_attr.mode) {
		VERBS_INFO("Given tx_attr->mode not supported\n");
		return -FI_ENODATA;
	}

	if (attr->msg_order & ~(verbs_tx_attr.msg_order)) {
		VERBS_INFO("Given tx_attr->msg_order not supported\n");
		return -FI_ENODATA;
	}

	if (attr->inject_size > verbs_tx_attr.inject_size) {
		VERBS_INFO("Given tx_attr->inject_size exceeds supported size\n");
		return -FI_ENODATA;
	}

	return 0;
}

static int fi_ibv_check_info(struct fi_info *info)
{
	int ret;

	if (info->caps && (info->caps & ~VERBS_CAPS)) {
		VERBS_INFO("Unsupported capabilities\n");
		return -FI_ENODATA;
	}

	if ((info->mode & VERBS_MODE) != VERBS_MODE) {
		VERBS_INFO("Required mode bits not set\n");
		return -FI_ENODATA;
	}

	if (info->fabric_attr) {
		ret = fi_ibv_check_fabric_attr(info->fabric_attr);
		if (ret)
			return ret;
	}

	if (info->domain_attr) {
		ret = fi_ibv_check_domain_attr(info->domain_attr);
		if (ret)
			return ret;
	}

	if (info->ep_attr) {
		ret = fi_ibv_check_ep_attr(info->ep_attr);
		if (ret)
			return ret;
	}

	if (info->rx_attr) {
		ret = fi_ibv_check_rx_attr(info->rx_attr, info);
		if (ret)
			return ret;
	}

	if (info->tx_attr) {
		ret = fi_ibv_check_tx_attr(info->tx_attr, info);
		if (ret)
			return ret;
	}

	return 0;
}

static int fi_ibv_check_dev_limits(struct fi_domain_attr *domain_attr,
				   struct ibv_device_attr *device_attr)
{
	if (domain_attr->cq_cnt > device_attr->max_cq) {
		VERBS_INFO("cq_cnt exceeds supported size\n");
		return -FI_ENODATA;
	}
	
	if (domain_attr->ep_cnt > device_attr->max_qp) {
		VERBS_INFO("ep_cnt exceeds supported size\n");
		return -FI_ENODATA;
	}

	if (domain_attr->tx_ctx_cnt > device_attr->max_qp) {
		VERBS_INFO("domain_attr: tx_ctx_cnt exceeds supported size\n");
		return -FI_ENODATA;
	}

	if (domain_attr->rx_ctx_cnt > device_attr->max_qp) {
		VERBS_INFO("domain_attr: rx_ctx_cnt exceeds supported size\n");
		return -FI_ENODATA;
	}

	return 0;
}

static int fi_ibv_fi_to_rai(struct fi_info *fi, uint64_t flags, struct rdma_addrinfo *rai)
{
	memset(rai, 0, sizeof *rai);
	if (flags & FI_SOURCE)
		rai->ai_flags = RAI_PASSIVE;
	if (flags & FI_NUMERICHOST)
		rai->ai_flags |= RAI_NUMERICHOST;

	rai->ai_qp_type = IBV_QPT_RC;
	rai->ai_port_space = RDMA_PS_TCP;

	if (fi && fi->src_addrlen) {
		if (!(rai->ai_src_addr = malloc(fi->src_addrlen)))
			return -FI_ENOMEM;
		memcpy(rai->ai_src_addr, fi->src_addr, fi->src_addrlen);
		rai->ai_src_len = fi->src_addrlen;
	}
	if (fi && fi->dest_addrlen) {
		if (!(rai->ai_dst_addr = malloc(fi->dest_addrlen)))
			return -FI_ENOMEM;
		memcpy(rai->ai_dst_addr, fi->dest_addr, fi->dest_addrlen);
		rai->ai_dst_len = fi->dest_addrlen;
	}

	return 0;
}

static int fi_ibv_rai_to_fi(struct rdma_addrinfo *rai, struct fi_info *fi)
{
	fi->caps = VERBS_CAPS;
	fi->mode = VERBS_MODE;
	fi->ep_attr->type = FI_EP_MSG;

 	if (rai->ai_src_len) {
 		if (!(fi->src_addr = malloc(rai->ai_src_len)))
 			return -FI_ENOMEM;
 		memcpy(fi->src_addr, rai->ai_src_addr, rai->ai_src_len);
 		fi->src_addrlen = rai->ai_src_len;
 		fi->addr_format = FI_SOCKADDR;
 	}
 	if (rai->ai_dst_len) {
 		if (!(fi->dest_addr = malloc(rai->ai_dst_len)))
 			return -FI_ENOMEM;
 		memcpy(fi->dest_addr, rai->ai_dst_addr, rai->ai_dst_len);
 		fi->dest_addrlen = rai->ai_dst_len;
 		fi->addr_format = FI_SOCKADDR;
 	}

 	return 0;
}

static int fi_ibv_fill_info_attr(struct ibv_context *ctx, struct fi_info *hints,
				 struct fi_info *fi)
{
	struct ibv_device_attr device_attr;
	struct ibv_port_attr port_attr;
	union ibv_gid gid;
	size_t name_len;
	int ret;

	*(fi->fabric_attr) = verbs_fabric_attr;
	*(fi->domain_attr) = verbs_domain_attr;
	*(fi->ep_attr)	   = verbs_ep_attr;
	*(fi->tx_attr)	   = verbs_tx_attr;
	*(fi->rx_attr)	   = verbs_rx_attr;
	
	if (!(fi->fabric_attr->prov_name = strdup(VERBS_PROV_NAME)))
		return -FI_ENOMEM;

	if (!ctx) {
		if (!(fi->fabric_attr->name = strdup(VERBS_ANY_FABRIC)))
			return -FI_ENOMEM;

		return 0;
	}

	ibv_query_gid(ctx, 1, 0, &gid);
	ret = ibv_query_device(ctx, &device_attr);
	if (ret)
		return -errno;

	ret = ibv_query_port(ctx, 1, &port_attr);
	if (ret)
		return -errno;

	if (hints && hints->domain_attr) {
		ret = fi_ibv_check_dev_limits(hints->domain_attr, &device_attr);
		if (ret)
			return ret;
	}

	switch (ctx->device->transport_type) {
	case IBV_TRANSPORT_IB:
		name_len =  strlen(VERBS_IB_PREFIX) + INET6_ADDRSTRLEN;
		if (!(fi->fabric_attr->name = calloc(1, name_len + 1)))
			return -FI_ENOMEM;

		snprintf(fi->fabric_attr->name, name_len, VERBS_IB_PREFIX "%lx",
			 gid.global.subnet_prefix);
		break;
	case IBV_TRANSPORT_IWARP:
		fi->fabric_attr->name = strdup(VERBS_IWARP_FABRIC);
		break;
	default:
		VERBS_INFO("Unknown transport type");
		return -FI_ENODATA;
	}

	if (!(fi->domain_attr->name = strdup(ctx->device->name)))
			return -FI_ENOMEM;

	fi->domain_attr->cq_cnt	= device_attr.max_cq;
	fi->domain_attr->ep_cnt	= device_attr.max_qp;
	fi->domain_attr->tx_ctx_cnt = device_attr.max_qp;
	fi->domain_attr->rx_ctx_cnt = device_attr.max_qp;

	switch (ctx->device->transport_type) {
	case IBV_TRANSPORT_IWARP:
		fi->ep_attr->protocol = FI_PROTO_IWARP;
		break;
	case IBV_TRANSPORT_IB:
		fi->ep_attr->protocol = FI_PROTO_RDMA_CM_IB_RC;
		break;
	default:
		return -FI_ENODATA;
	}

	fi->ep_attr->protocol_version = 1;
	fi->ep_attr->max_msg_size = port_attr.max_msg_sz;

	return 0;
}

static int
fi_ibv_create_ep(const char *node, const char *service,
		 uint64_t flags, struct fi_info *hints,
		 struct rdma_addrinfo **rai, struct rdma_cm_id **id)
{
	struct rdma_addrinfo rai_hints, *_rai;
	int ret;

	if (hints) {
		ret = fi_ibv_check_info(hints);
		if (ret)
			return ret;
	}

	ret = fi_ibv_fi_to_rai(hints, flags, &rai_hints);
	if (ret)
		return ret;

	if (!node && !rai_hints.ai_dst_addr) {
		if (!rai_hints.ai_src_addr) {
			node = local_node;
		}
		rai_hints.ai_flags |= RAI_PASSIVE;
	}

	ret = rdma_getaddrinfo((char *) node, (char *) service,
				&rai_hints, &_rai);
	if (ret)
		return (errno == ENODEV) ? -FI_ENODATA : -errno;

	ret = rdma_create_ep(id, _rai, NULL, NULL);
	if (ret) {
		ret = -errno;
		if (ret == -ENOENT) {
			FI_LOG(1, "verbs",
				"rdma_create_ep()-->ENOENT; likely usnic bug, "
				"skipping verbs provider.\n");
			ret = -FI_ENODATA;
		}
		goto err;
	}

	if (rai) {
		*rai = _rai;
		return 0;
	}
err:
	rdma_freeaddrinfo(_rai);
	return ret;
}

static int fi_ibv_getinfo(uint32_t version, const char *node, const char *service,
			  uint64_t flags, struct fi_info *hints, struct fi_info **info)
{
	struct rdma_cm_id *id;
	struct rdma_addrinfo *rai;
	struct fi_info *fi;
	int ret;

	ret = fi_ibv_create_ep(node, service, flags, hints, &rai, &id);
	if (ret)
		return ret;

	if (!(fi = fi_allocinfo())) {
		ret = -FI_ENOMEM;
		goto err;
	}

	ret = fi_ibv_rai_to_fi(rai, fi);
	if (ret)
		goto err;

	ret = fi_ibv_fill_info_attr(id->verbs, hints, fi);
	if (ret)
		goto err;

	*info = fi;

	rdma_destroy_ep(id);
	rdma_freeaddrinfo(rai);
	return 0;
err:
	if (fi)
		fi_freeinfo(fi);
	rdma_destroy_ep(id);
	rdma_freeaddrinfo(rai);
	return ret;
}

static int fi_ibv_msg_ep_create_qp(struct fi_ibv_msg_ep *ep)
{
	struct ibv_qp_init_attr attr;

	/* TODO: serialize access to string buffers */
	fi_read_file(FI_CONF_DIR, "def_send_wr",
			def_send_wr, sizeof def_send_wr);
	fi_read_file(FI_CONF_DIR, "def_recv_wr",
			def_recv_wr, sizeof def_recv_wr);
	fi_read_file(FI_CONF_DIR, "def_send_sge",
			def_send_sge, sizeof def_send_sge);
	fi_read_file(FI_CONF_DIR, "def_recv_sge",
			def_recv_sge, sizeof def_recv_sge);
	fi_read_file(FI_CONF_DIR, "def_inline_data",
			def_inline_data, sizeof def_inline_data);

	attr.cap.max_send_wr = atoi(def_send_wr);
	attr.cap.max_recv_wr = atoi(def_recv_wr);
	attr.cap.max_send_sge = atoi(def_send_sge);
	attr.cap.max_recv_sge = atoi(def_recv_sge);
	ep->inline_size = atoi(def_inline_data);
	attr.cap.max_inline_data = ep->inline_size;
	attr.qp_context = ep;
	attr.send_cq = ep->scq->cq;
	attr.recv_cq = ep->rcq->cq;
	attr.srq = NULL;
	attr.qp_type = IBV_QPT_RC;
	attr.sq_sig_all = 1;

	return rdma_create_qp(ep->id, ep->rcq->domain->pd, &attr) ? -errno : 0;
}

static int fi_ibv_msg_ep_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct fi_ibv_msg_ep *ep;
	int ret;

	ep = container_of(fid, struct fi_ibv_msg_ep, ep_fid.fid);

	switch (bfid->fclass) {
	case FI_CLASS_CQ:
		if (!(flags & (FI_RECV|FI_SEND))) {
			return -EINVAL;
		}
		if (flags & FI_RECV) {
			if (ep->rcq)
				return -EINVAL;
			ep->rcq = container_of(bfid, struct fi_ibv_cq, cq_fid.fid);
		}
		if (flags & FI_SEND) {
			if (ep->scq)
				return -EINVAL;
			ep->scq = container_of(bfid, struct fi_ibv_cq, cq_fid.fid);
		}
		break;
	case FI_CLASS_EQ:
		ep->eq = container_of(bfid, struct fi_ibv_eq, eq_fid.fid);
		ret = rdma_migrate_id(ep->id, ep->eq->channel);
		if (ret)
			return -errno;
		break;
	default:
		return -EINVAL;
	}

	return 0;
}

static ssize_t
fi_ibv_msg_ep_recv(struct fid_ep *ep, void *buf, size_t len,
		void *desc, fi_addr_t src_addr, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_recv_wr wr, *bad;
	struct ibv_sge sge;

	sge.addr = (uintptr_t) buf;
	sge.length = (uint32_t) len;
	sge.lkey = (uint32_t) (uintptr_t) desc;

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	return -ibv_post_recv(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_recvv(struct fid_ep *ep, const struct iovec *iov, void **desc,
                 size_t count, fi_addr_t src_addr, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_recv_wr wr, *bad;
	struct ibv_sge *sge;
	size_t i;

	sge = alloca(count * sizeof(struct ibv_sge));
	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = sge;
	wr.num_sge = (int) count;

	for (i = 0; i < count; i++) {
		sge[i].addr = (uintptr_t) iov[i].iov_base;
		sge[i].length = (uint32_t) iov[i].iov_len;
		sge[i].lkey = (uint32_t) (uintptr_t) desc[i];
	}

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	return -ibv_post_recv(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_send(struct fid_ep *ep, const void *buf, size_t len,
		void *desc, fi_addr_t dest_addr, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	sge.addr = (uintptr_t) buf;
	sge.length = (uint32_t) len;
	sge.lkey = (uint32_t) (uintptr_t) desc;

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.opcode = IBV_WR_SEND;
	wr.send_flags = (len <= _ep->inline_size) ? IBV_SEND_INLINE : 0;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_senddata(struct fid_ep *ep, const void *buf, size_t len,
		    void *desc, uint64_t data, fi_addr_t dest_addr, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	sge.addr = (uintptr_t) buf;
	sge.length = (uint32_t) len;
	sge.lkey = (uint32_t) (uintptr_t) desc;

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.opcode = IBV_WR_SEND_WITH_IMM;
	wr.send_flags = (len <= _ep->inline_size) ? IBV_SEND_INLINE : 0;
	wr.imm_data = (uint32_t) data;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
                 size_t count, fi_addr_t dest_addr, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge *sge;
	size_t bytes = 0, i;

	sge = alloca(count * sizeof(struct ibv_sge));
	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = sge;
	wr.num_sge = (int) count;
	wr.opcode = IBV_WR_SEND;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	for (i = 0; i < count; i++) {
		sge[i].addr = (uintptr_t) iov[i].iov_base;
		sge[i].length = (uint32_t) iov[i].iov_len;
		bytes += iov[i].iov_len;
		sge[i].lkey = (uint32_t) (uintptr_t) desc[i];
	}
	wr.send_flags = (bytes <= _ep->inline_size) ? IBV_SEND_INLINE : 0;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_sendmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge *sge;
	size_t i, len;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	wr.num_sge = msg->iov_count;
	if (msg->iov_count) {
		sge = alloca(sizeof(*sge) * msg->iov_count);
		for (len = 0, i = 0; i < msg->iov_count; i++) {
			sge[i].addr = (uintptr_t) msg->msg_iov[i].iov_base;
			sge[i].length = (uint32_t) msg->msg_iov[i].iov_len;
			sge[i].lkey = (uint32_t) (uintptr_t) (msg->desc[i]);
			len += sge[i].length;
		}

		wr.sg_list = sge;
		wr.send_flags = (len <= _ep->inline_size) ? IBV_SEND_INLINE : 0;
	} else {
		wr.send_flags = 0;
	}

	wr.wr_id = (uintptr_t) msg->context;
	wr.next = NULL;
	if (flags & FI_REMOTE_CQ_DATA) {
		wr.opcode = IBV_WR_SEND_WITH_IMM;
		wr.imm_data = (uint32_t) msg->data;
	} else {
		wr.opcode = IBV_WR_SEND;
	}

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_recvmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_recv_wr wr, *bad;
	struct ibv_sge *sge=NULL;
	size_t i;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	wr.wr_id = (uintptr_t) msg->context;
	wr.next = NULL;
	if (msg->iov_count) {
		sge = alloca(sizeof(*sge) * msg->iov_count);
		for (i = 0; i < msg->iov_count; i++) {
			sge[i].addr = (uintptr_t) msg->msg_iov[i].iov_base;
			sge[i].length = (uint32_t) msg->msg_iov[i].iov_len;
			sge[i].lkey = (uint32_t) (uintptr_t) (msg->desc[i]);
		}

	}
	wr.sg_list = sge;
	wr.num_sge = msg->iov_count;

	return -ibv_post_recv(_ep->id->qp, &wr, &bad);
}

static struct fi_ops_msg fi_ibv_msg_ep_msg_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = fi_ibv_msg_ep_recv,
	.recvv = fi_ibv_msg_ep_recvv,
	.recvmsg = fi_ibv_msg_ep_recvmsg,
	.send = fi_ibv_msg_ep_send,
	.sendv = fi_ibv_msg_ep_sendv,
	.sendmsg = fi_ibv_msg_ep_sendmsg,
	.inject = fi_no_msg_inject,
	.senddata = fi_ibv_msg_ep_senddata,
	.injectdata = fi_no_msg_injectdata,
};

static ssize_t
fi_ibv_msg_ep_rma_write(struct fid_ep *ep, const void *buf, size_t len,
		     void *desc, fi_addr_t dest_addr,
		     uint64_t addr, uint64_t key, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	sge.addr = (uintptr_t) buf;
	sge.length = (uint32_t) len;
	sge.lkey = (uint32_t) (uintptr_t) desc;

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.opcode = IBV_WR_RDMA_WRITE;
	wr.send_flags = (len <= _ep->inline_size) ? IBV_SEND_INLINE : 0;
	wr.wr.rdma.remote_addr = addr;
	wr.wr.rdma.rkey = (uint32_t) key;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_rma_writev(struct fid_ep *ep, const struct iovec *iov, void **desc,
		      size_t count, fi_addr_t dest_addr,
		      uint64_t addr, uint64_t key, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge *sge;
	size_t bytes = 0, i;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	sge = alloca(count * sizeof(struct ibv_sge));

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = sge;
	wr.num_sge = count;
	wr.opcode = IBV_WR_RDMA_WRITE;
	wr.wr.rdma.remote_addr = addr;
	wr.wr.rdma.rkey = (uint32_t) key;

	for (i = 0; i < count; i++) {
		sge[i].addr = (uintptr_t) iov[i].iov_base;
		sge[i].length = (uint32_t) iov[i].iov_len;
		bytes += iov[i].iov_len;
		sge[i].lkey = (uint32_t) (uintptr_t) desc[i];
	}
	wr.send_flags = (bytes <= _ep->inline_size) ? IBV_SEND_INLINE : 0;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_rma_writemsg(struct fid_ep *ep, const struct fi_msg_rma *msg,
			uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge *sge=NULL;
	size_t i, len;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	wr.wr_id = (uintptr_t) msg->context;
	wr.next = NULL;
	wr.num_sge = msg->iov_count;
	wr.send_flags = 0;
	if (msg->iov_count) {
		sge = alloca(sizeof(*sge) * msg->iov_count);
		for (len = 0, i = 0; i < msg->iov_count; i++) {
			sge[i].addr = (uintptr_t) msg->msg_iov[i].iov_base;
			sge[i].length = (uint32_t) msg->msg_iov[i].iov_len;
			sge[i].lkey = (uint32_t) (uintptr_t) (msg->desc[i]);
			len += sge[i].length;
		}

		wr.send_flags = (len <= _ep->inline_size) ? IBV_SEND_INLINE : 0;
	}
	wr.sg_list = sge;

	wr.opcode = IBV_WR_RDMA_WRITE;
	if (flags & FI_REMOTE_CQ_DATA) {
		wr.opcode = IBV_WR_RDMA_WRITE_WITH_IMM;
		wr.imm_data = (uint32_t) msg->data;
	}

	wr.wr.rdma.remote_addr = msg->rma_iov->addr;
	wr.wr.rdma.rkey = (uint32_t) msg->rma_iov->key;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_rma_read(struct fid_ep *ep, void *buf, size_t len,
		    void *desc, fi_addr_t src_addr,
		    uint64_t addr, uint64_t key, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	sge.addr = (uintptr_t) buf;
	sge.length = (uint32_t) len;
	sge.lkey = (uint32_t) (uintptr_t) desc;

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.opcode = IBV_WR_RDMA_READ;
	wr.send_flags = 0;
	wr.wr.rdma.remote_addr = addr;
	wr.wr.rdma.rkey = (uint32_t) key;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_rma_readv(struct fid_ep *ep, const struct iovec *iov, void **desc,
		     size_t count, fi_addr_t src_addr,
		     uint64_t addr, uint64_t key, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge *sge;
	size_t i;

	sge = alloca(count * sizeof(struct ibv_sge));
	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = sge;
	wr.num_sge = count;
	wr.opcode = IBV_WR_RDMA_READ;
	wr.send_flags = 0;
	wr.wr.rdma.remote_addr = addr;
	wr.wr.rdma.rkey = (uint32_t) key;

	for (i = 0; i < count; i++) {
		sge[i].addr = (uintptr_t) iov[i].iov_base;
		sge[i].length = (uint32_t) iov[i].iov_len;
		sge[i].lkey = (uint32_t) (uintptr_t) desc[i];
	}

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_rma_readmsg(struct fid_ep *ep, const struct fi_msg_rma *msg,
			uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge *sge;
	size_t i;

	wr.wr_id = (uintptr_t) msg->context;
	wr.next = NULL;
	wr.sg_list = NULL;
	if (msg->iov_count) {
		sge = alloca(sizeof(*sge) * msg->iov_count);
		for (i = 0; i < msg->iov_count; i++) {
			sge[i].addr = (uintptr_t) msg->msg_iov[i].iov_base;
			sge[i].length = (uint32_t) msg->msg_iov[i].iov_len;
			sge[i].lkey = (uint32_t) (uintptr_t) (msg->desc[i]);
		}
		wr.sg_list = sge;
	}
	wr.num_sge = msg->iov_count;
	wr.opcode = IBV_WR_RDMA_READ;
	wr.send_flags = 0;

	wr.wr.rdma.remote_addr = msg->rma_iov->addr;
	wr.wr.rdma.rkey = (uint32_t) msg->rma_iov->key;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_rma_writedata(struct fid_ep *ep, const void *buf, size_t len,
			void *desc, uint64_t data, fi_addr_t dest_addr,
			uint64_t addr, uint64_t key, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	sge.addr = (uintptr_t) buf;
	sge.length = (uint32_t) len;
	sge.lkey = (uint32_t) (uintptr_t) desc;

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.opcode = IBV_WR_RDMA_WRITE_WITH_IMM;
	wr.send_flags = (len <= _ep->inline_size) ? IBV_SEND_INLINE : 0;
	wr.imm_data = (uint32_t) data;

	wr.wr.rdma.remote_addr = addr;
	wr.wr.rdma.rkey = (uint32_t) key;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static struct fi_ops_rma fi_ibv_msg_ep_rma_ops = {
	.size = sizeof(struct fi_ops_rma),
	.read = fi_ibv_msg_ep_rma_read,
	.readv = fi_ibv_msg_ep_rma_readv,
	.readmsg = fi_ibv_msg_ep_rma_readmsg,
	.write = fi_ibv_msg_ep_rma_write,
	.writev = fi_ibv_msg_ep_rma_writev,
	.writemsg = fi_ibv_msg_ep_rma_writemsg,
	.inject = fi_no_rma_inject,
	.writedata = fi_ibv_msg_ep_rma_writedata,
	.injectdata = fi_no_rma_injectdata,
};

static ssize_t
fi_ibv_msg_ep_atomic_write(struct fid_ep *ep, const void *buf, size_t count,
			void *desc, fi_addr_t dest_addr, uint64_t addr, uint64_t key,
			enum fi_datatype datatype, enum fi_op op, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	if (count != 1)
		return -FI_E2BIG;

	switch (datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	switch (op) {
	case FI_ATOMIC_WRITE:
		wr.opcode = IBV_WR_RDMA_WRITE;
		wr.wr.rdma.remote_addr = addr;
		wr.wr.rdma.rkey = (uint32_t) (uintptr_t) key;
		break;
	default:
		return -ENOSYS;
	}
	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);

	sge.addr = (uintptr_t) buf;
	sge.length = (uint32_t) sizeof(uint64_t);
	sge.lkey = (uint32_t) (uintptr_t) desc;

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = (sge.length <= _ep->inline_size) ? IBV_SEND_INLINE : 0;
	wr.send_flags |= IBV_SEND_FENCE; 

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_atomic_writev(struct fid_ep *ep,
                        const struct fi_ioc *iov, void **desc, size_t count,
                        fi_addr_t dest_addr, uint64_t addr, uint64_t key,
                        enum fi_datatype datatype, enum fi_op op, void *context)
{
	if (iov->count != 1)
		return -FI_E2BIG;

	return fi_ibv_msg_ep_atomic_write(ep, iov->addr, count, desc[0],
			dest_addr, addr, key, datatype, op, context);
}

static ssize_t
fi_ibv_msg_ep_atomic_writemsg(struct fid_ep *ep,
                        const struct fi_msg_atomic *msg, uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	if (msg->iov_count != 1 || msg->msg_iov->count != 1)
		return -FI_E2BIG;

	switch (msg->datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	switch (msg->op) {
	case FI_ATOMIC_WRITE:
		if (flags & FI_REMOTE_CQ_DATA) {
			wr.opcode = IBV_WR_RDMA_WRITE_WITH_IMM;
			wr.imm_data = (uint32_t) msg->data;
		} else {
			wr.opcode = IBV_WR_RDMA_WRITE;
		}
		wr.wr.rdma.remote_addr = msg->rma_iov->addr;
		wr.wr.rdma.rkey = (uint32_t) (uintptr_t) msg->rma_iov->key;
		break;
	default:
		return -ENOSYS;
	}
	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);

	sge.addr = (uintptr_t) msg->msg_iov->addr;
	sge.length = (uint32_t) sizeof(uint64_t);
	sge.lkey = (uint32_t) (uintptr_t) msg->desc[0];

	wr.wr_id = (uintptr_t) msg->context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = (sge.length <= _ep->inline_size) ? IBV_SEND_INLINE : 0;
	wr.send_flags |= IBV_SEND_FENCE; 

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_atomic_readwrite(struct fid_ep *ep, const void *buf, size_t count,
			void *desc, void *result, void *result_desc,
			fi_addr_t dest_addr, uint64_t addr, uint64_t key,
			enum fi_datatype datatype,
			enum fi_op op, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	if (count != 1)
		return -FI_E2BIG;

	switch (datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	switch (op) {
	case FI_ATOMIC_READ:
		wr.opcode = IBV_WR_RDMA_READ;
		wr.wr.rdma.remote_addr = addr;
		wr.wr.rdma.rkey = (uint32_t) (uintptr_t) key;
		break;
	case FI_SUM:
		wr.opcode = IBV_WR_ATOMIC_FETCH_AND_ADD;
		wr.wr.atomic.remote_addr = addr;
		wr.wr.atomic.compare_add = (uintptr_t) buf;
		wr.wr.atomic.swap = 0;
		wr.wr.atomic.rkey = (uint32_t) (uintptr_t) key;
		break;
	default:
		return -ENOSYS;
	}

	sge.addr = (uintptr_t) result;
	sge.length = (uint32_t) sizeof(uint64_t);
	sge.lkey = (uint32_t) (uintptr_t) result_desc;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = IBV_SEND_FENCE; 

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_atomic_readwritev(struct fid_ep *ep, const struct fi_ioc *iov,
			void **desc, size_t count,
			struct fi_ioc *resultv, void **result_desc,
			size_t result_count, fi_addr_t dest_addr, uint64_t addr,
			uint64_t key, enum fi_datatype datatype,
			enum fi_op op, void *context)
{
	if (iov->count != 1)
		return -FI_E2BIG;

        return fi_ibv_msg_ep_atomic_readwrite(ep, iov->addr, count,
			desc[0], resultv->addr, result_desc[0],
			dest_addr, addr, key, datatype, op, context);
}

static ssize_t
fi_ibv_msg_ep_atomic_readwritemsg(struct fid_ep *ep,
				const struct fi_msg_atomic *msg,
				struct fi_ioc *resultv, void **result_desc,
				size_t result_count, uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	if (msg->iov_count != 1 || msg->msg_iov->count != 1)
		return -FI_E2BIG;

	switch (msg->datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	switch (msg->op) {
	case FI_ATOMIC_READ:
		wr.opcode = IBV_WR_RDMA_READ;
		wr.wr.rdma.remote_addr = msg->rma_iov->addr;
		wr.wr.rdma.rkey = (uint32_t) (uintptr_t) msg->rma_iov->key;
		break;
	case FI_SUM:
		wr.opcode = IBV_WR_ATOMIC_FETCH_AND_ADD;
		wr.wr.atomic.remote_addr = msg->rma_iov->addr;
		wr.wr.atomic.compare_add = (uintptr_t) msg->addr;
		wr.wr.atomic.swap = 0;
		wr.wr.atomic.rkey = (uint32_t) (uintptr_t) msg->rma_iov->key;
		break;
	default:
		return -ENOSYS;
	}

	sge.addr = (uintptr_t) resultv->addr;
	sge.length = (uint32_t) sizeof(uint64_t);
	sge.lkey = (uint32_t) (uintptr_t) result_desc[0];

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);

	wr.wr_id = (uintptr_t) msg->context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = IBV_SEND_FENCE; 
	if (flags & FI_REMOTE_CQ_DATA)
		wr.imm_data = (uint32_t) msg->data;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_atomic_compwrite(struct fid_ep *ep, const void *buf, size_t count,
			void *desc, const void *compare,
			void *compare_desc, void *result,
			void *result_desc,
			fi_addr_t dest_addr, uint64_t addr, uint64_t key,
			enum fi_datatype datatype,
			enum fi_op op, void *context)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	if (op != FI_CSWAP)
		return -ENOSYS;

	if (count != 1)
		return -FI_E2BIG;

	switch (datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	wr.opcode = IBV_WR_ATOMIC_CMP_AND_SWP;
	wr.wr.atomic.remote_addr = addr;
	wr.wr.atomic.compare_add = (uintptr_t) compare;
	wr.wr.atomic.swap = (uintptr_t) buf;
	wr.wr.atomic.rkey = (uint32_t) (uintptr_t) key;

	sge.addr = (uintptr_t) result;
	sge.length = (uint32_t) sizeof(uint64_t);
	sge.lkey = (uint32_t) (uintptr_t) result_desc;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);

	wr.wr_id = (uintptr_t) context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = IBV_SEND_FENCE; 

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static ssize_t
fi_ibv_msg_ep_atomic_compwritev(struct fid_ep *ep, const struct fi_ioc *iov,
				void **desc, size_t count,
				const struct fi_ioc *comparev,
				void **compare_desc, size_t compare_count,
				struct fi_ioc *resultv, void **result_desc,
				size_t result_count,
				fi_addr_t dest_addr, uint64_t addr, uint64_t key,
				enum fi_datatype datatype,
				enum fi_op op, void *context)
{
	if (iov->count != 1)
		return -FI_E2BIG;

	return fi_ibv_msg_ep_atomic_compwrite(ep, iov->addr, count, desc[0],
				comparev->addr, compare_desc[0], resultv->addr,
				result_desc[0], dest_addr, addr, key,
                        	datatype, op, context);
}

static ssize_t
fi_ibv_msg_ep_atomic_compwritemsg(struct fid_ep *ep,
				const struct fi_msg_atomic *msg,
				const struct fi_ioc *comparev,
				void **compare_desc, size_t compare_count,
				struct fi_ioc *resultv,
				void **result_desc, size_t result_count,
				uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	struct ibv_send_wr wr, *bad;
	struct ibv_sge sge;

	if (msg->op != FI_CSWAP)
		return -ENOSYS;

	if (msg->iov_count != 1 || msg->msg_iov->count != 1)
		return -FI_E2BIG;

	switch(msg->datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	wr.opcode = IBV_WR_ATOMIC_CMP_AND_SWP;
	wr.wr.atomic.remote_addr = msg->rma_iov->addr;
	wr.wr.atomic.compare_add = (uintptr_t) comparev->addr;
	wr.wr.atomic.swap = (uintptr_t) msg->addr;
	wr.wr.atomic.rkey = (uint32_t) (uintptr_t) msg->rma_iov->key;

	sge.addr = (uintptr_t) resultv->addr;
	sge.length = (uint32_t) sizeof(uint64_t);
	sge.lkey = (uint32_t) (uintptr_t) result_desc[0];

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);

	wr.wr_id = (uintptr_t) msg->context;
	wr.next = NULL;
	wr.sg_list = &sge;
	wr.num_sge = 1;
	wr.send_flags = IBV_SEND_FENCE; 
	if (flags & FI_REMOTE_CQ_DATA)
		wr.imm_data = (uint32_t) msg->data;

	return -ibv_post_send(_ep->id->qp, &wr, &bad);
}

static int
fi_ibv_msg_ep_atomic_writevalid(struct fid_ep *ep, enum fi_datatype datatype,
				enum fi_op op, size_t *count)
{
	switch (op) {
	case FI_ATOMIC_WRITE:
		break;
	default:
		return -FI_ENOSYS;
	}

	switch (datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	if (count)
		*count = 1;
	return 0;
}

static int
fi_ibv_msg_ep_atomic_readwritevalid(struct fid_ep *ep, enum fi_datatype datatype,
				enum fi_op op, size_t *count)
{
	switch (op) {
	case FI_ATOMIC_READ:
	case FI_SUM:
		break;
	default:
		return -FI_ENOSYS;
	}

	switch (datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	if (count)
		*count = 1;
	return 0;
}

static int
fi_ibv_msg_ep_atomic_compwritevalid(struct fid_ep *ep, enum fi_datatype datatype,
				enum fi_op op, size_t *count)
{
	if (op != FI_CSWAP)
		return -FI_ENOSYS;

	switch (datatype) {
	case FI_INT64:
	case FI_UINT64:
#if __BITS_PER_LONG == 64
	case FI_DOUBLE:
	case FI_FLOAT:
#endif
		break;
	default:
		return -FI_EINVAL;
	}

	if (count)
		*count = 1;
	return 0;
}


static struct fi_ops_atomic fi_ibv_msg_ep_atomic_ops = {
	.size		= sizeof(struct fi_ops_atomic),
	.write		= fi_ibv_msg_ep_atomic_write,
	.writev		= fi_ibv_msg_ep_atomic_writev,
	.writemsg	= fi_ibv_msg_ep_atomic_writemsg,
	.readwrite	= fi_ibv_msg_ep_atomic_readwrite,
	.readwritev	= fi_ibv_msg_ep_atomic_readwritev,
	.readwritemsg	= fi_ibv_msg_ep_atomic_readwritemsg,
	.compwrite	= fi_ibv_msg_ep_atomic_compwrite,
	.compwritev	= fi_ibv_msg_ep_atomic_compwritev,
	.compwritemsg	= fi_ibv_msg_ep_atomic_compwritemsg,
	.writevalid	= fi_ibv_msg_ep_atomic_writevalid,
	.readwritevalid	= fi_ibv_msg_ep_atomic_readwritevalid,
	.compwritevalid = fi_ibv_msg_ep_atomic_compwritevalid
};

static int fi_ibv_copy_addr(void *dst_addr, size_t *dst_addrlen, void *src_addr)
{
	size_t src_addrlen = fi_ibv_sockaddr_len(src_addr);

	if (*dst_addrlen == 0) {
		*dst_addrlen = src_addrlen;
		return -FI_ETOOSMALL;
	}

	if (*dst_addrlen < src_addrlen) {
		memcpy(dst_addr, src_addr, *dst_addrlen);
	} else {
		memcpy(dst_addr, src_addr, src_addrlen);
	}
	*dst_addrlen = src_addrlen;
	return 0;
}

static int fi_ibv_msg_ep_getname(fid_t ep, void *addr, size_t *addrlen)
{
	struct fi_ibv_msg_ep *_ep;
	struct sockaddr *sa;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	sa = rdma_get_local_addr(_ep->id);
	return fi_ibv_copy_addr(addr, addrlen, sa);
}

static int fi_ibv_msg_ep_getpeer(struct fid_ep *ep, void *addr, size_t *addrlen)
{
	struct fi_ibv_msg_ep *_ep;
	struct sockaddr *sa;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	sa = rdma_get_peer_addr(_ep->id);
	return fi_ibv_copy_addr(addr, addrlen, sa);
}

static int
fi_ibv_msg_ep_connect(struct fid_ep *ep, const void *addr,
		   const void *param, size_t paramlen)
{
	struct fi_ibv_msg_ep *_ep;
	struct rdma_conn_param conn_param;
	int ret;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	if (!_ep->id->qp) {
		ret = ep->fid.ops->control(&ep->fid, FI_ENABLE, NULL);
		if (ret)
			return ret;
	}

	memset(&conn_param, 0, sizeof conn_param);
	conn_param.private_data = param;
	conn_param.private_data_len = paramlen;
	conn_param.responder_resources = RDMA_MAX_RESP_RES;
	conn_param.initiator_depth = RDMA_MAX_INIT_DEPTH;
	conn_param.flow_control = 1;
	conn_param.retry_count = 15;
	conn_param.rnr_retry_count = 7;

	return rdma_connect(_ep->id, &conn_param) ? -errno : 0;
}

static int
fi_ibv_msg_ep_accept(struct fid_ep *ep, const void *param, size_t paramlen)
{
	struct fi_ibv_msg_ep *_ep;
	struct rdma_conn_param conn_param;
	int ret;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	if (!_ep->id->qp) {
		ret = ep->fid.ops->control(&ep->fid, FI_ENABLE, NULL);
		if (ret)
			return ret;
	}

	memset(&conn_param, 0, sizeof conn_param);
	conn_param.private_data = param;
	conn_param.private_data_len = paramlen;
	conn_param.responder_resources = RDMA_MAX_RESP_RES;
	conn_param.initiator_depth = RDMA_MAX_INIT_DEPTH;
	conn_param.flow_control = 1;
	conn_param.rnr_retry_count = 7;

	return rdma_accept(_ep->id, &conn_param) ? -errno : 0;
}

static int
fi_ibv_msg_ep_reject(struct fid_pep *pep, fi_connreq_t connreq,
		  const void *param, size_t paramlen)
{
	return rdma_reject((struct rdma_cm_id *) connreq, param,
			   (uint8_t) paramlen) ? -errno : 0;
}

static int fi_ibv_msg_ep_shutdown(struct fid_ep *ep, uint64_t flags)
{
	struct fi_ibv_msg_ep *_ep;
	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	return rdma_disconnect(_ep->id) ? -errno : 0;
}

static struct fi_ops_cm fi_ibv_msg_ep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = fi_ibv_msg_ep_getname,
	.getpeer = fi_ibv_msg_ep_getpeer,
	.connect = fi_ibv_msg_ep_connect,
	.listen = fi_no_listen,
	.accept = fi_ibv_msg_ep_accept,
	.reject = fi_no_reject,
	.shutdown = fi_ibv_msg_ep_shutdown,
};

static int
fi_ibv_msg_ep_getopt(fid_t fid, int level, int optname,
		  void *optval, size_t *optlen)
{
	switch (level) {
	case FI_OPT_ENDPOINT:
		switch (optname) {
		case FI_OPT_CM_DATA_SIZE:
			if (*optlen < sizeof(size_t))
				return -FI_ETOOSMALL;
			*((size_t *) optval) = VERBS_CM_DATA_SIZE;
			*optlen = sizeof(size_t);
			return 0;
		default:
			return -FI_ENOPROTOOPT;
		}
	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

static int
fi_ibv_msg_ep_setopt(fid_t fid, int level, int optname,
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

static int fi_ibv_msg_ep_enable(struct fid_ep *ep)
{
	struct fi_ibv_msg_ep *_ep;

	_ep = container_of(ep, struct fi_ibv_msg_ep, ep_fid);
	if (!_ep->eq)
		return -FI_ENOEQ;
	if (!_ep->scq || !_ep->rcq)
		return -FI_ENOCQ;

	return fi_ibv_msg_ep_create_qp(_ep);
}

static struct fi_ops_ep fi_ibv_msg_ep_base_ops = {
	.size = sizeof(struct fi_ops_ep),
	.cancel = fi_no_cancel,
	.getopt = fi_ibv_msg_ep_getopt,
	.setopt = fi_ibv_msg_ep_setopt,
	.rx_size_left = fi_no_rx_size_left,
	.tx_size_left = fi_no_tx_size_left,
};

static int fi_ibv_msg_ep_close(fid_t fid)
{
	struct fi_ibv_msg_ep *ep;

	ep = container_of(fid, struct fi_ibv_msg_ep, ep_fid.fid);
	if (ep->id)
		rdma_destroy_ep(ep->id);

	free(ep);
	return 0;
}

static int fi_ibv_msg_ep_control(struct fid *fid, int command, void *arg)
{
	struct fid_ep *ep;

	switch (fid->fclass) {
	case FI_CLASS_EP:
		ep = container_of(fid, struct fid_ep, fid);
		switch (command) {
		case FI_ENABLE:
			return fi_ibv_msg_ep_enable(ep);
			break;
		default:
			return -FI_ENOSYS;
		}
		break;
	default:
		return -FI_ENOSYS;
	}
}

static struct fi_ops fi_ibv_msg_ep_ops = {
	.size = sizeof(struct fi_ops),
	.close = fi_ibv_msg_ep_close,
	.bind = fi_ibv_msg_ep_bind,
	.control = fi_ibv_msg_ep_control,
	.ops_open = fi_no_ops_open,
};

static int
fi_ibv_open_ep(struct fid_domain *domain, struct fi_info *info,
	    struct fid_ep **ep, void *context)
{
	struct fi_ibv_domain *_domain;
	struct fi_ibv_msg_ep *_ep;
	int ret;

	_domain = container_of(domain, struct fi_ibv_domain, domain_fid);
	if (strcmp(_domain->verbs->device->name, info->domain_attr->name))
		return -FI_EINVAL;

	_ep = calloc(1, sizeof *_ep);
	if (!_ep)
		return -FI_ENOMEM;

	if (!info->connreq) {
		ret = fi_ibv_create_ep(NULL, NULL, 0, info, NULL, &_ep->id);
		if (ret)
			goto err;

	} else {
		_ep->id = (struct rdma_cm_id *) info->connreq;
	}
	_ep->id->context = &_ep->ep_fid.fid;

	_ep->ep_fid.fid.fclass = FI_CLASS_EP;
	_ep->ep_fid.fid.context = context;
	_ep->ep_fid.fid.ops = &fi_ibv_msg_ep_ops;
	_ep->ep_fid.ops = &fi_ibv_msg_ep_base_ops;
	_ep->ep_fid.msg = &fi_ibv_msg_ep_msg_ops;
	_ep->ep_fid.cm = &fi_ibv_msg_ep_cm_ops;
	_ep->ep_fid.rma = &fi_ibv_msg_ep_rma_ops;
	_ep->ep_fid.atomic = &fi_ibv_msg_ep_atomic_ops;

	*ep = &_ep->ep_fid;
	return 0;
err:
	free(_ep);
	return ret;
}

static ssize_t
fi_ibv_eq_readerr(struct fid_eq *eq, struct fi_eq_err_entry *entry,
		  uint64_t flags)
{
	struct fi_ibv_eq *_eq;

	_eq = container_of(eq, struct fi_ibv_eq, eq_fid.fid);
	if (!_eq->err.err)
		return 0;

	*entry = _eq->err;
	_eq->err.err = 0;
	_eq->err.prov_errno = 0;
	return sizeof(*entry);
}

/* TODO: This should copy the listening fi_info as the base */
static struct fi_info *
fi_ibv_eq_cm_getinfo(struct fi_ibv_fabric *fab, struct rdma_cm_event *event)
{
	struct fi_info *fi;

	fi = fi_allocinfo();
	if (!fi)
		return NULL;

	fi->ep_attr->type = FI_EP_MSG;
	fi->caps  = VERBS_CAPS;

	fi->src_addrlen = fi_ibv_sockaddr_len(rdma_get_local_addr(event->id));
	if (!(fi->src_addr = malloc(fi->src_addrlen)))
		goto err;
	memcpy(fi->src_addr, rdma_get_local_addr(event->id), fi->src_addrlen);

	fi->dest_addrlen = fi_ibv_sockaddr_len(rdma_get_peer_addr(event->id));
	if (!(fi->dest_addr = malloc(fi->dest_addrlen)))
		goto err;
	memcpy(fi->dest_addr, rdma_get_peer_addr(event->id), fi->dest_addrlen);

	fi_ibv_fill_info_attr(event->id->verbs, NULL, fi);

	fi->connreq = (fi_connreq_t) event->id;
	return fi;
err:
	fi_freeinfo(fi);
	return NULL;
}

static ssize_t
fi_ibv_eq_cm_process_event(struct fi_ibv_eq *eq, struct rdma_cm_event *cma_event,
	uint32_t *event, struct fi_eq_cm_entry *entry, size_t len)
{
	fid_t fid;
	size_t datalen;

	fid = cma_event->id->context;
	switch (cma_event->event) {
//	case RDMA_CM_EVENT_ADDR_RESOLVED:
//		return 0;
//	case RDMA_CM_EVENT_ROUTE_RESOLVED:
//		return 0;
	case RDMA_CM_EVENT_CONNECT_REQUEST:
		*event = FI_CONNREQ;
		entry->info = fi_ibv_eq_cm_getinfo(eq->fab, cma_event);
		if (!entry->info) {
			rdma_destroy_id(cma_event->id);
			return 0;
		}
		break;
	case RDMA_CM_EVENT_ESTABLISHED:
		*event = FI_CONNECTED;
		entry->info = NULL;
		break;
	case RDMA_CM_EVENT_DISCONNECTED:
		*event = FI_SHUTDOWN;
		entry->info = NULL;
		break;
	case RDMA_CM_EVENT_ADDR_ERROR:
	case RDMA_CM_EVENT_ROUTE_ERROR:
	case RDMA_CM_EVENT_CONNECT_ERROR:
	case RDMA_CM_EVENT_UNREACHABLE:
		eq->err.fid = fid;
		eq->err.err = cma_event->status;
		return -FI_EAVAIL;
	case RDMA_CM_EVENT_REJECTED:
		eq->err.fid = fid;
		eq->err.err = ECONNREFUSED;
		eq->err.prov_errno = cma_event->status;
		return -FI_EAVAIL;
	case RDMA_CM_EVENT_DEVICE_REMOVAL:
		eq->err.fid = fid;
		eq->err.err = ENODEV;
		return -FI_EAVAIL;
	case RDMA_CM_EVENT_ADDR_CHANGE:
		eq->err.fid = fid;
		eq->err.err = EADDRNOTAVAIL;
		return -FI_EAVAIL;
	default:
		return 0;
	}

	entry->fid = fid;
	datalen = MIN(len - sizeof(*entry), cma_event->param.conn.private_data_len);
	if (datalen)
		memcpy(entry->data, cma_event->param.conn.private_data, datalen);
	return sizeof(*entry) + datalen;
}

static ssize_t
fi_ibv_eq_read(struct fid_eq *eq, uint32_t *event,
	       void *buf, size_t len, uint64_t flags)
{
	struct fi_ibv_eq *_eq;
	struct fi_eq_cm_entry *entry;
	struct rdma_cm_event *cma_event;
	ssize_t ret = 0;

	_eq = container_of(eq, struct fi_ibv_eq, eq_fid.fid);
	entry = (struct fi_eq_cm_entry *) buf;
	if (_eq->err.err)
		return -FI_EAVAIL;

	ret = rdma_get_cm_event(_eq->channel, &cma_event);
	if (ret)
		return (errno == EAGAIN) ? 0 : -errno;

	ret = fi_ibv_eq_cm_process_event(_eq, cma_event, event, entry, len);
	rdma_ack_cm_event(cma_event);
	return ret;
}

static ssize_t
fi_ibv_eq_sread(struct fid_eq *eq, uint32_t *event,
		void *buf, size_t len, int timeout, uint64_t flags)
{
	struct fi_ibv_eq *_eq;
	ssize_t ret;

	_eq = container_of(eq, struct fi_ibv_eq, eq_fid.fid);

	while (1) {
		ret = fi_ibv_eq_read(eq, event, buf, len, flags);
		if (ret)
			return ret;

		ret = fi_poll_fd(_eq->channel->fd, timeout);
		if (ret == 0)
			return -FI_ETIMEDOUT;
		else if (ret < 0)
			return ret;
	};
}

static const char *
fi_ibv_eq_strerror(struct fid_eq *eq, int prov_errno, const void *err_data,
		   char *buf, size_t len)
{
	if (buf && len)
		strncpy(buf, strerror(prov_errno), len);
	return strerror(prov_errno);
}

static struct fi_ops_eq fi_ibv_eq_ops = {
	.size = sizeof(struct fi_ops_eq),
	.read = fi_ibv_eq_read,
	.readerr = fi_ibv_eq_readerr,
	.write = fi_no_eq_write,
	.sread = fi_ibv_eq_sread,
	.strerror = fi_ibv_eq_strerror
};

static int fi_ibv_eq_control(fid_t fid, int command, void *arg)
{
	struct fi_ibv_eq *eq;
	int ret = 0;

	eq = container_of(fid, struct fi_ibv_eq, eq_fid.fid);
	switch (command) {
	case FI_GETWAIT:
		if (!eq->channel) {
			ret = -FI_ENODATA;
			break;
		}
		*(int *) arg = eq->channel->fd;
		break;
	default:
		ret = -FI_ENOSYS;
		break;
	}

	return ret;
}

static int fi_ibv_eq_close(fid_t fid)
{
	struct fi_ibv_eq *eq;

	eq = container_of(fid, struct fi_ibv_eq, eq_fid.fid);
	if (eq->channel)
		rdma_destroy_event_channel(eq->channel);

	free(eq);
	return 0;
}

static struct fi_ops fi_ibv_eq_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = fi_ibv_eq_close,
	.bind = fi_no_bind,
	.control = fi_ibv_eq_control,
	.ops_open = fi_no_ops_open,
};

static int
fi_ibv_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
	       struct fid_eq **eq, void *context)
{
	struct fi_ibv_eq *_eq;
	long flags = 0;
	int ret;

	_eq = calloc(1, sizeof *_eq);
	if (!_eq)
		return -ENOMEM;

	_eq->fab = container_of(fabric, struct fi_ibv_fabric, fabric_fid);

	switch (attr->wait_obj) {
	case FI_WAIT_UNSPEC:
	case FI_WAIT_FD:
		_eq->channel = rdma_create_event_channel();
		if (!_eq->channel) {
			ret = -errno;
			goto err1;
		}
		flags = fcntl(_eq->channel->fd, F_GETFL);
		if (flags < 0) {
			ret = -errno;
			goto err2;
		}
		ret = fcntl(_eq->channel->fd, F_SETFL, flags | O_NONBLOCK);
		if (ret) {
			ret = -errno;
			goto err2;
		}
		break;
	case FI_WAIT_NONE:
		break;
	default:
		ret = -FI_ENOSYS;
		goto err1;
	}

	_eq->flags = attr->flags;
	_eq->eq_fid.fid.fclass = FI_CLASS_EQ;
	_eq->eq_fid.fid.context = context;
	_eq->eq_fid.fid.ops = &fi_ibv_eq_fi_ops;
	_eq->eq_fid.ops = &fi_ibv_eq_ops;

	*eq = &_eq->eq_fid;
	return 0;
err2:
	if (_eq->channel)
		rdma_destroy_event_channel(_eq->channel);
err1:
	free(_eq);
	return ret;
}

static ssize_t
fi_ibv_cq_readerr(struct fid_cq *cq, struct fi_cq_err_entry *entry,
		  uint64_t flags)
{
	struct fi_ibv_cq *_cq;

	_cq = container_of(cq, struct fi_ibv_cq, cq_fid);
	if (!_cq->wc.status)
		return 0;

	entry->op_context = (void *) (uintptr_t) _cq->wc.wr_id;
	entry->flags = 0;
	entry->err = EIO;
	entry->prov_errno = _cq->wc.status;
	memcpy(&entry->err_data, &_cq->wc.vendor_err,
	       sizeof(_cq->wc.vendor_err));

	_cq->wc.status = 0;
	return sizeof(*entry);
}

static int fi_ibv_cq_reset(struct fid_cq *cq, const void *cond)
{
        struct fi_ibv_cq *_cq;
        struct ibv_cq *ibcq;
        void *context;
        int ret;

        _cq = container_of(cq, struct fi_ibv_cq, cq_fid);
        ret = ibv_get_cq_event(_cq->channel, &ibcq, &context);
        if (!ret)
                ibv_ack_cq_events(ibcq, 1);

        return -ibv_req_notify_cq(_cq->cq, (_cq->flags & FI_REMOTE_SIGNAL) ? 1:0);
}

static ssize_t
fi_ibv_cq_sread(struct fid_cq *cq, void *buf, size_t count, const void *cond,
		int timeout)
{
	ssize_t ret = 0, cur;
	ssize_t  threshold;
	int reset = 1;
	struct fi_ibv_cq *_cq;

	_cq = container_of(cq, struct fi_ibv_cq, cq_fid);
	threshold = (_cq->wait_cond == FI_CQ_COND_THRESHOLD) ?
		MIN((ssize_t) cond, count) : 1;

	for (cur = 0; cur < threshold; ) {
		ret = _cq->cq_fid.ops->read(cq, buf, count - cur);
		if (ret < 0 || !_cq->channel)
			break;

		if (ret > 0) {
			buf += ret * _cq->entry_size;
			cur += ret;
		}

		if (cur >= threshold)
			break;

		if (reset) {
			fi_ibv_cq_reset(cq, NULL);
			reset = 0;
			continue;
		}
		ret = fi_poll_fd(_cq->channel->fd, timeout);
		if (ret == 0)
			return -FI_ETIMEDOUT;
		else if (ret < 0)
			break;
	}

	return cur ? cur : ret;
}

static uint64_t fi_ibv_comp_flags(struct ibv_wc *wc)
{
	uint64_t flags = 0;

	if (wc->wc_flags & IBV_WC_WITH_IMM)
		flags |= FI_REMOTE_CQ_DATA;

	switch (wc->opcode) {
	case IBV_WC_SEND:
		flags |= FI_SEND | FI_MSG;
		break;
	case IBV_WC_RDMA_WRITE:
		flags |= FI_RMA | FI_WRITE;
		break;
	case IBV_WC_RDMA_READ:
		flags |= FI_RMA | FI_READ;
		break;
	case IBV_WC_COMP_SWAP:
		flags |= FI_ATOMIC;
		break;
	case IBV_WC_FETCH_ADD:
		flags |= FI_ATOMIC;
		break;
	case IBV_WC_RECV:
		flags |= FI_RECV | FI_MSG;
		break;
	case IBV_WC_RECV_RDMA_WITH_IMM:
		flags |= FI_RMA | FI_REMOTE_WRITE;
		break;
	default:
		break;
	}
	return flags;
}

static ssize_t fi_ibv_cq_read_context(struct fid_cq *cq, void *buf, size_t count)
{
	struct fi_ibv_cq *_cq;
	struct fi_cq_entry *entry = buf;
	ssize_t ret = 0, i;

	_cq = container_of(cq, struct fi_ibv_cq, cq_fid);
	if (_cq->wc.status)
		return -FI_EAVAIL;

	for (i = 0; i < count; i++) {
		ret = ibv_poll_cq(_cq->cq, 1, &_cq->wc);
		if (ret <= 0)
			break;

		if (_cq->wc.status) {
			ret = -FI_EAVAIL;
			break;
		}

		entry->op_context = (void *) (uintptr_t) _cq->wc.wr_id;
		entry += 1;
	}

	return i ? i : ret;
}

static ssize_t fi_ibv_cq_read_msg(struct fid_cq *cq, void *buf, size_t count)
{
	struct fi_ibv_cq *_cq;
	struct fi_cq_msg_entry *entry = buf;
	ssize_t ret = 0, i;

	_cq = container_of(cq, struct fi_ibv_cq, cq_fid);
	if (_cq->wc.status)
		return -FI_EAVAIL;

	for (i = 0; i < count; i++) {
		ret = ibv_poll_cq(_cq->cq, 1, &_cq->wc);
		if (ret <= 0)
			break;

		if (_cq->wc.status) {
			ret = -FI_EAVAIL;
			break;
		}

		entry->op_context = (void *) (uintptr_t) _cq->wc.wr_id;
		entry->flags = fi_ibv_comp_flags(&_cq->wc);
		entry->len = (uint64_t) _cq->wc.byte_len;
		entry += 1;
	}

	return i ? i : ret;
}

static ssize_t fi_ibv_cq_read_data(struct fid_cq *cq, void *buf, size_t count)
{
	struct fi_ibv_cq *_cq;
	struct fi_cq_data_entry *entry = buf;
	ssize_t ret = 0, i;

	_cq = container_of(cq, struct fi_ibv_cq, cq_fid);
	if (_cq->wc.status)
		return -FI_EAVAIL;

	for (i = 0; i < count; i++) {
		ret = ibv_poll_cq(_cq->cq, 1, &_cq->wc);
		if (ret <= 0)
			break;

		if (_cq->wc.status) {
			ret = -FI_EAVAIL;
			break;
		}

		entry->op_context = (void *) (uintptr_t) _cq->wc.wr_id;
		entry->flags = fi_ibv_comp_flags(&_cq->wc);
		if (_cq->wc.wc_flags & IBV_WC_WITH_IMM) {
			entry->data = _cq->wc.imm_data;
		} else {
			entry->data = 0;
		}
		if (_cq->wc.opcode & (IBV_WC_RECV | IBV_WC_RECV_RDMA_WITH_IMM))
			entry->len = _cq->wc.byte_len;
		else
			entry->len = 0;

		entry += 1;
	}

	return i ? i : ret;
}

static const char *
fi_ibv_cq_strerror(struct fid_cq *eq, int prov_errno, const void *err_data,
		   char *buf, size_t len)
{
	if (buf && len)
		strncpy(buf, ibv_wc_status_str(prov_errno), len);
	return ibv_wc_status_str(prov_errno);
}

static struct fi_ops_cq fi_ibv_cq_context_ops = {
	.size = sizeof(struct fi_ops_cq),
	.read = fi_ibv_cq_read_context,
	.readfrom = fi_no_cq_readfrom,
	.readerr = fi_ibv_cq_readerr,
	.write = fi_no_cq_write,
	.writeerr = fi_no_cq_writeerr,
	.sread = fi_ibv_cq_sread,
	.strerror = fi_ibv_cq_strerror
};

static struct fi_ops_cq fi_ibv_cq_msg_ops = {
	.size = sizeof(struct fi_ops_cq),
	.read = fi_ibv_cq_read_msg,
	.readfrom = fi_no_cq_readfrom,
	.readerr = fi_ibv_cq_readerr,
	.write = fi_no_cq_write,
	.writeerr = fi_no_cq_writeerr,
	.sread = fi_ibv_cq_sread,
	.strerror = fi_ibv_cq_strerror
};

static struct fi_ops_cq fi_ibv_cq_data_ops = {
	.size = sizeof(struct fi_ops_cq),
	.read = fi_ibv_cq_read_data,
	.readfrom = fi_no_cq_readfrom,
	.readerr = fi_ibv_cq_readerr,
	.write = fi_no_cq_write,
	.writeerr = fi_no_cq_writeerr,
	.sread = fi_ibv_cq_sread,
	.strerror = fi_ibv_cq_strerror
};

static int fi_ibv_cq_control(fid_t fid, int command, void *arg)
{
	struct fi_ibv_cq *cq;
	int ret = 0;

	cq = container_of(fid, struct fi_ibv_cq, cq_fid.fid);
	switch(command) {
	case FI_GETWAIT:
		if (!cq->channel) {
			ret = -FI_ENODATA;
			break;
		}
		*(int *) arg = cq->channel->fd;
		break;
	default:
		ret = -FI_ENOSYS;
		break;
	}

	return ret;
}

static int fi_ibv_cq_close(fid_t fid)
{
	struct fi_ibv_cq *cq;
	int ret;

	cq = container_of(fid, struct fi_ibv_cq, cq_fid.fid);
	if (cq->cq) {
		ret = ibv_destroy_cq(cq->cq);
		if (ret)
			return -ret;
	}

	if (cq->channel)
		ibv_destroy_comp_channel(cq->channel);

	free(cq);
	return 0;
}

static struct fi_ops fi_ibv_cq_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = fi_ibv_cq_close,
	.bind = fi_no_bind,
	.control = fi_ibv_cq_control,
	.ops_open = fi_no_ops_open,
};

static int
fi_ibv_cq_open(struct fid_domain *domain, struct fi_cq_attr *attr,
	    struct fid_cq **cq, void *context)
{
	struct fi_ibv_cq *_cq;
	long flags = 0;
	int ret;

	_cq = calloc(1, sizeof *_cq);
	if (!_cq)
		return -FI_ENOMEM;

	_cq->domain = container_of(domain, struct fi_ibv_domain, domain_fid);

	switch (attr->wait_obj) {
	case FI_WAIT_UNSPEC:
	case FI_WAIT_FD:
		_cq->channel = ibv_create_comp_channel(_cq->domain->verbs);
		if (!_cq->channel) {
			ret = -errno;
			goto err1;
		}

		flags = fcntl(_cq->channel->fd, F_GETFL);
		if (flags < 0) {
			ret = -errno;
			goto err2;
		}
		ret = fcntl(_cq->channel->fd, F_SETFL, flags | O_NONBLOCK);
		if (ret) {
			ret = -errno;
			goto err2;
		}
		break;
	case FI_WAIT_NONE:
		break;
	default:
		ret = -FI_ENOSYS;
		goto err1;
	}

	_cq->cq = ibv_create_cq(_cq->domain->verbs, attr->size, _cq,
				_cq->channel, attr->signaling_vector);
	if (!_cq->cq) {
		ret = -errno;
		goto err2;
	}

	_cq->flags |= attr->flags;
	_cq->wait_cond = attr->wait_cond;
	_cq->cq_fid.fid.fclass = FI_CLASS_CQ;
	_cq->cq_fid.fid.context = context;
	_cq->cq_fid.fid.ops = &fi_ibv_cq_fi_ops;

	switch (attr->format) {
	case FI_CQ_FORMAT_CONTEXT:
		_cq->cq_fid.ops = &fi_ibv_cq_context_ops;
		_cq->entry_size = sizeof(struct fi_cq_entry);
		break;
	case FI_CQ_FORMAT_MSG:
		_cq->cq_fid.ops = &fi_ibv_cq_msg_ops;
		_cq->entry_size = sizeof(struct fi_cq_msg_entry);
		break;
	case FI_CQ_FORMAT_DATA:
		_cq->cq_fid.ops = &fi_ibv_cq_data_ops;
		_cq->entry_size = sizeof(struct fi_cq_data_entry);
		break;
	default:
		ret = -FI_ENOSYS;
		goto err3;
	}

	*cq = &_cq->cq_fid;
	return 0;

err3:
	ibv_destroy_cq(_cq->cq);
err2:
	if (_cq->channel)
		ibv_destroy_comp_channel(_cq->channel);
err1:
	free(_cq);
	return ret;
}

static int fi_ibv_mr_close(fid_t fid)
{
	struct fi_ibv_mem_desc *mr;
	int ret;

	mr = container_of(fid, struct fi_ibv_mem_desc, mr_fid.fid);
	ret = -ibv_dereg_mr(mr->mr);
	if (!ret)
		free(mr);
	return ret;
}

static struct fi_ops fi_ibv_mr_ops = {
	.size = sizeof(struct fi_ops),
	.close = fi_ibv_mr_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static int
fi_ibv_mr_reg(struct fid *fid, const void *buf, size_t len,
	   uint64_t access, uint64_t offset, uint64_t requested_key,
	   uint64_t flags, struct fid_mr **mr, void *context)
{
	struct fi_ibv_mem_desc *md;
	int fi_ibv_access;
	struct fid_domain *domain;

	if (flags)
		return -FI_EBADFLAGS;

	if (fid->fclass != FI_CLASS_DOMAIN) {
		return -FI_EINVAL;
	}
	domain = container_of(fid, struct fid_domain, fid);

	md = calloc(1, sizeof *md);
	if (!md)
		return -FI_ENOMEM;

	md->domain = container_of(domain, struct fi_ibv_domain, domain_fid);
	md->mr_fid.fid.fclass = FI_CLASS_MR;
	md->mr_fid.fid.context = context;
	md->mr_fid.fid.ops = &fi_ibv_mr_ops;

	fi_ibv_access = IBV_ACCESS_LOCAL_WRITE;
	if (access & FI_REMOTE_READ)
		fi_ibv_access |= IBV_ACCESS_REMOTE_READ;
	if (access & FI_REMOTE_WRITE)
		fi_ibv_access |= IBV_ACCESS_REMOTE_WRITE;
	if ((access & FI_READ) || (access & FI_WRITE))
		fi_ibv_access |= IBV_ACCESS_REMOTE_ATOMIC;

	md->mr = ibv_reg_mr(md->domain->pd, (void *) buf, len, fi_ibv_access);
	if (!md->mr)
		goto err;

	md->mr_fid.mem_desc = (void *) (uintptr_t) md->mr->lkey;
	md->mr_fid.key = md->mr->rkey;
	*mr = &md->mr_fid;
	return 0;

err:
	free(md);
	return -errno;
}

static int fi_ibv_close(fid_t fid)
{
	struct fi_ibv_domain *domain;
	int ret;

	domain = container_of(fid, struct fi_ibv_domain, domain_fid.fid);
	if (domain->pd) {
		ret = ibv_dealloc_pd(domain->pd);
		if (ret)
			return -ret;
		domain->pd = NULL;
	}

	free(domain);
	return 0;
}

static int fi_ibv_open_device_by_name(struct fi_ibv_domain *domain, const char *name)
{
	struct ibv_context **dev_list;
	int i, ret = -FI_ENODEV;

	if (!name)
		return -FI_EINVAL;

	dev_list = rdma_get_devices(NULL);
	if (!dev_list)
		return -errno;

	for (i = 0; dev_list[i]; i++) {
		if (!strcmp(name, ibv_get_device_name(dev_list[i]->device))) {
			domain->verbs = dev_list[i];
			ret = 0;
			break;
		}
	}
	rdma_free_devices(dev_list);
	return ret;
}

static struct fi_ops fi_ibv_fid_ops = {
	.size = sizeof(struct fi_ops),
	.close = fi_ibv_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_mr fi_ibv_domain_mr_ops = {
	.size = sizeof(struct fi_ops_mr),
	.reg = fi_ibv_mr_reg,
	.regv = fi_no_mr_regv,
	.regattr = fi_no_mr_regattr,
};

static struct fi_ops_domain fi_ibv_domain_ops = {
	.size = sizeof(struct fi_ops_domain),
	.av_open = fi_no_av_open,
	.cq_open = fi_ibv_cq_open,
	.endpoint = fi_ibv_open_ep,
	.cntr_open = fi_no_cntr_open,
	.poll_open = fi_no_poll_open,
};

static int
fi_ibv_domain(struct fid_fabric *fabric, struct fi_info *info,
	   struct fid_domain **domain, void *context)
{
	struct fi_ibv_domain *_domain;
	int ret;

	_domain = calloc(1, sizeof *_domain);
	if (!_domain)
		return -FI_ENOMEM;

	ret = fi_ibv_open_device_by_name(_domain, info->domain_attr->name);
	if (ret)
		goto err;

	_domain->pd = ibv_alloc_pd(_domain->verbs);
	if (!_domain->pd) {
		ret = -errno;
		goto err;
	}

	_domain->domain_fid.fid.fclass = FI_CLASS_DOMAIN;
	_domain->domain_fid.fid.context = context;
	_domain->domain_fid.fid.ops = &fi_ibv_fid_ops;
	_domain->domain_fid.ops = &fi_ibv_domain_ops;
	_domain->domain_fid.mr = &fi_ibv_domain_mr_ops;

	*domain = &_domain->domain_fid;
	return 0;
err:
	free(_domain);
	return ret;
}

static int fi_ibv_pep_getname(fid_t pep, void *addr, size_t *addrlen)
{
	struct fi_ibv_pep *_pep;
	struct sockaddr *sa;

	_pep = container_of(pep, struct fi_ibv_pep, pep_fid);
	sa = rdma_get_local_addr(_pep->id);
	return fi_ibv_copy_addr(addr, addrlen, sa);
}

static int fi_ibv_pep_listen(struct fid_pep *pep)
{
	struct fi_ibv_pep *_pep;

	_pep = container_of(pep, struct fi_ibv_pep, pep_fid);
	return rdma_listen(_pep->id, 0) ? -errno : 0;
}

static struct fi_ops_cm fi_ibv_pep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = fi_ibv_pep_getname,
	.getpeer = fi_no_getpeer,
	.connect = fi_no_connect,
	.listen = fi_ibv_pep_listen,
	.accept = fi_no_accept,
	.reject = fi_ibv_msg_ep_reject,
	.shutdown = fi_no_shutdown,
};

static int fi_ibv_pep_bind(fid_t fid, struct fid *bfid, uint64_t flags)
{
	struct fi_ibv_pep *pep;
	int ret;

	pep = container_of(fid, struct fi_ibv_pep, pep_fid.fid);
	if (bfid->fclass != FI_CLASS_EQ)
		return -FI_EINVAL;

	pep->eq = container_of(bfid, struct fi_ibv_eq, eq_fid.fid);
	ret = rdma_migrate_id(pep->id, pep->eq->channel);
	if (ret)
		return -errno;

	return 0;
}

static int fi_ibv_pep_close(fid_t fid)
{
	struct fi_ibv_pep *pep;

	pep = container_of(fid, struct fi_ibv_pep, pep_fid.fid);
	if (pep->id)
		rdma_destroy_ep(pep->id);

	free(pep);
	return 0;
}

static struct fi_ops fi_ibv_pep_ops = {
	.size = sizeof(struct fi_ops),
	.close = fi_ibv_pep_close,
	.bind = fi_ibv_pep_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static int
fi_ibv_passive_ep(struct fid_fabric *fabric, struct fi_info *info,
	      struct fid_pep **pep, void *context)
{
	struct fi_ibv_pep *_pep;
	int ret;

	_pep = calloc(1, sizeof *_pep);
	if (!_pep)
		return -FI_ENOMEM;

	ret = fi_ibv_create_ep(NULL, NULL, FI_SOURCE, info, NULL, &_pep->id);
	if (ret)
		goto err;

	_pep->id->context = &_pep->pep_fid.fid;

	_pep->pep_fid.fid.fclass = FI_CLASS_PEP;
	_pep->pep_fid.fid.context = context;
	_pep->pep_fid.fid.ops = &fi_ibv_pep_ops;
	_pep->pep_fid.cm = &fi_ibv_pep_cm_ops;

	*pep = &_pep->pep_fid;
	return 0;
err:
	free(_pep);
	return ret;
}

static int fi_ibv_fabric_close(fid_t fid)
{
	free(fid);
	return 0;
}

static struct fi_ops fi_ibv_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = fi_ibv_fabric_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_fabric fi_ibv_ops_fabric = {
	.size = sizeof(struct fi_ops_fabric),
	.domain = fi_ibv_domain,
	.passive_ep = fi_ibv_passive_ep,
	.eq_open = fi_ibv_eq_open,
	.wait_open = fi_no_wait_open,
};

int fi_ibv_fabric(struct fi_fabric_attr *attr, struct fid_fabric **fabric, void *context)
{
	struct fi_ibv_fabric *fab;
	int ret;

	ret = fi_ibv_check_fabric_attr(attr);
	if (ret)
		return -FI_ENODATA;

	fab = calloc(1, sizeof(*fab));
	if (!fab)
		return -FI_ENOMEM;

	fab->fabric_fid.fid.fclass = FI_CLASS_FABRIC;
	fab->fabric_fid.fid.context = context;
	fab->fabric_fid.fid.ops = &fi_ibv_fi_ops;
	fab->fabric_fid.ops = &fi_ibv_ops_fabric;
	*fabric = &fab->fabric_fid;
	return 0;
}

static void fi_ibv_fini(void)
{
}

static struct fi_provider fi_ibv_prov = {
	.name = VERBS_PROV_NAME,
	.version = VERBS_PROV_VERS,
	.fi_version = FI_VERSION(FI_MAJOR_VERSION, FI_MINOR_VERSION),
	.getinfo = fi_ibv_getinfo,
	.fabric = fi_ibv_fabric,
	.cleanup = fi_ibv_fini
};

VERBS_INI
{
	fi_log_init();
	return &fi_ibv_prov;
}
