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
#include <string.h>

#include "sock.h"
#include "sock_util.h"


static int sock_dom_close(struct fid *fid)
{
	struct sock_domain *dom;

	dom = container_of(fid, struct sock_domain, dom_fid.fid);
	if (atomic_get(&dom->ref))
		return -FI_EBUSY;

	fastlock_destroy(&dom->lock);
	free(dom);
	return 0;
}

//static int sock_dom_query(struct fid_domain *domain, struct fi_domain_attr *attr)
//{
//	attr->mr_key_size = 2; /* IDX_MAX_INDEX bits */
//	attr->eq_data_size = sizeof(uint64_t);
//	return 0;
//}

static uint16_t sock_get_mr_key(struct sock_domain *dom)
{
	uint16_t i;

	for (i = 1; i < IDX_MAX_INDEX; i++) {
		if (!idm_lookup(&dom->mr_idm, i))
			return i;
	}
	return 0;
}

static int sock_mr_close(struct fid *fid)
{
	struct sock_domain *dom;
	struct sock_mr *mr;

	mr = container_of(fid, struct sock_mr, mr_fid.fid);
	dom = mr->dom;
	fastlock_acquire(&dom->lock);
	idm_clear(&dom->mr_idm , (int) mr->mr_fid.key);
	fastlock_release(&dom->lock);
	atomic_dec(&dom->ref);
	free(mr);
	return 0;
}

static struct fi_ops sock_mr_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_mr_close,
	.bind = fi_no_bind,
	.sync = fi_no_sync,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static int sock_regattr(struct fid_domain *domain, const struct fi_mr_attr *attr,
		uint64_t flags, struct fid_mr **mr)
{
	struct sock_domain *dom;
	struct sock_mr *_mr;
	uint16_t key;

	dom = container_of(domain, struct sock_domain, dom_fid);
	if (!(dom->mode & FI_PROV_MR_KEY) && ((attr->requested_key > IDX_MAX_INDEX) ||
	    idm_lookup(&dom->mr_idm, (int) attr->requested_key)))
		return -FI_ENOKEY;

	_mr = calloc(1, sizeof(*_mr) + sizeof(_mr->mr_iov) * (attr->iov_count - 1));
	if (!_mr)
		return -FI_ENOMEM;

	_mr->mr_fid.fid.fclass = FI_CLASS_MR;
	_mr->mr_fid.fid.context = attr->context;
	_mr->mr_fid.fid.ops = &sock_mr_fi_ops;

	atomic_inc(&dom->ref);
	_mr->dom = dom;
	_mr->access = attr->access;
	_mr->offset = (flags & FI_MR_OFFSET) ?
		      attr->offset : (uintptr_t) attr->mr_iov[0].iov_base;

	fastlock_acquire(&dom->lock);
	key = (dom->mode & FI_PROV_MR_KEY) ?
	      sock_get_mr_key(dom) : (uint16_t) attr->requested_key;
	if (idm_set(&dom->mr_idm, key, _mr) < 0)
		goto err;
	_mr->mr_fid.key = key;
	fastlock_release(&dom->lock);

	_mr->iov_count = attr->iov_count;
	memcpy(&_mr->mr_iov, attr->mr_iov, sizeof(_mr->mr_iov) * attr->iov_count);

	*mr = &_mr->mr_fid;
	/* TODO: async */
	return 0;

err:
	fastlock_release(&dom->lock);
	atomic_dec(&dom->ref);
	free(_mr);
	return -errno;
}

static int sock_regv(struct fid_domain *domain, const struct iovec *iov,
		size_t count, uint64_t access,
		uint64_t offset, uint64_t requested_key,
		uint64_t flags, struct fid_mr **mr, void *context)
{
	struct fi_mr_attr attr;

	attr.mr_iov = iov;
	attr.iov_count = count;
	attr.access = access;
	attr.offset = offset;
	attr.requested_key = requested_key;
	attr.context = context;
	return sock_regattr(domain, &attr, flags, mr);
}

static int sock_reg(struct fid_domain *domain, const void *buf, size_t len,
		uint64_t access, uint64_t offset, uint64_t requested_key,
		uint64_t flags, struct fid_mr **mr, void *context)
{
	struct iovec iov;

	iov.iov_base = (void *) buf;
	iov.iov_len = len;
	return sock_regv(domain, &iov, 1, access,  offset, requested_key,
			 flags, mr, context);
}

int sock_dom_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	return -FI_ENOSYS;
}

int sock_dom_sync(struct fid *fid, uint64_t flags, void *context)
{
	return -FI_ENOSYS;
}

int sock_dom_control(struct fid *fid, int command, void *arg)
{
	return -FI_ENOSYS;
}

int sock_dom_ops_open(struct fid *fid, const char *name,
			uint64_t flags, void **ops, void *context)
{
	return -FI_ENOSYS;
}

int sock_endpoint(struct fid_domain *domain, struct fi_info *info,
			 struct fid_ep **ep, void *context)
{
	switch (info->ep_type) {
	case FI_EP_RDM:
		return sock_rdm_ep(domain, info, ep, context);
	case FI_EP_DGRAM:
		return sock_dgram_ep(domain, info, ep, context);
	default:
		return -FI_ENOPROTOOPT;
	}
}

static struct fi_ops sock_dom_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_dom_close,
	.bind = fi_no_bind,
	.sync = fi_no_sync,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_domain sock_dom_ops = {
	.size = sizeof(struct fi_ops_domain),
	.av_open = sock_av_open,
	.cq_open = sock_cq_open,
	.endpoint = sock_endpoint,
	.cntr_open = sock_cntr_open,
	.wait_open = sock_wait_open,
	.poll_open = sock_poll_open,
};

static struct fi_ops_mr sock_dom_mr_ops = {
	.size = sizeof(struct fi_ops_mr),
	.reg = sock_reg,
	.regv = sock_regv,
	.regattr = sock_regattr,
};

int _sock_verify_domain_attr(struct fi_domain_attr *attr)
{
	if(attr->name){
		if (strcmp(attr->name, sock_dom_name))
			return -FI_ENODATA;
	}

	switch(attr->threading){
	case FI_THREAD_UNSPEC:
	case FI_THREAD_SAFE:
	case FI_THREAD_PROGRESS:
		break;
	default:
		sock_debug(SOCK_INFO, "Invalid threading model!\n");
		return -FI_ENODATA;
	}

	switch (attr->control_progress){
	case FI_PROGRESS_UNSPEC:
	case FI_PROGRESS_AUTO:
		break;

	case FI_PROGRESS_MANUAL:
	default:
		sock_debug(SOCK_INFO, "Control progress mode not supported!\n");
		return -FI_ENODATA;
	}

	switch (attr->data_progress){
	case FI_PROGRESS_UNSPEC:
	case FI_PROGRESS_AUTO:
		break;

	case FI_PROGRESS_MANUAL:
	default:
		sock_debug(SOCK_INFO, "Data progress mode not supported!\n");
		return -FI_ENODATA;
	}

	if(attr->max_ep_tx_ctx > SOCK_EP_TX_CTX_CNT)
		return -FI_ENODATA;

	if(attr->max_ep_rx_ctx > SOCK_EP_RX_CTX_CNT)
		return -FI_ENODATA;

	return 0;
}

int sock_domain(struct fid_fabric *fabric, struct fi_info *info,
		struct fid_domain **dom, void *context)
{
	int ret;
	struct sock_domain *sock_domain;

	if(info && info->domain_attr){
		ret = _sock_verify_domain_attr(info->domain_attr);
		if(ret)
			return ret;
	}

	sock_domain = calloc(1, sizeof *sock_domain);
	if (!sock_domain)
		return -FI_ENOMEM;
	
	fastlock_init(&sock_domain->lock);
	atomic_init(&sock_domain->ref, 0);

	sock_domain->dom_fid.fid.fclass = FI_CLASS_DOMAIN;
	sock_domain->dom_fid.fid.context = context;
	sock_domain->dom_fid.fid.ops = &sock_dom_fi_ops;
	sock_domain->dom_fid.ops = &sock_dom_ops;
	sock_domain->dom_fid.mr = &sock_dom_mr_ops;

	*dom = &sock_domain->dom_fid;
	return 0;
}
