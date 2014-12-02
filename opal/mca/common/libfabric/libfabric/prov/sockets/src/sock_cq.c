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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/types.h>

#include <fi_list.h>

#include "sock.h"
#include "sock_util.h"


static ssize_t sock_cq_entry_size(struct sock_cq *sock_cq)
{
	ssize_t size;

	switch(sock_cq->attr.format) {
	case FI_CQ_FORMAT_CONTEXT:
		size = sizeof(struct fi_cq_entry);
		break;

	case FI_CQ_FORMAT_MSG:
		size = sizeof(struct fi_cq_msg_entry);
		break;

	case FI_CQ_FORMAT_DATA:
		size = sizeof(struct fi_cq_data_entry);
		break;

	case FI_CQ_FORMAT_TAGGED:
		size = sizeof(struct fi_cq_tagged_entry);
		break;

	case FI_CQ_FORMAT_UNSPEC:
	default:
		size = -1;
		sock_debug(SOCK_ERROR, "CQ: Invalid CQ format\n");
		break;
	}
	return size;
}

static ssize_t _sock_cq_write(struct sock_cq *cq, fi_addr_t addr,
			      const void *buf, size_t len)
{
	ssize_t ret;

	fastlock_acquire(&cq->lock);

	if(rbfdavail(&cq->cq_rbfd) < len) {
		ret = -FI_ENOSPC;
		goto out;
	}

	rbfdwrite(&cq->cq_rbfd, buf, len);
	rbfdcommit(&cq->cq_rbfd);
	ret = len;

	rbwrite(&cq->addr_rb, &addr, sizeof(fi_addr_t));
	rbcommit(&cq->addr_rb);

out:
	fastlock_release(&cq->lock);
	return ret;
}

static ssize_t _sock_cq_writeerr(struct sock_cq *cq, 
				 struct fi_cq_err_entry *buf, size_t len)
{
	ssize_t ret;
	
	fastlock_acquire(&cq->lock);
	if(rbavail(&cq->cqerr_rb) < len) {
		ret = -FI_ENOSPC;
		goto out;
	}

	rbwrite(&cq->cqerr_rb, buf, len);
	rbcommit(&cq->cqerr_rb);
	ret = len;

out:
	fastlock_release(&cq->lock);
	return ret;
}


static int sock_cq_report_context(struct sock_cq *cq, fi_addr_t addr,
				  struct sock_pe_entry *pe_entry)
{
	struct fi_cq_entry cq_entry;
	cq_entry.op_context = (void*)pe_entry->context;
	return _sock_cq_write(cq, addr, &cq_entry, sizeof(cq_entry));
}

static int sock_cq_report_msg(struct sock_cq *cq, fi_addr_t addr,
			      struct sock_pe_entry *pe_entry)
{
	struct fi_cq_msg_entry cq_entry;
	cq_entry.op_context = (void*)pe_entry->context;
	cq_entry.flags = pe_entry->flags;
	cq_entry.len = pe_entry->done_len;
	return _sock_cq_write(cq, addr, &cq_entry, sizeof(cq_entry));
}

static int sock_cq_report_data(struct sock_cq *cq, fi_addr_t addr,
			       struct sock_pe_entry *pe_entry)
{
	struct fi_cq_data_entry cq_entry;
	cq_entry.op_context = (void*)pe_entry->context;
	cq_entry.flags = pe_entry->flags;
	cq_entry.len = pe_entry->done_len;
	cq_entry.buf = (void*)pe_entry->rx.rx_iov[0].iov.addr;
	cq_entry.data = pe_entry->data;
	return _sock_cq_write(cq, addr, &cq_entry, sizeof(cq_entry));
}

static int sock_cq_report_tagged(struct sock_cq *cq, fi_addr_t addr,
				 struct sock_pe_entry *pe_entry)
{
	struct fi_cq_tagged_entry cq_entry;
	cq_entry.op_context = (void*)pe_entry->context;
	cq_entry.flags = pe_entry->flags;
	cq_entry.len = pe_entry->done_len;
	cq_entry.buf = (void*)pe_entry->rx.rx_iov[0].iov.addr;
	cq_entry.data = pe_entry->data;
	cq_entry.tag = pe_entry->tag;
	return _sock_cq_write(cq, addr, &cq_entry, sizeof(cq_entry));
}

static void sock_cq_set_report_fn(struct sock_cq *sock_cq)
{
	switch(sock_cq->attr.format) {
	case FI_CQ_FORMAT_CONTEXT:
		sock_cq->report_completion = &sock_cq_report_context;
		break;

	case FI_CQ_FORMAT_MSG:
		sock_cq->report_completion = &sock_cq_report_msg;
		break;

	case FI_CQ_FORMAT_DATA:
		sock_cq->report_completion = &sock_cq_report_data;
		break;

	case FI_CQ_FORMAT_TAGGED:
		sock_cq->report_completion = &sock_cq_report_tagged;
		break;

	case FI_CQ_FORMAT_UNSPEC:
	default:
		sock_debug(SOCK_ERROR, "CQ: Invalid CQ format\n");
		break;
	}
}

ssize_t sock_cq_sreadfrom(struct fid_cq *cq, void *buf, size_t count,
			fi_addr_t *src_addr, const void *cond, int timeout)
{
	int ret;
	fi_addr_t addr;
	int64_t threshold;
	ssize_t i, bytes_read, num_read, cq_entry_len;
	struct sock_cq *sock_cq;
	
	sock_cq = container_of(cq, struct sock_cq, cq_fid);
	cq_entry_len = sock_cq->cq_entry_size;

	if (sock_cq->attr.wait_cond == FI_CQ_COND_THRESHOLD) {
		threshold = MIN((int64_t)cond, count);
	}else{
		threshold = count;
	}

	fastlock_acquire(&sock_cq->lock);
	bytes_read = rbfdsread(&sock_cq->cq_rbfd, buf, 
			       cq_entry_len*threshold, timeout);

	if(bytes_read == 0) {
		ret = -FI_ETIMEDOUT;
		goto out;
	}

	num_read = bytes_read/cq_entry_len;
	for(i=0; i < num_read; i++) {
		rbread(&sock_cq->addr_rb, &addr, sizeof(fi_addr_t));
		if(src_addr)
			src_addr[i] = addr;
	}
	ret = num_read;

out:
	fastlock_release(&sock_cq->lock);
	return ret;
}

ssize_t sock_cq_sread(struct fid_cq *cq, void *buf, size_t len,
			     const void *cond, int timeout)
{
	return sock_cq_sreadfrom(cq, buf, len, NULL, cond, timeout);
}

ssize_t sock_cq_readfrom(struct fid_cq *cq, void *buf, size_t count,
			fi_addr_t *src_addr)
{
	int ret;
	ret = sock_cq_sreadfrom(cq, buf, count, src_addr, NULL, 0);
	return (ret == -FI_ETIMEDOUT) ? 0 : ret;
}

ssize_t sock_cq_read(struct fid_cq *cq, void *buf, size_t count)
{
	return sock_cq_readfrom(cq, buf, count, NULL);
}


ssize_t sock_cq_readerr(struct fid_cq *cq, struct fi_cq_err_entry *buf,
			size_t len, uint64_t flags)
{
	ssize_t num_read;
	struct sock_cq *sock_cq;
	
	sock_cq = container_of(cq, struct sock_cq, cq_fid);
	if(len < sizeof(struct fi_cq_err_entry))
		return -FI_ETOOSMALL;

	num_read = 0;
	fastlock_acquire(&sock_cq->lock);

	while(rbused(&sock_cq->cqerr_rb) >= sizeof(struct fi_cq_err_entry)) {
		rbread(&sock_cq->cqerr_rb, 
		       (char*)buf +sizeof(struct fi_cq_err_entry) * num_read, 
		       sizeof(struct fi_cq_err_entry));
		num_read++;
	}

	fastlock_release(&sock_cq->lock);
	return num_read;
}

ssize_t sock_cq_write(struct fid_cq *cq, const void *buf, size_t len)
{
	struct sock_cq *sock_cq;
	
	sock_cq = container_of(cq, struct sock_cq, cq_fid);
	if(!(sock_cq->attr.flags & FI_WRITE))
		return -FI_EINVAL;

	return _sock_cq_write(sock_cq, FI_ADDR_UNSPEC, buf, len);
}

ssize_t sock_cq_writeerr(struct fid_cq *cq, struct fi_cq_err_entry *buf,
			size_t len, uint64_t flags)
{
	struct sock_cq *sock_cq;
	
	sock_cq = container_of(cq, struct sock_cq, cq_fid);
	if(!(sock_cq->attr.flags & FI_WRITE))
		return -FI_EINVAL;

	return _sock_cq_writeerr(sock_cq, buf, len);
}

const char * sock_cq_strerror(struct fid_cq *cq, int prov_errno,
			      const void *err_data, void *buf, size_t len)
{
	if (buf && len)
		return strncpy(buf, strerror(prov_errno), len);
	return strerror(prov_errno);
}

int sock_cq_close(struct fid *fid)
{
	struct sock_cq *cq;

	cq = container_of(fid, struct sock_cq, cq_fid.fid);
	if (atomic_get(&cq->ref))
		return -FI_EBUSY;

	rbfree(&cq->addr_rb);
	rbfree(&cq->cqerr_rb);
	rbfdfree(&cq->cq_rbfd);

	fastlock_destroy(&cq->lock);
	atomic_dec(&cq->domain->ref);

	free(cq);
	return 0;
}

struct fi_ops_cq sock_cq_ops = {
	.read = sock_cq_read,
	.readfrom = sock_cq_readfrom,
	.readerr = sock_cq_readerr,
	.write = sock_cq_write,
	.writeerr = sock_cq_writeerr,
	.sread = sock_cq_sread,
	.sreadfrom = sock_cq_sreadfrom,
	.strerror = sock_cq_strerror,
};

struct fi_ops sock_cq_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_cq_close,
};

static int sock_cq_verify_attr(struct fi_cq_attr *attr)
{
	if(!attr)
		return 0;

	switch (attr->format) {
	case FI_CQ_FORMAT_CONTEXT:
	case FI_CQ_FORMAT_MSG:
	case FI_CQ_FORMAT_DATA:
	case FI_CQ_FORMAT_TAGGED:
		break;
	default:
		return -FI_ENOSYS;
	}

	switch (attr->wait_obj) {
	case FI_WAIT_NONE:
	case FI_WAIT_FD:
		break;
	case FI_WAIT_UNSPEC:
		attr->wait_obj = FI_WAIT_FD;
		break;
	default:
		return -FI_ENOSYS;
	}

	return 0;
}

static struct fi_cq_attr _sock_cq_def_attr = {
	.size = SOCK_CQ_DEF_LEN,
	.flags = 0,
	.format = FI_CQ_FORMAT_CONTEXT,
	.wait_obj = FI_WAIT_FD,
	.signaling_vector = 0,
	.wait_cond = FI_CQ_COND_NONE,
	.wait_set = NULL,
};

int sock_cq_open(struct fid_domain *domain, struct fi_cq_attr *attr,
		 struct fid_cq **cq, void *context)
{
	struct sock_domain *sock_dom;
	struct sock_cq *sock_cq;
	int ret;

	sock_dom = container_of(domain, struct sock_domain, dom_fid);
	ret = sock_cq_verify_attr(attr);
	if (ret)
		return ret;

	sock_cq = calloc(1, sizeof(*sock_cq));
	if (!sock_cq)
		return -FI_ENOMEM;
	
	atomic_init(&sock_cq->ref, 0);
	sock_cq->cq_fid.fid.fclass = FI_CLASS_CQ;
	sock_cq->cq_fid.fid.context = context;
	sock_cq->cq_fid.fid.ops = &sock_cq_fi_ops;
	sock_cq->cq_fid.ops = &sock_cq_ops;
	atomic_inc(&sock_dom->ref);

	if(attr == NULL)
		memcpy(&sock_cq->attr, &_sock_cq_def_attr, 
		       sizeof(struct fi_cq_attr));
	else
		memcpy(&sock_cq->attr, attr, sizeof(struct fi_cq_attr));
	
	sock_cq->domain = sock_dom;
	sock_cq->cq_entry_size = sock_cq_entry_size(sock_cq);
	sock_cq_set_report_fn(sock_cq);

	dlist_init(&sock_cq->tx_list);
	dlist_init(&sock_cq->rx_list);
	dlist_init(&sock_cq->ep_list);

	if((ret = rbfdinit(&sock_cq->cq_rbfd, sock_cq->attr.size)))
		goto err1;

	if((ret = rbinit(&sock_cq->addr_rb, 
			 (sock_cq->attr.size/sock_cq->cq_entry_size) * sizeof(fi_addr_t))))
		goto err2;
	
	if((ret = rbinit(&sock_cq->cqerr_rb, sock_cq->attr.size)))
		goto err3;

	fastlock_init(&sock_cq->lock);

	*cq = &sock_cq->cq_fid;
	atomic_inc(&sock_dom->ref);
	return 0;

err3:
	rbfree(&sock_cq->addr_rb);
err2:
	rbfdfree(&sock_cq->cq_rbfd);
err1:
	free(sock_cq);
	return ret;
}

int sock_cq_report_error(struct sock_cq *cq, struct sock_pe_entry *entry,
			 size_t olen, int err, int prov_errno, void *err_data)
{
	int ret;
	struct fi_cq_err_entry err_entry;

	fastlock_acquire(&cq->lock);

	if(rbavail(&cq->cqerr_rb) < sizeof(struct fi_cq_err_entry)) {
		ret = -FI_ENOSPC;
		goto out;
	}

	err_entry.err = err;
	err_entry.olen = olen;
	err_entry.err_data = err_data;
	err_entry.len = entry->done_len;
	err_entry.prov_errno = prov_errno;
	err_entry.flags = entry->flags;
	err_entry.data = entry->data;
	err_entry.tag = entry->tag;
	err_entry.op_context = (void*)entry->context;
	
	if(entry->type == SOCK_PE_RX) {
		err_entry.buf = (void*)entry->rx.rx_iov[0].iov.addr;
	}else {
		err_entry.buf = (void*)entry->tx.tx_iov[0].src.iov.addr;
	}

	rbwrite(&cq->cqerr_rb, &err_entry, sizeof(struct fi_cq_err_entry));
	rbcommit(&cq->cqerr_rb);
	ret = 0;

out:
	fastlock_release(&cq->lock);
	return ret;
}

