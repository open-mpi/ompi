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


int sock_cq_progress(struct sock_cq *cq)
{
	struct sock_tx_ctx *tx_ctx;
	struct sock_rx_ctx *rx_ctx;
	struct dlist_entry *entry;

	if (cq->domain->progress_mode == FI_PROGRESS_AUTO && 
	    !sock_progress_thread_wait)
		return 0;

	fastlock_acquire(&cq->list_lock);
	for (entry = cq->tx_list.next; entry != &cq->tx_list;
	     entry = entry->next) {
		tx_ctx = container_of(entry, struct sock_tx_ctx, cq_entry);
		sock_pe_progress_tx_ctx(cq->domain->pe, tx_ctx);
	}

	for (entry = cq->rx_list.next; entry != &cq->rx_list;
	     entry = entry->next) {
		rx_ctx = container_of(entry, struct sock_rx_ctx, cq_entry);
		sock_pe_progress_rx_ctx(cq->domain->pe, rx_ctx);
	}
	fastlock_release(&cq->list_lock);

	return 0;
}

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
		SOCK_LOG_ERROR("Invalid CQ format\n");
		break;
	}
	return size;
}

static ssize_t _sock_cq_write(struct sock_cq *cq, fi_addr_t addr,
			      const void *buf, size_t len)
{
	ssize_t ret;

	fastlock_acquire(&cq->lock);
	if (rbfdavail(&cq->cq_rbfd) < len) {
		ret = -FI_ENOSPC;
		SOCK_LOG_ERROR("Not enough space in CQ\n");
		goto out;
	}


	rbwrite(&cq->addr_rb, &addr, sizeof(fi_addr_t));
	rbcommit(&cq->addr_rb);


	rbfdwrite(&cq->cq_rbfd, buf, len);
	rbfdcommit(&cq->cq_rbfd);
	ret = len;

	if (cq->signal) 
		sock_wait_signal(cq->waitset);
out:
	fastlock_release(&cq->lock);
	return ret;
}

static ssize_t _sock_cq_writeerr(struct sock_cq *cq, 
				 struct fi_cq_err_entry *buf, size_t len)
{
	ssize_t ret;
	
	fastlock_acquire(&cq->lock);
	if (rbavail(&cq->cqerr_rb) < len) {
		ret = -FI_ENOSPC;
		SOCK_LOG_ERROR("Not enough space in CQ\n");
		goto out;
	}

	rbwrite(&cq->cqerr_rb, buf, len);
	rbcommit(&cq->cqerr_rb);
	ret = len;

	if (cq->signal) 
		sock_wait_signal(cq->waitset);
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
	cq_entry.len = pe_entry->data_len;
	return _sock_cq_write(cq, addr, &cq_entry, sizeof(cq_entry));
}

static int sock_cq_report_data(struct sock_cq *cq, fi_addr_t addr,
			       struct sock_pe_entry *pe_entry)
{
	struct fi_cq_data_entry cq_entry;
	cq_entry.op_context = (void*)pe_entry->context;
	cq_entry.flags = pe_entry->flags;
	cq_entry.len = pe_entry->data_len;
	cq_entry.buf = (void*)pe_entry->buf;
	cq_entry.data = pe_entry->data;
	return _sock_cq_write(cq, addr, &cq_entry, sizeof(cq_entry));
}

static int sock_cq_report_tagged(struct sock_cq *cq, fi_addr_t addr,
				 struct sock_pe_entry *pe_entry)
{
	struct fi_cq_tagged_entry cq_entry;
	cq_entry.op_context = (void*)pe_entry->context;
	cq_entry.flags = pe_entry->flags;
	cq_entry.len = pe_entry->data_len;
	cq_entry.buf = (void*)pe_entry->buf;
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
		SOCK_LOG_ERROR("Invalid CQ format\n");
		break;
	}
}

static inline ssize_t sock_cq_rbuf_read(struct sock_cq *cq, void *buf,
					size_t count, fi_addr_t *src_addr,
					size_t cq_entry_len)
{
	ssize_t i;
	fi_addr_t addr;

	rbfdread(&cq->cq_rbfd, buf, cq_entry_len * count);
	for(i = 0; i < count; i++) {
		rbread(&cq->addr_rb, &addr, sizeof(fi_addr_t));
		if (src_addr)
			src_addr[i] = addr;
	}
	return count;
}

ssize_t sock_cq_sreadfrom(struct fid_cq *cq, void *buf, size_t count,
			fi_addr_t *src_addr, const void *cond, int timeout)
{
	int ret = 0;
	int64_t threshold;
	struct sock_cq *sock_cq;
	uint64_t start_ms = 0, end_ms = 0;
	ssize_t cq_entry_len, avail;
	
	sock_cq = container_of(cq, struct sock_cq, cq_fid);
	cq_entry_len = sock_cq->cq_entry_size;

	if (sock_cq->attr.wait_cond == FI_CQ_COND_THRESHOLD) {
		threshold = MIN((int64_t)cond, count);
	}else{
		threshold = count;
	}

	if (sock_cq->domain->progress_mode == FI_PROGRESS_MANUAL) {
		if (timeout >= 0) {
			start_ms = fi_gettime_ms();
			end_ms = start_ms + timeout;
		}

		do {
			sock_cq_progress(sock_cq);
			fastlock_acquire(&sock_cq->lock);
			if ((avail = rbfdused(&sock_cq->cq_rbfd)))
				ret = sock_cq_rbuf_read(sock_cq, buf, 
							MIN(threshold, avail / cq_entry_len),
							src_addr, cq_entry_len);
			fastlock_release(&sock_cq->lock);
			if (ret == 0 && timeout >= 0) {
				if (fi_gettime_ms() >= end_ms)
					return -FI_ETIMEDOUT;
			}
		}while (ret == 0);
	} else {
		ret = rbfdwait(&sock_cq->cq_rbfd, timeout);
		fastlock_acquire(&sock_cq->lock);
		if (ret != -FI_ETIMEDOUT && (avail = rbfdused(&sock_cq->cq_rbfd)))
			ret = sock_cq_rbuf_read(sock_cq, buf, 
						MIN(threshold, avail / cq_entry_len),
						src_addr, cq_entry_len);
		fastlock_release(&sock_cq->lock);
	}
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
			uint64_t flags)
{
	ssize_t num_read;
	struct sock_cq *sock_cq;
	
	sock_cq = container_of(cq, struct sock_cq, cq_fid);
	num_read = 0;

	if (sock_cq->domain->progress_mode == FI_PROGRESS_MANUAL)
		sock_cq_progress(sock_cq);

	fastlock_acquire(&sock_cq->lock);
	while (rbused(&sock_cq->cqerr_rb) >= sizeof(struct fi_cq_err_entry)) {
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
	if (!(sock_cq->attr.flags & FI_WRITE))
		return -FI_EINVAL;

	return _sock_cq_write(sock_cq, FI_ADDR_NOTAVAIL, buf, len);
}

ssize_t sock_cq_writeerr(struct fid_cq *cq, struct fi_cq_err_entry *buf,
			size_t len, uint64_t flags)
{
	struct sock_cq *sock_cq;
	
	sock_cq = container_of(cq, struct sock_cq, cq_fid);
	if (!(sock_cq->attr.flags & FI_WRITE))
		return -FI_EINVAL;

	return _sock_cq_writeerr(sock_cq, buf, len);
}

const char * sock_cq_strerror(struct fid_cq *cq, int prov_errno,
			      const void *err_data, char *buf, size_t len)
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

	if (cq->signal && cq->attr.wait_obj == FI_WAIT_MUTEX_COND)
		sock_wait_close(&cq->waitset->fid);

	rbfree(&cq->addr_rb);
	rbfree(&cq->cqerr_rb);
	rbfdfree(&cq->cq_rbfd);

	fastlock_destroy(&cq->lock);
	fastlock_destroy(&cq->list_lock);
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

static int sock_cq_control(struct fid *fid, int command, void *arg)
{
	struct sock_cq *cq;
	int ret = 0;
	
	cq = container_of(fid, struct sock_cq, cq_fid);
	switch (command) {
	case FI_GETWAIT:
		switch (cq->attr.wait_obj) {
		case FI_WAIT_NONE:
		case FI_WAIT_FD:
		case FI_WAIT_UNSPEC:
			memcpy(arg, &cq->cq_rbfd.fd[RB_READ_FD], sizeof(int));
			break;

		case FI_WAIT_SET:
		case FI_WAIT_MUTEX_COND:
			sock_wait_get_obj(cq->waitset, arg);
			break;

		default:
			ret = -FI_EINVAL;
			break;
		}
		break;

	default:
		ret =  -FI_EINVAL;
		break;
	}
	
	return ret;
}

struct fi_ops sock_cq_fi_ops = {
	.size = sizeof(struct fi_ops),
	.control = sock_cq_control,
	.close = sock_cq_close,
};

static int sock_cq_verify_attr(struct fi_cq_attr *attr)
{
	if (!attr)
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
	case FI_WAIT_SET:
	case FI_WAIT_MUTEX_COND:
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
	.size = SOCK_CQ_DEF_SZ,
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
	struct fi_wait_attr wait_attr;
	struct sock_fid_list *list_entry;
	struct sock_wait *wait;
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

	if (attr == NULL) 
		sock_cq->attr = _sock_cq_def_attr;
	else {
		sock_cq->attr = *attr;
		if (attr->size == 0) 
			sock_cq->attr.size = _sock_cq_def_attr.size;
	}
	
	sock_cq->domain = sock_dom;
	sock_cq->cq_entry_size = sock_cq_entry_size(sock_cq);
	sock_cq_set_report_fn(sock_cq);

	dlist_init(&sock_cq->tx_list);
	dlist_init(&sock_cq->rx_list);
	dlist_init(&sock_cq->ep_list);

	if ((ret = rbfdinit(&sock_cq->cq_rbfd, sock_cq->attr.size *
		    sock_cq->cq_entry_size)))
		goto err1;

	if ((ret = rbinit(&sock_cq->addr_rb, 
			 sock_cq->attr.size * sizeof(fi_addr_t))))
		goto err2;
	
	if ((ret = rbinit(&sock_cq->cqerr_rb, sock_cq->attr.size * 
			 sizeof(struct fi_cq_err_entry))))
		goto err3;

	fastlock_init(&sock_cq->lock);

	switch (sock_cq->attr.wait_obj) {
	case FI_WAIT_NONE:
	case FI_WAIT_UNSPEC:
	case FI_WAIT_FD:
		break;

	case FI_WAIT_MUTEX_COND:
		wait_attr.flags = 0;
		wait_attr.wait_obj = FI_WAIT_MUTEX_COND;
		ret = sock_wait_open(&sock_dom->fab->fab_fid, &wait_attr,
				     &sock_cq->waitset);
		if (ret) {
			ret = -FI_EINVAL;
			goto err4;
		}
		sock_cq->signal = 1;
		break;

	case FI_WAIT_SET:
		if (!attr) {
			ret = -FI_EINVAL;
			goto err4;
		}

		sock_cq->waitset = attr->wait_set;
		sock_cq->signal = 1;
		wait = container_of(attr->wait_set, struct sock_wait, wait_fid);
		list_entry = calloc(1, sizeof(*list_entry));
		dlist_init(&list_entry->entry);
		list_entry->fid = &sock_cq->cq_fid.fid;
		dlist_insert_after(&list_entry->entry, &wait->fid_list);
		break;

	default:
		break;
	}
	
	*cq = &sock_cq->cq_fid;
	atomic_inc(&sock_dom->ref);
	fastlock_init(&sock_cq->list_lock);

	return 0;

err4:
	rbfree(&sock_cq->cqerr_rb);
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
	if (rbavail(&cq->cqerr_rb) < sizeof(struct fi_cq_err_entry)) {
		ret = -FI_ENOSPC;
		goto out;
	}

	err_entry.err = err;
	err_entry.olen = olen;
	err_entry.err_data = err_data;
	err_entry.len = entry->data_len;
	err_entry.prov_errno = prov_errno;
	err_entry.flags = entry->flags;
	err_entry.data = entry->data;
	err_entry.tag = entry->tag;
	err_entry.op_context = (void*)entry->context;
	
	if (entry->type == SOCK_PE_RX) {
		err_entry.buf = (void*)entry->pe.rx.rx_iov[0].iov.addr;
	}else {
		err_entry.buf = (void*)entry->pe.tx.data.tx_iov[0].src.iov.addr;
	}

	rbwrite(&cq->cqerr_rb, &err_entry, sizeof(struct fi_cq_err_entry));
	rbcommit(&cq->cqerr_rb);
	ret = 0;

out:
	fastlock_release(&cq->lock);
	return ret;
}

