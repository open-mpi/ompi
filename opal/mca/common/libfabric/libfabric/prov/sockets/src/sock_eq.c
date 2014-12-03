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
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <fi_list.h>

#include "sock.h"
#include "sock_util.h"

ssize_t sock_eq_sread(struct fid_eq *eq, uint32_t *event, void *buf, size_t len,
		      int timeout, uint64_t flags)
{
	int ret;
	struct sock_eq *sock_eq;
	struct dlist_entry *list;
	struct sock_eq_entry *entry;

	sock_eq = container_of(eq, struct sock_eq, eq);

	fastlock_acquire(&sock_eq->lock);
	if(!dlistfd_empty(&sock_eq->err_list)) {
		ret = -FI_EAVAIL;
		goto out;
	}
		
	if(dlistfd_empty(&sock_eq->list)) {
		ret = dlistfd_wait_avail(&sock_eq->list, timeout);
		if(ret <= 0)
			goto out;
	}

	list = sock_eq->list.list.next;
	entry = container_of(list, struct sock_eq_entry, entry);

	if(entry->len > len) {
		ret = -FI_ETOOSMALL;
		goto out;
	}

	ret = entry->len;
	*event = entry->type;
	memcpy(buf, entry->event, entry->len);

	if(!(flags & FI_PEEK)) {
		dlistfd_remove(list, &sock_eq->list);
		free(entry);
	}

out:
	fastlock_release(&sock_eq->lock);
	return ret;
}


ssize_t sock_eq_read(struct fid_eq *eq, uint32_t *event, void *buf, size_t len,
		     uint64_t flags)
{
	return sock_eq_sread(eq, event, buf, len, 0, flags);
}

ssize_t sock_eq_readerr(struct fid_eq *eq, struct fi_eq_err_entry *buf,
			uint64_t flags)
{
	int ret;
	struct sock_eq *sock_eq;
	struct dlist_entry *list;
	struct sock_eq_entry *entry;

	sock_eq = container_of(eq, struct sock_eq, eq);

	fastlock_acquire(&sock_eq->lock);
	if(dlistfd_empty(&sock_eq->err_list)) {
		ret = 0;
		goto out;
	}

	list = sock_eq->err_list.list.next;
	entry = container_of(list, struct sock_eq_entry, entry);

	ret = entry->len;
	memcpy(buf, entry->event, entry->len);

	if(!(flags & FI_PEEK)) {
		dlistfd_remove(list, &sock_eq->err_list);
		free(entry);
	}

out:
	fastlock_release(&sock_eq->lock);
	return ret;
}

ssize_t sock_eq_report_event(struct sock_eq *sock_eq, uint32_t event, 
			     const void *buf, size_t len, uint64_t flags)
{
	struct sock_eq_entry *entry = calloc(1, len + 
					     sizeof(struct sock_eq_entry));
	if(!entry)
		return -FI_ENOMEM;

	fastlock_acquire(&sock_eq->lock);
	
	entry->type = event;
	entry->len = len;
	entry->flags = flags;
	memcpy(entry->event, buf, len);

	dlistfd_insert_tail(&entry->entry, &sock_eq->list);
	fastlock_release(&sock_eq->lock);
	return 0;
}

ssize_t sock_eq_report_error(struct sock_eq *sock_eq, fid_t fid, void *context,
			     int err, int prov_errno, void *err_data)
{
	struct fi_eq_err_entry *err_entry;
	struct sock_eq_entry *entry = calloc(1, sizeof(struct fi_eq_err_entry) +
					     sizeof(struct sock_eq_entry));
	if(!entry)
		return -FI_ENOMEM;

	fastlock_acquire(&sock_eq->lock);

	err_entry = (struct fi_eq_err_entry*)entry->event;
	err_entry->fid = fid;
	err_entry->context = context;
	err_entry->err = err;
	err_entry->prov_errno = prov_errno;
	err_entry->err_data = err_data;	
	entry->len = sizeof(struct fi_eq_err_entry);

	dlistfd_insert_tail(&entry->entry, &sock_eq->err_list);
	fastlock_release(&sock_eq->lock);
	return 0;
}

static ssize_t sock_eq_write(struct fid_eq *eq, uint32_t event, 
		      const void *buf, size_t len, uint64_t flags)
{
	struct sock_eq *sock_eq;
	sock_eq = container_of(eq, struct sock_eq, eq);
	
	if(!(sock_eq->attr.flags & FI_WRITE))
		return -FI_EINVAL;

	return sock_eq_report_event(sock_eq, event, buf, len, flags);
}

const char * sock_eq_strerror(struct fid_eq *eq, int prov_errno,
			      const void *err_data, char *buf, size_t len)
{
	if (buf && len)
		return strncpy(buf, strerror(prov_errno), len);
	return strerror(prov_errno);
}

static struct fi_ops_eq sock_eq_ops = {
	.size = sizeof(struct fi_ops_eq),
	.read = sock_eq_read,
	.readerr = sock_eq_readerr,
	.write = sock_eq_write,
	.sread = sock_eq_sread,
	.strerror = sock_eq_strerror,
};

int sock_eq_fi_close(struct fid *fid)
{
	struct sock_eq *sock_eq;
	sock_eq = container_of(fid, struct sock_eq, eq);

	dlistfd_head_free(&sock_eq->list);
	dlistfd_head_free(&sock_eq->err_list);
	fastlock_destroy(&sock_eq->lock);
	atomic_dec(&sock_eq->sock_fab->ref);

	free(sock_eq);
	return 0;
}

int sock_eq_fi_control(struct fid *fid, int command, void *arg)
{
	struct sock_eq *eq;
	int ret = 0;

	eq = container_of(fid, struct sock_eq, eq.fid);
	
	switch (command) {
	case FI_GETWAIT:
		*(void **) arg = &eq->list.fd[LIST_READ_FD];
		break;
	default:
		ret = -FI_ENOSYS;
		break;
	}

	return ret;
}

static struct fi_ops sock_eq_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_eq_fi_close,
	.bind = fi_no_bind,
	.control = sock_eq_fi_control,
	.ops_open = fi_no_ops_open,
};

static int _sock_eq_verify_attr(struct fi_eq_attr *attr)
{
	if(!attr)
		return 0;

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

static struct fi_eq_attr _sock_eq_def_attr ={
	.size = SOCK_EQ_DEF_SZ,
	.flags = 0,
	.wait_obj = FI_WAIT_FD,
	.signaling_vector = 0,
	.wait_set = NULL,
};

int sock_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
		 struct fid_eq **eq, void *context)
{
	int ret;
	struct sock_eq *sock_eq;

	ret = _sock_eq_verify_attr(attr);
	if (ret)
		return ret;
	
	sock_eq = (struct sock_eq *)calloc(1, sizeof(struct sock_eq));
	if(!sock_eq)
		return -FI_ENOMEM;

	sock_eq->sock_fab = container_of(fabric, struct sock_fabric, fab_fid);
	
	sock_eq->eq.fid.fclass = FI_CLASS_EQ;
	sock_eq->eq.fid.context = context;
	sock_eq->eq.fid.ops = &sock_eq_fi_ops;	
	sock_eq->eq.ops = &sock_eq_ops;	

	if(attr == NULL)
		memcpy(&sock_eq->attr, &_sock_eq_def_attr,
		       sizeof(struct fi_cq_attr));
	else
		memcpy(&sock_eq->attr, attr, sizeof(struct fi_cq_attr));

	ret = dlistfd_head_init(&sock_eq->list);
	if(ret)
		goto err1;

	ret = dlistfd_head_init(&sock_eq->err_list);
	if(ret)
		goto err2;
	
	fastlock_init(&sock_eq->lock);
	atomic_inc(&sock_eq->sock_fab->ref);
	return 0;

err2:
	dlistfd_head_free(&sock_eq->list);
err1:
	free(sock_eq);
	return ret;
}
