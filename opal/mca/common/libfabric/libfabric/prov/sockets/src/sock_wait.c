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
#include <sys/time.h>

#include "sock.h"
#include "sock_util.h"

#define SOCK_LOG_INFO(...) _SOCK_LOG_INFO(FI_LOG_CORE, __VA_ARGS__)
#define SOCK_LOG_ERROR(...) _SOCK_LOG_ERROR(FI_LOG_CORE, __VA_ARGS__)

enum {
	WAIT_READ_FD = 0,
	WAIT_WRITE_FD,
};

int sock_wait_get_obj(struct fid_wait *fid, void *arg)
{
	struct fi_mutex_cond mut_cond;
	struct sock_wait *wait;

	wait = container_of(fid, struct sock_wait, wait_fid.fid);
	
	switch (wait->type) {
	case FI_WAIT_FD:
		memcpy(arg,&wait->wobj.fd[WAIT_READ_FD], sizeof(int));
		break;
		
	case FI_WAIT_MUTEX_COND:
		mut_cond.mutex = &wait->wobj.mutex_cond.mutex;
		mut_cond.cond = &wait->wobj.mutex_cond.cond;
		memcpy(arg, &mut_cond, sizeof(mut_cond));
		break;
		
	default:
		SOCK_LOG_ERROR("Invalid wait obj type\n");
		return -FI_EINVAL;
	}
	
	return 0;
}

static int sock_wait_init(struct sock_wait *wait, enum fi_wait_obj type)
{
	int ret;

	wait->type = type;
	
	switch (type) {
	case FI_WAIT_FD:
		if (socketpair(AF_UNIX, SOCK_STREAM, 0, wait->wobj.fd))
			return -errno;
		
		ret = fd_set_nonblock(wait->wobj.fd[WAIT_READ_FD]);
		if (ret) {
			close(wait->wobj.fd[WAIT_READ_FD]);
			close(wait->wobj.fd[WAIT_WRITE_FD]);
			return ret;
		}
		break;
		
	case FI_WAIT_MUTEX_COND:
		pthread_mutex_init(&wait->wobj.mutex_cond.mutex, NULL);
		pthread_cond_init(&wait->wobj.mutex_cond.cond, NULL);
		break;
		
	default:
		SOCK_LOG_ERROR("Invalid wait object type\n");
		return -FI_EINVAL;
	}	
	return 0;
}

static int sock_wait_wait(struct fid_wait *wait_fid, int timeout)
{
	int err = 0;
	struct sock_cq *cq;
	struct sock_cntr *cntr;
	struct timeval now;
	struct sock_wait *wait;
	double start_ms = 0.0, end_ms = 0.0;
	struct dlist_entry *p, *head;
	struct sock_fid_list *list_item;
	
	wait = container_of(wait_fid, struct sock_wait, wait_fid);
	if (timeout > 0) {
		gettimeofday(&now, NULL);
		start_ms = (double)now.tv_sec * 1000.0 +
			(double)now.tv_usec / 1000.0;
	}

	head = &wait->fid_list;
	for (p = head->next; p != head; p = p->next) {
		list_item = container_of(p, struct sock_fid_list, entry);
		switch (list_item->fid->fclass) {
		case FI_CLASS_CQ:
			cq = container_of(list_item->fid,
					  struct sock_cq, cq_fid);
			sock_cq_progress(cq);
			break;

		case FI_CLASS_CNTR:
			cntr = container_of(list_item->fid,
					    struct sock_cntr, cntr_fid);
			sock_cntr_progress(cntr);
			break;
		}
	}
	if (timeout > 0) {
		gettimeofday(&now, NULL);
		end_ms = (double)now.tv_sec * 1000.0 +
			(double)now.tv_usec / 1000.0;
		timeout -=  (end_ms - start_ms);
		timeout = timeout < 0 ? 0 : timeout;
	}

	switch (wait->type) {
	case FI_WAIT_FD:
		err = fi_poll_fd(wait->wobj.fd[WAIT_READ_FD], timeout);
		if (err > 0)
			err = 0;
		else if (err == 0)
			err = -FI_ETIMEDOUT;
		break;
		
	case FI_WAIT_MUTEX_COND:
		err = fi_wait_cond(&wait->wobj.mutex_cond.cond,
				   &wait->wobj.mutex_cond.mutex, timeout);
		break;
		
	default:
		SOCK_LOG_ERROR("Invalid wait object type\n");
		return -FI_EINVAL;
	}
	return err;
}

void sock_wait_signal(struct fid_wait *wait_fid)
{
	struct sock_wait *wait;
	static char c = 'a';
	int ret;

	wait = container_of(wait_fid, struct sock_wait, wait_fid);

	switch (wait->type) {
	case FI_WAIT_FD:
		ret = write(wait->wobj.fd[WAIT_WRITE_FD], &c, 1);
		if (ret != 1)
			SOCK_LOG_ERROR("failed to signal\n");
		break;
		
	case FI_WAIT_MUTEX_COND:
		pthread_cond_signal(&wait->wobj.mutex_cond.cond);
		break;
	default:
		SOCK_LOG_ERROR("Invalid wait object type\n");
		return;
	}
}

static struct fi_ops_wait sock_wait_ops = {
	.size = sizeof(struct fi_ops_wait),
	.wait = sock_wait_wait,
};

static int sock_wait_control(struct fid *fid, int command, void *arg)
{
	struct sock_wait *wait;
	int ret = 0;

	wait = container_of(fid, struct sock_wait, wait_fid.fid);
	switch (command) {
	case FI_GETWAIT:
		ret = sock_wait_get_obj(&wait->wait_fid, arg);
		break;
	default:
		ret = -FI_EINVAL;
		break;
	}
	return ret;
}

int sock_wait_close(fid_t fid)
{
	struct sock_fid_list *list_item;
	struct dlist_entry *p, *head;
	struct sock_wait *wait;

	wait = container_of(fid, struct sock_wait, wait_fid.fid);
	head = &wait->fid_list;

	for (p = head->next; p != head;) {
		list_item = container_of(p, struct sock_fid_list, entry);
		p = p->next;
		free(list_item);
	}

	if (wait->type == FI_WAIT_FD) {
		close(wait->wobj.fd[WAIT_READ_FD]);
		close(wait->wobj.fd[WAIT_WRITE_FD]);
	}

	atomic_dec(&wait->fab->ref);
	free(wait);
	return 0;
}

static struct fi_ops sock_wait_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = sock_wait_close,
	.bind = fi_no_bind,
	.control = sock_wait_control,
	.ops_open = fi_no_ops_open,
};

static int sock_verify_wait_attr(struct fi_wait_attr *attr)
{
	switch (attr->wait_obj) {
	case FI_WAIT_UNSPEC:
	case FI_WAIT_FD:
	case FI_WAIT_MUTEX_COND:
		break;
		
	default:
		SOCK_LOG_ERROR("Invalid wait object type\n");
		return -FI_EINVAL;
	}
	if (attr->flags)
		return -FI_EINVAL;
	return 0;
}

int sock_wait_open(struct fid_fabric *fabric, struct fi_wait_attr *attr,
		   struct fid_wait **waitset)
{
	int err;
	struct sock_wait *wait;
	struct sock_fabric *fab;
	enum fi_wait_obj wait_obj_type;

	if (attr && sock_verify_wait_attr(attr))
		return -FI_EINVAL;
	
	fab = container_of(fabric, struct sock_fabric, fab_fid);
	if (!attr || attr->wait_obj == FI_WAIT_UNSPEC)
		wait_obj_type = FI_WAIT_FD;
	else 
		wait_obj_type = attr->wait_obj;
	
	wait = calloc(1, sizeof(*wait));
	if (!wait)
		return -FI_ENOMEM;
	
	err = sock_wait_init(wait, wait_obj_type);
	if (err) {
		free(wait);
		return err;
	}

	wait->wait_fid.fid.fclass = FI_CLASS_WAIT;
	wait->wait_fid.fid.context = 0;
	wait->wait_fid.fid.ops = &sock_wait_fi_ops;
	wait->wait_fid.ops = &sock_wait_ops;
	wait->fab = fab;
	wait->type = wait_obj_type;
	atomic_inc(&fab->ref);
	dlist_init(&wait->fid_list);

	*waitset = &wait->wait_fid;
	return 0;
}
