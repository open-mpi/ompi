/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
 *
 * This software is waitailable to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, waitailable from the file
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

#include "psmx.h"

int psmx_wait_get_obj(struct psmx_fid_wait *wait, void *arg)
{
	struct fi_wait_obj_set *wait_obj_set = arg;
	void *obj_ptr;
	int obj_size = 0;
	int obj_type = FI_WAIT_NONE;
	int ret_count = 0;
	struct {
		pthread_mutex_t *mutex;
		pthread_cond_t *cond;
	} mutex_cond;

	if (!arg)
		return -EINVAL;

	if (wait) {
		switch (wait->type) {
			case FI_WAIT_FD:
				obj_size = sizeof(wait->fd[0]);
				obj_type = wait->type;
				obj_ptr = &wait->fd[0];
				break;

			case FI_WAIT_MUT_COND:
				mutex_cond.mutex = &wait->mutex;
				mutex_cond.cond = &wait->cond;
				obj_size = sizeof(mutex_cond);
				obj_type = wait->type;
				obj_ptr = &mutex_cond;
				break;

			default:
				break;
		}
	}

	if (obj_size) {
		ret_count = 1;
		if (wait_obj_set->count)
			memcpy(wait_obj_set->obj, obj_ptr, obj_size);
	}

	wait_obj_set->count = ret_count;
	wait_obj_set->wait_obj = obj_type;

	return 0;
}

int psmx_wait_wait(struct fid_wait *wait, int timeout)
{
	struct psmx_fid_wait *wait_priv;
	int err = 0;
	
	wait_priv = container_of(wait, struct psmx_fid_wait, wait.fid);
	switch (wait_priv->type) {
	case FI_WAIT_UNSPEC:
		/* TODO: optimized custom wait */
		break;

	case FI_WAIT_FD:
		err = fi_poll_fd(wait_priv->fd[0], timeout);
		if (err > 0)
			err = 0;
		else if (err == 0)
			err = -FI_ETIMEDOUT;
		break;

	case FI_WAIT_MUT_COND:
		err = fi_wait_cond(&wait_priv->cond,
				   &wait_priv->mutex, timeout);
		break;

	default:
		break;
	}

	return err;
}

void psmx_wait_signal(struct fid_wait *wait)
{
	struct psmx_fid_wait *wait_priv;
	static char c = 'x';

	wait_priv = container_of(wait, struct psmx_fid_wait, wait.fid);

	switch (wait_priv->type) {
	case FI_WAIT_UNSPEC:
		/* TODO: optimized custom wait */
		break;

	case FI_WAIT_FD:
		write(wait_priv->fd[1], &c, 1);
		break;

	case FI_WAIT_MUT_COND:
		pthread_cond_signal(&wait_priv->cond);
		break;
	}
}

static int psmx_wait_close(fid_t fid)
{
	struct psmx_fid_wait *wait;

	wait = container_of(fid, struct psmx_fid_wait, wait.fid);
	if (wait->type == FI_WAIT_FD) {
		close(wait->fd[0]);
		close(wait->fd[1]);
	}
	free(wait);
	return 0;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_wait_close,
	.bind = fi_no_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_wait psmx_wait_ops = {
	.size = sizeof(struct fi_ops_wait),
	.wait = psmx_wait_wait,
};

static int psmx_wait_init(struct psmx_fid_wait *wait, int type)
{
	long flags = 0;

	wait->type = type;
	
	switch (type) {
	case FI_WAIT_UNSPEC:
		/* TODO: optimized custom wait */
		break;

	case FI_WAIT_FD:
		if (socketpair(AF_UNIX, SOCK_STREAM, 0, wait->fd))
			return -errno;

		fcntl(wait->fd[0], F_GETFL, &flags);
		if (fcntl(wait->fd[0], F_SETFL, flags | O_NONBLOCK)) {
			close(wait->fd[0]);
			close(wait->fd[1]);
			return -errno;
		}
		break;

	case FI_WAIT_MUT_COND:
		pthread_mutex_init(&wait->mutex, NULL);
		pthread_cond_init(&wait->cond, NULL);
		break;
 
	default:
		break;
	}

	return 0;
}

int psmx_wait_open(struct fid_domain *domain, struct fi_wait_attr *attr,
		   struct fid_wait **waitset)
{
	struct psmx_fid_domain *domain_priv;
	struct psmx_fid_wait *wait_priv;
	int type = FI_WAIT_FD;
	int err;

	domain_priv = container_of(domain, struct psmx_fid_domain, domain);

	if (attr) {
		switch (attr->wait_obj) {
		case FI_WAIT_UNSPEC:
			break;

		case FI_WAIT_FD:
		case FI_WAIT_MUT_COND:
			type = attr->wait_obj;
			break;
	 
		default:
			psmx_debug("%s: attr->wait_obj=%d, supported=%d,%d,%d\n",
					__func__, attr->wait_obj, FI_WAIT_UNSPEC,
					FI_WAIT_FD, FI_WAIT_MUT_COND);
			return -FI_EINVAL;
		}
	}

	wait_priv = calloc(1, sizeof(*wait_priv));
	if (!wait_priv)
		return -FI_ENOMEM;
	
	err = psmx_wait_init(wait_priv, type);
	if (err) {
		free(wait_priv);
		return err;
	}

	wait_priv->wait.fid.fclass = FI_CLASS_WAIT;
	wait_priv->wait.fid.context = 0;
	wait_priv->wait.fid.ops = &psmx_fi_ops;
	wait_priv->wait.ops = &psmx_wait_ops;
	wait_priv->domain = domain_priv;

	*waitset = &wait_priv->wait;
	return 0;
}

