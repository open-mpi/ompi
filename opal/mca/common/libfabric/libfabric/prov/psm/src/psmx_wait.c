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

/* It is necessary to have a separate thread making progress in order
 * for the wait functions to succeed. This thread is only created when
 * wait functions are called and. In order to minimize performance
 * impact, it only goes active during te time when wait calls are
 * blocked.
 */
static pthread_t	psmx_wait_thread;
static pthread_mutex_t	psmx_wait_mutex;
static pthread_cond_t	psmx_wait_cond;
static volatile int	psmx_wait_thread_ready = 0;
static volatile int	psmx_wait_thread_enabled = 0;
static volatile int	psmx_wait_thread_busy = 0;

static void *psmx_wait_progress(void *args)
{
	struct psmx_fid_domain *domain = args;

	psmx_wait_thread_ready = 1;
	pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

	while (1) {
		pthread_mutex_lock(&psmx_wait_mutex);
		pthread_cond_wait(&psmx_wait_cond, &psmx_wait_mutex);
		pthread_mutex_unlock(&psmx_wait_mutex);
		pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);

		psmx_wait_thread_busy = 1;
		while (psmx_wait_thread_enabled)
			psmx_progress(domain);

		psmx_wait_thread_busy = 0;

		pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
	}

	return NULL;
}

static void psmx_wait_start_progress(struct psmx_fid_domain *domain)
{
	pthread_attr_t attr;
	int err;

	if (!domain)
		return;

	if (!psmx_wait_thread) {
		pthread_mutex_init(&psmx_wait_mutex, NULL);
		pthread_cond_init(&psmx_wait_cond, NULL);
		pthread_attr_init(&attr);
		pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
		err = pthread_create(&psmx_wait_thread, &attr, psmx_wait_progress, (void *)domain);
		if (err)
			FI_WARN(&psmx_prov, FI_LOG_EQ,
				"cannot create wait progress thread\n");
		pthread_attr_destroy(&attr);
		while (!psmx_wait_thread_ready)
			;
	}

	psmx_wait_thread_enabled = 1;
	pthread_cond_signal(&psmx_wait_cond);
}

static void psmx_wait_stop_progress(void)
{
	psmx_wait_thread_enabled = 0;

	while (psmx_wait_thread_busy)
		;
}

int psmx_wait_get_obj(struct psmx_fid_wait *wait, void *arg)
{
	void *obj_ptr;
	int obj_size = 0;
	struct fi_mutex_cond mutex_cond;

	if (!arg)
		return -FI_EINVAL;

	if (wait) {
		switch (wait->type) {
			case FI_WAIT_FD:
				obj_size = sizeof(wait->fd[0]);
				obj_ptr = &wait->fd[0];
				break;

			case FI_WAIT_MUTEX_COND:
				mutex_cond.mutex = &wait->mutex;
				mutex_cond.cond = &wait->cond;
				obj_size = sizeof(mutex_cond);
				obj_ptr = &mutex_cond;
				break;

			default:
				break;
		}
	}

	if (obj_size) {
		memcpy(arg, obj_ptr, obj_size);
	}

	return 0;
}

int psmx_wait_wait(struct fid_wait *wait, int timeout)
{
	struct psmx_fid_wait *wait_priv;
	int err = 0;
	
	wait_priv = container_of(wait, struct psmx_fid_wait, wait.fid);

	psmx_wait_start_progress(wait_priv->fabric->active_domain);

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

	case FI_WAIT_MUTEX_COND:
		err = fi_wait_cond(&wait_priv->cond,
				   &wait_priv->mutex, timeout);
		break;

	default:
		break;
	}

	psmx_wait_stop_progress();

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
		if (write(wait_priv->fd[1], &c, 1) != 1)
			FI_WARN(&psmx_prov, FI_LOG_EQ,
				"error signaling wait object\n");
		break;

	case FI_WAIT_MUTEX_COND:
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

	case FI_WAIT_MUTEX_COND:
		pthread_mutex_init(&wait->mutex, NULL);
		pthread_cond_init(&wait->cond, NULL);
		break;
 
	default:
		break;
	}

	return 0;
}

int psmx_wait_open(struct fid_fabric *fabric, struct fi_wait_attr *attr,
		   struct fid_wait **waitset)
{
	struct psmx_fid_wait *wait_priv;
	int type = FI_WAIT_FD;
	int err;

	if (attr) {
		switch (attr->wait_obj) {
		case FI_WAIT_UNSPEC:
			break;

		case FI_WAIT_FD:
		case FI_WAIT_MUTEX_COND:
			type = attr->wait_obj;
			break;
	 
		default:
			FI_INFO(&psmx_prov, FI_LOG_EQ,
				"attr->wait_obj=%d, supported=%d,%d,%d\n",
				attr->wait_obj, FI_WAIT_UNSPEC,
				FI_WAIT_FD, FI_WAIT_MUTEX_COND);
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

	wait_priv->fabric = container_of(fabric, struct psmx_fid_fabric, fabric);
	wait_priv->wait.fid.fclass = FI_CLASS_WAIT;
	wait_priv->wait.fid.context = 0;
	wait_priv->wait.fid.ops = &psmx_fi_ops;
	wait_priv->wait.ops = &psmx_wait_ops;

	*waitset = &wait_priv->wait;
	return 0;
}

