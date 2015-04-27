/*
 * Copyright (c) 2013-2014 Intel Corporation. All rights reserved.
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

#include "psmx.h"

void psmx_eq_enqueue_event(struct psmx_fid_eq *eq, struct psmx_eq_event *event)
{
	pthread_mutex_lock(&eq->mutex);
	if (event->error)
		slist_insert_tail(&event->list_entry, &eq->error_queue);
	else
		slist_insert_tail(&event->list_entry, &eq->event_queue);
	pthread_mutex_unlock(&eq->mutex);

	if (eq->wait)
		psmx_wait_signal((struct fid_wait *)eq->wait);
}

static struct psmx_eq_event *psmx_eq_dequeue_event(struct psmx_fid_eq *eq)
{
	struct slist_entry *entry;

	if (slist_empty(&eq->event_queue))
		return NULL;

	pthread_mutex_lock(&eq->mutex);
	entry = slist_remove_head(&eq->event_queue);
	pthread_mutex_unlock(&eq->mutex);

	return container_of(entry, struct psmx_eq_event, list_entry);
}

static struct psmx_eq_event *psmx_eq_dequeue_error(struct psmx_fid_eq *eq)
{
	struct slist_entry *entry;

	if (slist_empty(&eq->error_queue))
		return NULL;

	pthread_mutex_lock(&eq->mutex);
	entry = slist_remove_head(&eq->error_queue);
	pthread_mutex_unlock(&eq->mutex);

	return container_of(entry, struct psmx_eq_event, list_entry);
}

static struct psmx_eq_event *psmx_eq_peek_event(struct psmx_fid_eq *eq)
{
	if (slist_empty(&eq->event_queue))
		return NULL;

	return container_of(eq->event_queue.head, struct psmx_eq_event, list_entry);
}

static struct psmx_eq_event *psmx_eq_alloc_event(struct psmx_fid_eq *eq)
{
	struct psmx_eq_event *event;

	if (!slist_empty(&eq->free_list)) {
		event = container_of(slist_remove_head(&eq->free_list),
				     struct psmx_eq_event, list_entry);
	}
	else {
		event = calloc(1, sizeof(*event));
		if (!event) {
			FI_WARN(&psmx_prov, FI_LOG_EQ, "out of memory.\n");
			return NULL;
		}
	}

	return event;
}

static void psmx_eq_free_event(struct psmx_fid_eq *eq, struct psmx_eq_event *event)
{
	memset(event, 0, sizeof(*event));
	slist_insert_tail(&event->list_entry, &eq->free_list);
}

struct psmx_eq_event *psmx_eq_create_event(struct psmx_fid_eq *eq,
					   uint32_t event_num,
					   void *context,
					   uint64_t data,
					   int err,
					   int prov_errno,
					   void *err_data,
					   size_t err_data_size)
{
	struct psmx_eq_event *event;

	event = psmx_eq_alloc_event(eq);
	if (!event)
		return NULL;

	event->event = event_num;
	event->eqe.data.fid = &eq->fabric->fabric.fid;

	if ((event->error = !!err)) {
		event->eqe.err.context = context;
		event->eqe.err.data = data;
		event->eqe.err.err = -err;
		event->eqe.err.prov_errno = prov_errno;
		event->eqe.err.err_data = err_data;
		event->eqe.err.err_data_size = err_data_size;
		event->entry_size = sizeof(event->eqe.err);
		goto out;
	}

	switch (event_num) {
	case FI_NOTIFY:
		event->eqe.data.context = context;
		event->eqe.data.data = data;
		event->entry_size = sizeof(event->eqe.data);
		break;

	case FI_CONNREQ:
	case FI_CONNECTED:
	case FI_SHUTDOWN:
		event->eqe.cm.info = context;
		/* TODO: store event->eqe.cm.data (upto sizeof(eqe)-sizeof(eqe.cm) bytes) */
		event->entry_size = sizeof(event->eqe.cm) + 0; /* TODO: extra data size */
		break;

	case FI_MR_COMPLETE:
	case FI_AV_COMPLETE:
		event->eqe.data.context = context;
		event->eqe.data.data = data;
		event->entry_size = sizeof(event->eqe.data);
		break;

	default:
		psmx_eq_free_event(eq, event);
		FI_WARN(&psmx_prov, FI_LOG_EQ, "invalid event %d.\n", event_num);
		event = NULL;
		break;
	}

out:
	return event;
}

static ssize_t psmx_eq_read(struct fid_eq *eq, uint32_t *event_num, void *buf,
			    size_t len, uint64_t flags)
{
	struct psmx_fid_eq *eq_priv;
	struct psmx_eq_event *event;
	ssize_t bytes_read = 0;

	eq_priv = container_of(eq, struct psmx_fid_eq, eq);

	if (slist_empty(&eq_priv->event_queue))
		psmx_progress(eq_priv->fabric->active_domain);

	if (!slist_empty(&eq_priv->error_queue))
		return -FI_EAVAIL;

	if (flags & FI_PEEK)
		event = psmx_eq_peek_event(eq_priv);
	else
		event = psmx_eq_dequeue_event(eq_priv);

	if (!event)
		return -FI_EAGAIN;

	*event_num = event->event;
	if (buf) {
		bytes_read = MIN(len, event->entry_size);
		memcpy(buf, (void *)&event->eqe, bytes_read);
	}

	if (!(flags & FI_PEEK))
		psmx_eq_free_event(eq_priv, event);

	return bytes_read;
}

static ssize_t psmx_eq_readerr(struct fid_eq *eq, struct fi_eq_err_entry *buf,
			       uint64_t flags)
{
	struct psmx_fid_eq *eq_priv;
	struct psmx_eq_event *event;
	ssize_t bytes_read = 0;

	eq_priv = container_of(eq, struct psmx_fid_eq, eq);

	event = psmx_eq_dequeue_error(eq_priv);
	if (!event)
		return -FI_EAGAIN;

	if (buf) {
		bytes_read = sizeof(*buf);
		memcpy(buf, &event->eqe.err, bytes_read);
		psmx_eq_free_event(eq_priv, event);
	}

	return bytes_read;
}

static ssize_t psmx_eq_sread(struct fid_eq *eq, uint32_t *event, void *buf,
			     size_t len, int timeout, uint64_t flags)
{
	struct psmx_fid_eq *eq_priv;
	struct timespec ts0, ts;
	int msec_passed = 0;
	ssize_t ret = -FI_EAGAIN;

	eq_priv = container_of(eq, struct psmx_fid_eq, eq);

	if (eq_priv->wait) {
		psmx_wait_wait((struct fid_wait *)eq_priv->wait, timeout);
		ret = psmx_eq_read(eq, event, buf, len, flags);
	}
	else {
		clock_gettime(CLOCK_REALTIME, &ts0);
		while (1) {
			ret = psmx_eq_read(eq, event, buf, len, flags);
			if (ret != -FI_EAGAIN)
				break;
			
			if (timeout < 0)
				continue;

			clock_gettime(CLOCK_REALTIME, &ts);
			msec_passed = (ts.tv_sec - ts0.tv_sec) * 1000 +
				       (ts.tv_nsec - ts0.tv_nsec) / 1000000;

			if (msec_passed >= timeout)
				break;
		}
	}

	return ret;
}

static ssize_t psmx_eq_write(struct fid_eq *eq, uint32_t event_num, const void *buf,
			 size_t len, uint64_t flags)
{
	struct psmx_fid_eq *eq_priv;
	struct psmx_eq_event *event;
	ssize_t bytes_written;

	eq_priv = container_of(eq, struct psmx_fid_eq, eq);

	event = psmx_eq_alloc_event(eq_priv);
	if (!event)
		return -FI_ENOMEM;

	bytes_written = MIN(len, sizeof(event->eqe));

	event->event = event_num;
	event->entry_size = bytes_written;
	memcpy(&event->eqe, buf, bytes_written);
	
	psmx_eq_enqueue_event(eq_priv, event);

	if (eq_priv->wait)
		psmx_wait_signal((struct fid_wait *)eq_priv->wait);

	return bytes_written;
}

static const char *psmx_eq_strerror(struct fid_eq *eq, int prov_errno,
				    const void *err_data, char *buf, size_t len)
{
	return psm_error_get_string(prov_errno);
}

static int psmx_eq_close(fid_t fid)
{
	struct psmx_fid_eq *eq;
	struct slist_entry *entry;
	struct psmx_eq_event *item;

	eq = container_of(fid, struct psmx_fid_eq, eq.fid);

	while (!slist_empty(&eq->free_list)) {
		entry = slist_remove_head(&eq->free_list);
		item = container_of(entry, struct psmx_eq_event, list_entry);
		free(item);
	}

	if (eq->wait && eq->wait_is_local)
		fi_close((fid_t)eq->wait);

	free(eq);

	return 0;
}

static int psmx_eq_control(struct fid *fid, int command, void *arg)
{
	struct psmx_fid_eq *eq;
	int ret = 0;

	eq = container_of(fid, struct psmx_fid_eq, eq.fid);

	switch (command) {
	case FI_GETWAIT:
		ret = psmx_wait_get_obj(eq->wait, arg);
		break;

	default:
		return -FI_ENOSYS;
	}

	return ret;
}

static struct fi_ops psmx_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = psmx_eq_close,
	.bind = fi_no_bind,
	.control = psmx_eq_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_eq psmx_eq_ops = {
	.size = sizeof(struct fi_ops_eq),
	.read = psmx_eq_read,
	.readerr = psmx_eq_readerr,
	.sread = psmx_eq_sread,
	.write = psmx_eq_write,
	.strerror = psmx_eq_strerror,
};

int psmx_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
		 struct fid_eq **eq, void *context)
{
	struct psmx_fid_fabric *fabric_priv;
	struct psmx_fid_eq *eq_priv;
	struct psmx_fid_wait *wait = NULL;
	struct psmx_eq_event *event;
	struct fi_wait_attr wait_attr;
	int wait_is_local = 0;
	int err;
	int i;

	fabric_priv = container_of(fabric, struct psmx_fid_fabric, fabric);

	switch (attr->wait_obj) {
	case FI_WAIT_NONE:
	case FI_WAIT_UNSPEC:
		break;

	case FI_WAIT_SET:
		if (!attr->wait_set) {
			FI_INFO(&psmx_prov, FI_LOG_EQ,
				"FI_WAIT_SET is specified but attr->wait_set is NULL\n");
			return -FI_EINVAL;
		}
		wait = (struct psmx_fid_wait *)attr->wait_set;
		break;

	case FI_WAIT_FD:
	case FI_WAIT_MUTEX_COND:
		wait_attr.wait_obj = attr->wait_obj;
		wait_attr.flags = 0;
		err = psmx_wait_open(fabric, &wait_attr, (struct fid_wait **)&wait);
		if (err)
			return err;
		wait_is_local = 1;
		break;

	default:
		FI_INFO(&psmx_prov, FI_LOG_EQ,
			"attr->wait_obj=%d, supported=%d...%d\n", attr->wait_obj,
			FI_WAIT_NONE, FI_WAIT_MUTEX_COND);
		return -FI_EINVAL;
	}

	eq_priv = (struct psmx_fid_eq *) calloc(1, sizeof *eq_priv);
	if (!eq_priv) {
		if (wait)
			free(wait);
		return -FI_ENOMEM;
	}

	eq_priv->fabric = fabric_priv;
	eq_priv->wait = wait;
	eq_priv->wait_is_local = wait_is_local;

	eq_priv->eq.fid.fclass = FI_CLASS_EQ;
	eq_priv->eq.fid.context = context;
	eq_priv->eq.fid.ops = &psmx_fi_ops;
	eq_priv->eq.ops = &psmx_eq_ops;

	slist_init(&eq_priv->event_queue);
	slist_init(&eq_priv->error_queue);
	slist_init(&eq_priv->free_list);
	pthread_mutex_init(&eq_priv->mutex, NULL);

#define PSMX_FREE_LIST_SIZE	64
	for (i=0; i<PSMX_FREE_LIST_SIZE; i++) {
		event = calloc(1, sizeof(*event));
		if (!event) {
			FI_WARN(&psmx_prov, FI_LOG_EQ, "out of memory.\n");
			exit(-1);
		}
		slist_insert_tail(&event->list_entry, &eq_priv->free_list);
	}

	*eq = &eq_priv->eq;
	return 0;
}

