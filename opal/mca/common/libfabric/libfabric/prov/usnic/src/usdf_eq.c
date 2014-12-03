/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif /* HAVE_CONFIG_H */

#include <asm/types.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/queue.h>
#include <sys/eventfd.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"
#include "fi_enosys.h"

#include "usnic_direct.h"
#include "usd.h"
#include "usdf.h"

static inline int
usdf_eq_empty(struct usdf_eq *eq)
{
	return (atomic_get(&eq->eq_num_events) == 0);
}

static inline int
usdf_eq_error(struct usdf_eq *eq)
{
	return ((eq->eq_ev_tail->ue_flags & USDF_EVENT_FLAG_ERROR) != 0);
}

/*
 * read and event from the ring.  Caller must hold eq lock, and caller 
 * needs to have checked for empty and error
 */
static inline ssize_t
usdf_eq_read_event(struct usdf_eq *eq, uint32_t *event, void *buf, size_t len,
		uint64_t flags)
{
	struct usdf_event *ev;
	size_t copylen;

	ev = eq->eq_ev_tail;

	copylen = MIN(ev->ue_len, len);

	/* copy out the event */
	*event = ev->ue_event;
	memcpy(buf, ev->ue_buf, copylen);

	if ((flags & FI_PEEK) == 0) {

		/* update count */
		atomic_dec(&eq->eq_num_events);

		/* Free the event buf if needed */
		if (ev->ue_flags & USDF_EVENT_FLAG_FREE_BUF) {
			free(ev->ue_buf);
		}

		/* new tail */
		eq->eq_ev_tail++;
		if (eq->eq_ev_tail >= eq->eq_ev_end) {
			eq->eq_ev_tail = eq->eq_ev_ring;
		}
	}
	return copylen;
}

/*
 * unconditionally write an event to the EQ.  Caller is responsible for
 * ensuring there is room.  EQ must be locked.
 */
static inline ssize_t
usdf_eq_write_event(struct usdf_eq *eq, uint32_t event,
		const void *buf, size_t len, uint64_t flags)
{
	struct usdf_event *ev;
	void *ev_buf;

	ev = eq->eq_ev_head;
	ev->ue_event = event;
	ev->ue_len = len;
	ev->ue_flags = flags;

	/* save the event data if we can, else malloc() */
	if (len <= sizeof(struct fi_eq_entry)) {
		ev_buf = eq->eq_ev_buf + (ev - eq->eq_ev_ring);
	} else {
		ev_buf = malloc(len);
		if (ev_buf == NULL) {
			return -errno;
		}
		ev->ue_flags |= USDF_EVENT_FLAG_FREE_BUF;
	}
	memcpy(ev_buf, buf, len);
	ev->ue_buf = ev_buf;

	/* new head */
	eq->eq_ev_head++;
	if (eq->eq_ev_head >= eq->eq_ev_end) {
		eq->eq_ev_head = eq->eq_ev_ring;
	}

	/* increment queued event count */
	atomic_inc(&eq->eq_num_events);

	return len;
}

static ssize_t
usdf_eq_readerr(struct fid_eq *feq, struct fi_eq_err_entry *entry,
		uint64_t flags)
{
	struct usdf_eq *eq;
	struct usdf_event *ev;
	uint64_t val;
	int ret;

	eq = eq_ftou(feq);
	pthread_spin_lock(&eq->eq_lock);

	/* make sure there is an error on top */
	if (usdf_eq_empty(eq) || !usdf_eq_error(eq)) {
		ret = -FI_EAGAIN;
		goto done;
	}

	switch (eq->eq_wait_obj) {
	case FI_WAIT_FD:
		/* consume entry from eventfd */
		ret = read(eq->eq_fd, &val, sizeof(val));
		if (ret != sizeof(val)) {
			if (ret == 0) {
				errno = FI_EIO;
			}
			ret = -errno;
			goto done;
		}
		break;
	default:
		break;
	}

	ev = eq->eq_ev_tail;
	memcpy(entry, ev->ue_buf, sizeof(*entry));

	/* update count */
	atomic_dec(&eq->eq_num_events);

	/* Free the event buf if needed */
	if (ev->ue_flags & USDF_EVENT_FLAG_FREE_BUF) {
		free(ev->ue_buf);
	}

	/* new tail */
	eq->eq_ev_tail++;
	if (eq->eq_ev_tail >= eq->eq_ev_end) {
		eq->eq_ev_tail = eq->eq_ev_ring;
	}

	ret = sizeof(*entry);

done:
	pthread_spin_unlock(&eq->eq_lock);
	return ret;
}

static ssize_t
usdf_eq_read(struct fid_eq *feq, uint32_t *event, void *buf, size_t len,
		uint64_t flags)
{
	struct usdf_eq *eq;
	int ret;

	eq = eq_ftou(feq);

	ret = -FI_EAGAIN;

	if (!usdf_eq_empty(eq)) {
		pthread_spin_lock(&eq->eq_lock);
		if (!usdf_eq_empty(eq)) {
			if (usdf_eq_error(eq)) {
				ret = -FI_EAVAIL;
			} else {
				ret = usdf_eq_read_event(eq, event, buf, len,
						flags);
			}
		}
		pthread_spin_unlock(&eq->eq_lock);
	}
	return ret;
}

static ssize_t
usdf_eq_write(struct fid_eq *feq, uint32_t event, const void *buf,
		size_t len, uint64_t flags)
{
	struct usdf_eq *eq;
	int ret;

	eq = eq_ftou(feq);

	pthread_spin_lock(&eq->eq_lock);

	/* EQ full? */
	if (atomic_get(&eq->eq_num_events) == eq->eq_ev_ring_size) {
		ret = -FI_EAGAIN;
		goto done;
	}

	ret = usdf_eq_write_event(eq, event, buf, len, flags);
done:
	pthread_spin_unlock(&eq->eq_lock);
	return ret;
}

/*
 * routines for FI_WAIT_FD
 */

static ssize_t
usdf_eq_read_fd(struct fid_eq *feq, uint32_t *event, void *buf, size_t len,
		uint64_t flags)
{
	struct usdf_eq *eq;
	uint64_t val;
	int ret;

	eq = eq_ftou(feq);

	/* no lock if empty */
	if (usdf_eq_empty(eq)) {
		return -FI_EAGAIN;
	}

	pthread_spin_lock(&eq->eq_lock);

	if (usdf_eq_empty(eq)) {
		ret = -FI_EAGAIN;
		goto done;
	}

	if (usdf_eq_error(eq)) {
		ret = -FI_EAVAIL;
		goto done;
	}

	if ((flags & FI_PEEK) != 0) {
		/* consume the event in eventfd */
		ret = read(eq->eq_fd, &val, sizeof(val));
		if (ret != sizeof(val)) {
			ret = -FI_EIO;
		}
	}

	ret =  usdf_eq_read_event(eq, event, buf, len, flags);
done:
	pthread_spin_unlock(&eq->eq_lock);
	return ret;
}

static ssize_t
usdf_eq_sread_fd(struct fid_eq *feq, uint32_t *event, void *buf, size_t len,
		int timeout, uint64_t flags)
{
	struct usdf_eq *eq;
	uint64_t val;
	struct pollfd pfd;
	int ret;

	eq = eq_ftou(feq);

	/* block awaiting event */
	pfd.fd = eq->eq_fd;
	pfd.events = POLLIN;
retry:
	ret = poll(&pfd, 1, timeout);
	if (ret < 0) {
		return -errno;
	} else if (ret == 0) {
		return -FI_ETIMEDOUT;
	}

	pthread_spin_lock(&eq->eq_lock);

	/* did someone steal our event? */
	if (usdf_eq_empty(eq)) {
		pthread_spin_unlock(&eq->eq_lock);
		goto retry;
	}

	if (usdf_eq_error(eq)) {
		ret = -FI_EAVAIL;
		goto done;
	}

	/* consume the event in eventfd */
	ret = read(eq->eq_fd, &val, sizeof(val));
	if (ret != sizeof(val)) {
		ret = -FI_EIO;
		goto done;
	}

	ret = usdf_eq_read_event(eq, event, buf, len, flags);

done:
	pthread_spin_unlock(&eq->eq_lock);
	return ret;
}

static ssize_t
usdf_eq_write_fd(struct fid_eq *feq, uint32_t event, const void *buf,
		size_t len, uint64_t flags)
{
	struct usdf_eq *eq;
	uint64_t val;
	int ret;
	int n;

	eq = eq_ftou(feq);

	pthread_spin_lock(&eq->eq_lock);

	/* EQ full? */
	if (atomic_get(&eq->eq_num_events) == eq->eq_ev_ring_size) {
		ret = -FI_EAGAIN;
		goto done;
	}

	ret = usdf_eq_write_event(eq, event, buf, len, flags);

	/* If successful, post to eventfd */
	if (ret >= 0) {
		val = 1;
		n = write(eq->eq_fd, &val, sizeof(val));
		if (n != sizeof(val)) {
			ret = -FI_EIO;
		}
		/* XXX unpost event? */
	}

done:
	pthread_spin_unlock(&eq->eq_lock);
	return ret;
}

ssize_t
usdf_eq_write_internal(struct usdf_eq *eq, uint32_t event, const void *buf,
		size_t len, uint64_t flags)
{
	uint64_t val;
	int ret;
	int n;

	pthread_spin_lock(&eq->eq_lock);

	/* EQ full? */
	if (atomic_get(&eq->eq_num_events) == eq->eq_ev_ring_size) {
		ret = -FI_EAGAIN;
		goto done;
	}

	ret = usdf_eq_write_event(eq, event, buf, len, flags);

	/* If successful, post to eventfd */
	if (ret >= 0 && eq->eq_wait_obj == FI_WAIT_FD) {
		val = 1;
		n = write(eq->eq_fd, &val, sizeof(val));
		if (n != sizeof(val)) {
			ret = -FI_EIO;
		}
	}

done:
	pthread_spin_unlock(&eq->eq_lock);
	return ret;
}

static const char *
usdf_eq_strerror(struct fid_eq *feq, int prov_errno, const void *err_data,
		 char *buf, size_t len)
{
	return NULL;
}

static int
usdf_eq_control(fid_t fid, int command, void *arg)
{
	struct usdf_eq *eq;

	eq = eq_fidtou(fid);

	switch (command) {
	case FI_GETWAIT:
		if (eq->eq_wait_obj == FI_WAIT_FD) {
			*(int *)arg = eq->eq_fd;
		} else {
			return -FI_ENODATA;
		}
		break;
	default:
		return -FI_EINVAL;
	}

	return 0;
}

static int
usdf_eq_close(fid_t fid)
{
	struct usdf_eq *eq;

	eq = eq_fidtou(fid);

	if (atomic_get(&eq->eq_refcnt) > 0) {
		return -FI_EBUSY;
	}
	atomic_dec(&eq->eq_fabric->fab_refcnt);

	/* release wait obj */
	switch (eq->eq_wait_obj) {
	case FI_WAIT_FD:
		close(eq->eq_fd);
		break;
	default:
		break;
	}

	free(eq);

	return 0;
}

static struct fi_ops_eq usdf_eq_ops = {
        .size = sizeof(struct fi_ops_eq),
        .read = usdf_eq_read,
        .sread = fi_no_eq_sread,
        .readerr = usdf_eq_readerr,
        .write = fi_no_eq_write,
        .strerror = usdf_eq_strerror
};

static struct fi_ops usdf_eq_fi_ops = {
        .size = sizeof(struct fi_ops),
        .close = usdf_eq_close,
        .bind = fi_no_bind,
        .control = usdf_eq_control,
        .ops_open = fi_no_ops_open,
};

int
usdf_eq_open(struct fid_fabric *fabric, struct fi_eq_attr *attr,
	struct fid_eq **feq, void *context)
{
	struct usdf_eq *eq;
	struct usdf_fabric *fab;
	int ret;

	fab = fab_ftou(fabric);

	eq = calloc(1, sizeof(*eq));
	if (eq == NULL) {
		ret = -errno;
		goto fail;
	}

	/* fill in the EQ struct */
	eq->eq_fid.fid.fclass = FI_CLASS_EQ;
	eq->eq_fid.fid.context = context;
	eq->eq_fid.fid.ops = &usdf_eq_fi_ops;
	eq->eq_fid.ops = &eq->eq_ops_data;
	eq->eq_wait_obj = attr->wait_obj;

	eq->eq_fabric = fab;
	atomic_init(&eq->eq_refcnt, 0);
	ret = pthread_spin_init(&eq->eq_lock, PTHREAD_PROCESS_PRIVATE);
	if (ret != 0) {
		ret = -ret;
		goto fail;
	}

	/* get baseline routines */
	eq->eq_ops_data = usdf_eq_ops;

	/* fill in sread based on wait type */
	switch (eq->eq_wait_obj) {
	case FI_WAIT_NONE:
		eq->eq_ops_data.read = usdf_eq_read;
		eq->eq_ops_data.sread = fi_no_eq_sread;
		eq->eq_ops_data.write = usdf_eq_write;
		break;
	case FI_WAIT_UNSPEC:	/* default to FD */
		eq->eq_wait_obj = FI_WAIT_FD;
		attr->wait_obj = FI_WAIT_FD;
		/* FALLSTHROUGH */
	case FI_WAIT_FD:
		eq->eq_ops_data.read = usdf_eq_read_fd;
		eq->eq_ops_data.sread = usdf_eq_sread_fd;
		eq->eq_ops_data.write = usdf_eq_write_fd;
		eq->eq_fd = eventfd(0, EFD_NONBLOCK | EFD_SEMAPHORE);
		if (eq->eq_fd == -1) {
			ret = -errno;
			goto fail;
		}
		break;
	default:
		ret = -FI_ENOSYS;
		goto fail;
	}

	/*
	 * Dis-allow write if requested
	 */
	if ((attr->flags & FI_WRITE) == 0) {
		eq->eq_ops_data.write = fi_no_eq_write;
	}

	/*
	 * Allocate and initialize event ring
	 */
	eq->eq_ev_ring = calloc(attr->size, sizeof(*eq->eq_ev_ring));
	eq->eq_ev_buf = calloc(attr->size, sizeof(*eq->eq_ev_buf));
	if (eq->eq_ev_ring == NULL || eq->eq_ev_buf == NULL) {
		ret = -errno;
		goto fail;
	}
	eq->eq_ev_head = eq->eq_ev_ring;
	eq->eq_ev_tail = eq->eq_ev_ring;
	eq->eq_ev_ring_size = attr->size;
	eq->eq_ev_end = eq->eq_ev_ring + eq->eq_ev_ring_size;
	atomic_init(&eq->eq_num_events, 0);

	atomic_inc(&eq->eq_fabric->fab_refcnt);
	*feq = eq_utof(eq);

	return 0;

fail:
	if (eq != NULL) {
		free(eq);
		if (eq->eq_ev_ring != NULL) {
			free(eq->eq_ev_ring);
		}
	}
	return ret;
}
