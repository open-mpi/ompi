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
#include <arpa/inet.h>

#include "rdma/fi_errno.h"
#include "fi_enosys.h"
#include "fi.h"

#include "usnic_direct.h"
#include "usnic_ip_utils.h"
#include "libnl_utils.h"
#include "usd.h"
#include "usd_queue.h"
#include "usd_dest.h"

#include "usdf.h"
#include "usdf_av.h"
#include "usdf_timer.h"

/* would like to move to include/rdma */
#include "fi_usnic.h"

static void
usdf_av_insert_async_complete(struct usdf_av_insert *insert)
{
	struct fi_eq_entry entry;
	struct usdf_av *av;

	av = insert->avi_av;

	entry.fid = &av->av_fid.fid;
	entry.context = insert->avi_context;
	entry.data = insert->avi_successes;
	usdf_eq_write_internal(av->av_eq,
		FI_COMPLETE, &entry, sizeof(entry), 0);

	pthread_spin_lock(&av->av_lock);

	atomic_dec(&av->av_active_inserts);
	if (atomic_get(&av->av_active_inserts) == 0 && av->av_closing) {
		free(av);
	} else {
		pthread_spin_unlock(&av->av_lock);
	}

	usdf_timer_free(av->av_domain->dom_fabric, insert->avi_timer);
	free(insert);
}

/*
 * A request failed, post an error event to the EQ
 */
static void
usdf_post_insert_request_error(struct usdf_av_insert *insert,
		struct usdf_av_req *req)
{
	struct fi_eq_err_entry err_entry;
	struct usdf_av *av;

	av = insert->avi_av;

	*req->avr_fi_addr = FI_ADDR_NOTAVAIL;
	free(req->avr_dest);

	err_entry.fid = &av->av_fid.fid;
	err_entry.context = insert->avi_context;
	err_entry.data = req - (struct usdf_av_req *)(insert + 1);
	err_entry.err = -req->avr_status;

	usdf_eq_write_internal(av->av_eq, FI_COMPLETE,
		&err_entry, sizeof(err_entry),
		USDF_EVENT_FLAG_ERROR);
}

/*
 * Called by progression thread to look for AV completions on this domain
 */
static void
usdf_av_insert_progress(void *v)
{
	int ret;
	struct usdf_av_insert *insert;
	struct usdf_fabric *fp;
	struct usd_dest *dest;
	struct usdf_av_req *req;
	struct usdf_av_req *tmpreq;
	struct usd_device_attrs *dap;
	uint64_t now;
	uint8_t *eth;

	insert = v;
	fp = insert->avi_av->av_domain->dom_fabric;
	dap = fp->fab_dev_attrs;

	TAILQ_FOREACH_SAFE(req, tmpreq, &insert->avi_req_list, avr_link) {

		dest = req->avr_dest;
		eth = &dest->ds_dest.ds_udp.u_hdr.uh_eth.ether_dhost[0];
		ret = usnic_arp_lookup(dap->uda_ifname,
				req->avr_daddr_be, fp->fab_arp_sockfd, eth);

		/* anything besides -EAGAIN means request is completed */
		if (ret != EAGAIN) {
			TAILQ_REMOVE(&insert->avi_req_list, req, avr_link);
			req->avr_status = -ret;

			if (ret == 0) {
				++insert->avi_successes;
				*(struct usd_dest **)req->avr_fi_addr = dest;
			} else {
				usdf_post_insert_request_error(insert, req);
			}
		}
	}

	/* Time for a new ARP? */
	now = usdf_get_ms();
	if (now - insert->avi_last_arp_time > USDF_AV_ARP_INTERVAL) {

		/* If no more ARP requests left, fail all remaining requests */
		if (insert->avi_arps_left == 0) {
			TAILQ_FOREACH(req, &insert->avi_req_list, avr_link) {
				req->avr_status = -FI_EHOSTUNREACH;
				usdf_post_insert_request_error(insert, req);
			}
			TAILQ_INIT(&insert->avi_req_list);

		/* Trigger an ARP request for all pending requests */
		} else {
			TAILQ_FOREACH_SAFE(req, tmpreq,
					&insert->avi_req_list, avr_link) {
				ret = usnic_arp_request(req->avr_daddr_be,
						fp->fab_arp_sockfd);
				if (ret != 0) {
					req->avr_status = -ret;
					TAILQ_REMOVE(&insert->avi_req_list,
							req, avr_link);
					usdf_post_insert_request_error(insert,
							req);
				}
			}

			insert->avi_last_arp_time = now;
			--insert->avi_arps_left;
		}
	}

	/* If no more pending requests, all done! */
	if (TAILQ_EMPTY(&insert->avi_req_list)) {
		usdf_av_insert_async_complete(insert);
	} else {
		/* retry in 1 ms */
		usdf_timer_set(fp, insert->avi_timer, 1);
	}

}

static int
usdf_am_insert_async(struct fid_av *fav, const void *addr, size_t count,
			  fi_addr_t *fi_addr, uint64_t flags, void *context)
{
	const struct sockaddr_in *sin;
	struct usd_device_attrs *dap;
	struct usdf_av_insert *insert;
	struct usdf_av_req *req;
	struct usdf_av *av;
	struct usdf_fabric *fp;
	int ret;
	int i;

	if ((flags & ~FI_MORE) != 0) {
		return -FI_EBADFLAGS;
	}

	av = av_ftou(fav);
	fp = av->av_domain->dom_fabric;
	dap = fp->fab_dev_attrs;

	if (av->av_flags & FI_READ) {
		return -FI_EACCES;
	}
	if (av->av_eq == NULL) {
		return -FI_ENOEQ;
	}

	sin = addr;

	/* allocate an insert record and N requests */
	insert = calloc(1, sizeof(*insert) + count * sizeof(*req));
	if (insert == NULL) {
		return -errno;
	}
	insert->avi_av = av;
	insert->avi_context = context;
	ret = usdf_timer_alloc(usdf_av_insert_progress, insert,
			&insert->avi_timer);
	if (ret != 0) {
		goto fail;
	}
	TAILQ_INIT(&insert->avi_req_list);
	insert->avi_arps_left = USDF_AV_MAX_ARPS;

	/* If no addresses, complete now */
	if (count == 0) {
		usdf_av_insert_async_complete(insert);
		return 0;
	}

	atomic_inc(&av->av_active_inserts);

	req = (struct usdf_av_req *)(insert + 1);

	for (i = 0; i < count; i++) {
		req->avr_fi_addr = &fi_addr[i];

		/* find the address we actually need to look up */
		ret = usnic_nl_rt_lookup(dap->uda_ipaddr_be,
				sin->sin_addr.s_addr, dap->uda_ifindex,
				&req->avr_daddr_be);
		if (ret != 0) {
			if (ret == EHOSTUNREACH) {
				req->avr_status = -FI_EHOSTUNREACH;
				usdf_post_insert_request_error(insert, req);
			} else {
				ret = -ret;
				goto fail;
			}

		} else {
			if (req->avr_daddr_be == 0) {
				req->avr_daddr_be = sin->sin_addr.s_addr;
			}
			req->avr_dest = calloc(1, sizeof(*req->avr_dest));
			if (req->avr_dest == NULL) {
				ret = -FI_ENOMEM;
				goto fail;
			}
			usd_fill_udp_dest(req->avr_dest, dap,
					sin->sin_addr.s_addr, sin->sin_port);

			TAILQ_INSERT_TAIL(&insert->avi_req_list, req, avr_link);
		}

		++sin;
		++req;
	}

	/* resolve all addresses we can */
	usdf_av_insert_progress(insert);

	return count;

fail:
	if (insert != NULL) {
		if (insert->avi_timer != NULL) {
			usdf_timer_free(fp, insert->avi_timer);
		}
		free(insert);
	}
	return ret;
}

static int
usdf_am_insert_sync(struct fid_av *fav, const void *addr, size_t count,
			  fi_addr_t *fi_addr, uint64_t flags, void *context)
{
	const struct sockaddr_in *sin;
	struct usdf_av *av;
	struct usd_dest *dest;
	int ret_count;
	int ret;
	int i;

	if ((flags & ~FI_MORE) != 0) {
		return -FI_EBADFLAGS;
	}

	av = av_ftou(fav);

	ret_count = 0;
	sin = addr;

	/* XXX parallelize */
	for (i = 0; i < count; i++) {
		ret = usd_create_dest(av->av_domain->dom_dev,
				sin->sin_addr.s_addr, sin->sin_port,
				&dest);
		if (ret != 0) {
			fi_addr[i] = FI_ADDR_NOTAVAIL;
		} else {
			fi_addr[i] = (fi_addr_t)dest;
			++ret_count;
		}
		++sin;
	}

	return ret_count;
}

static int
usdf_am_remove(struct fid_av *fav, fi_addr_t *fi_addr, size_t count,
			  uint64_t flags)
{
	struct usd_dest *dest;
	struct usdf_av *av;

	av = av_ftou(fav);

	if (av->av_flags & FI_READ) {
		return -FI_EACCES;
	}

	// XXX
	dest = (struct usd_dest *)(uintptr_t)fi_addr;
	usd_destroy_dest(dest);

	return 0;
}

static int
usdf_am_lookup(struct fid_av *av, fi_addr_t fi_addr, void *addr,
			  size_t *addrlen)
{
	struct usd_dest *dest;
	struct sockaddr_in sin;
	size_t copylen;

	dest = (struct usd_dest *)(uintptr_t)fi_addr;

	if (*addrlen < sizeof(sin)) {
		copylen = *addrlen;
	} else {
		copylen = sizeof(sin);
	}

	sin.sin_family = AF_INET;
	usd_expand_dest(dest, &sin.sin_addr.s_addr, &sin.sin_port);
	memcpy(addr, &sin, copylen);

	*addrlen = sizeof(sin);
	return 0;
}

static const char *
usdf_av_straddr(struct fid_av *av, const void *addr,
				    char *buf, size_t *len)
{
	const struct sockaddr_in *sin;
	char straddr[24];
	int size;

	sin = addr;
	size = snprintf(straddr, sizeof straddr, "%s:%d",
			inet_ntoa(sin->sin_addr), sin->sin_port);
	snprintf(buf, *len, "%s", straddr);
	*len = size + 1;
	return buf;
}

static int
usdf_av_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct usdf_av *av;

	av = av_fidtou(fid);

	switch (bfid->fclass) {
	case FI_CLASS_EQ:
		if (av->av_eq != NULL) {
			return -FI_EINVAL;
		}
		av->av_eq = eq_fidtou(bfid);
		atomic_inc(&av->av_eq->eq_refcnt);
		break;
	default:
		return -FI_EINVAL;
	}

	return 0;
}

static int
usdf_av_close(struct fid *fid)
{
	struct usdf_av *av;

	av = container_of(fid, struct usdf_av, av_fid.fid);
	if (atomic_get(&av->av_refcnt) > 0) {
		return -FI_EBUSY;
	}

	pthread_spin_lock(&av->av_lock);

	if (av->av_eq != NULL) {
		atomic_dec(&av->av_eq->eq_refcnt);
	}
	atomic_dec(&av->av_domain->dom_refcnt);

	if (atomic_get(&av->av_active_inserts) > 0) {
		av->av_closing = 1;
		pthread_spin_unlock(&av->av_lock);
	} else {
		free(av);
	}
	return 0;
}

static int
usdf_am_get_distance(struct fid_av *fav, void *addr, int *metric_o)
{
	struct usdf_av *av;
	struct usdf_domain *udp;
	struct sockaddr_in *sin;
	int ret;

	av = av_ftou(fav);
	udp = av->av_domain;
	sin = addr;

	ret = usd_get_dest_distance(udp->dom_dev,
			sin->sin_addr.s_addr, metric_o);
	return ret;
}

static struct fi_usnic_ops_av usdf_usnic_ops_av = {
	.size = sizeof(struct fi_usnic_ops_fabric),
	.get_distance = usdf_am_get_distance,
};

static int
usdf_av_ops_open(struct fid *fid, const char *ops_name, uint64_t flags,
		void **ops, void *context)
{
	if (strcmp(ops_name, FI_USNIC_AV_OPS_1) == 0) {
		*ops = &usdf_usnic_ops_av;
	} else {
		return -FI_EINVAL;
	}

	return 0;
}

static struct fi_ops usdf_av_fi_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_av_close,
	.bind = usdf_av_bind,
	.control = fi_no_control,
	.ops_open = usdf_av_ops_open,
};

static struct fi_ops_av usdf_am_ops_async = {
	.size = sizeof(struct fi_ops_av),
	.insert = usdf_am_insert_async,
	.remove = usdf_am_remove,
	.lookup = usdf_am_lookup,
	.straddr = usdf_av_straddr
};

static struct fi_ops_av usdf_am_ops_sync = {
	.size = sizeof(struct fi_ops_av),
	.insert = usdf_am_insert_sync,
	.remove = usdf_am_remove,
	.lookup = usdf_am_lookup,
	.straddr = usdf_av_straddr
};

int
usdf_av_open(struct fid_domain *domain, struct fi_av_attr *attr,
		 struct fid_av **av_o, void *context)
{
	struct usdf_domain *udp;
	struct usdf_av *av;

	if (attr->name != NULL) {
		return -FI_ENOSYS;
	}
	if ((attr->flags & ~(FI_EVENT | FI_READ)) != 0) {
		return -FI_ENOSYS;
	}

	udp = dom_ftou(domain);

	av = calloc(1, sizeof(*av));
	if (av == NULL) {
		return -FI_ENOMEM;
	}

	if (attr->flags & FI_EVENT) {
		av->av_fid.ops = &usdf_am_ops_async;
	} else {
		av->av_fid.ops = &usdf_am_ops_sync;
	}
	av->av_fid.fid.fclass = FI_CLASS_AV;
	av->av_fid.fid.context = context;
	av->av_fid.fid.ops = &usdf_av_fi_ops;
	av->av_flags = attr->flags;

	pthread_spin_init(&av->av_lock, PTHREAD_PROCESS_PRIVATE);
	atomic_init(&av->av_active_inserts, 0);

	atomic_init(&av->av_refcnt, 0);
	atomic_inc(&udp->dom_refcnt);
	av->av_domain = udp;

	*av_o = av_utof(av);
	return 0;
}
