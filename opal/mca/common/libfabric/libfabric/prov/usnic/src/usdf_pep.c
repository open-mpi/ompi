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
#include <sys/epoll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"
#include "fi_enosys.h"

#include "fi_ext_usnic.h"
#include "usnic_direct.h"
#include "usd.h"
#include "usdf.h"
#include "usdf_cm.h"
#include "usdf_msg.h"

static int
usdf_pep_bind(fid_t fid, fid_t bfid, uint64_t flags)
{
	struct usdf_pep *pep;

	pep = pep_fidtou(fid);

	switch (bfid->fclass) {

	case FI_CLASS_EQ:
		if (pep->pep_eq != NULL) {
			return -FI_EINVAL;
		}
		pep->pep_eq = eq_fidtou(bfid);
		atomic_inc(&pep->pep_eq->eq_refcnt);
		break;
		
	default:
		return -FI_EINVAL;
	}

	return 0;
}

static struct fi_info *
usdf_pep_conn_info(struct usdf_connreq *crp)
{
	struct fi_info *ip;
	struct usdf_pep *pep;
	struct sockaddr_in *sin;
	struct usdf_fabric *fp;
	struct usdf_domain *udp;
	struct usd_device_attrs *dap;
	struct usdf_connreq_msg *reqp;

	pep = crp->cr_pep;
	fp = pep->pep_fabric;
	udp = LIST_FIRST(&fp->fab_domain_list);
	dap = fp->fab_dev_attrs;
	reqp = (struct usdf_connreq_msg *)crp->cr_data;

	/* If there is a domain, just copy info from there */
	if (udp != NULL) {
		ip = fi_dupinfo(udp->dom_info);
		if (ip == NULL) {
			return NULL;
		}

	/* no domains yet, make an info suitable for creating one */
	} else {
		ip = fi_allocinfo();
		if (ip == NULL) {
			return NULL;
		}

		ip->caps = USDF_MSG_CAPS;
		ip->mode = USDF_MSG_SUPP_MODE;
		ip->ep_attr->type = FI_EP_MSG;

		ip->addr_format = FI_SOCKADDR_IN;
		ip->src_addrlen = sizeof(struct sockaddr_in);
		sin = calloc(1, ip->src_addrlen);
		if (sin == NULL) {
			goto fail;
		}
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = dap->uda_ipaddr_be;
		ip->src_addr = sin;
		
		ip->ep_attr->protocol = FI_PROTO_RUDP;

		ip->fabric_attr->fabric = fab_utof(fp);
		ip->fabric_attr->name = strdup(fp->fab_attr.name);
		ip->fabric_attr->prov_name = strdup(fp->fab_attr.prov_name);
		ip->fabric_attr->prov_version = fp->fab_attr.prov_version;
		if (ip->fabric_attr->name == NULL ||
				ip->fabric_attr->prov_name == NULL) {
			goto fail;
		}
	}

	/* fill in dest addr */
	ip->dest_addrlen = ip->src_addrlen;
	sin = calloc(1, ip->dest_addrlen);
	if (sin == NULL) {
		goto fail;
	}
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = reqp->creq_ipaddr;
	sin->sin_port = reqp->creq_port;

	ip->dest_addr = sin;
	ip->connreq = (fi_connreq_t)crp;
	return ip;
fail:
	fi_freeinfo(ip);
	return NULL;
}

/*
 * Remove connection request from epoll list if not done already.
 * crp->cr_pollitem.pi_rtn is non-NULL when epoll() is active
 */
static int 
usdf_pep_creq_epoll_del(struct usdf_connreq *crp)
{
	int ret;
	struct usdf_pep *pep;

	pep = crp->cr_pep;

	if (crp->cr_pollitem.pi_rtn != NULL) {
		ret = epoll_ctl(pep->pep_fabric->fab_epollfd, EPOLL_CTL_DEL,
				crp->cr_sockfd, NULL);
		crp->cr_pollitem.pi_rtn = NULL;
		if (ret != 0) {
			ret = -errno;
		}
	} else {
		ret = 0;
	}
	return ret;
}

static int
usdf_pep_read_connreq(void *v)
{
	struct usdf_connreq *crp;
	struct usdf_pep *pep;
	struct usdf_connreq_msg *reqp;
	struct fi_eq_cm_entry *entry;
	size_t entry_len;
	int ret;
	int n;

	crp = v;
	pep = crp->cr_pep;

	n = read(crp->cr_sockfd, crp->cr_ptr, crp->cr_resid);
	if (n == -1) {
		usdf_cm_msg_connreq_failed(crp, -errno);
		return 0;
	}

	crp->cr_ptr += n;
	crp->cr_resid -= n;

	reqp = (struct usdf_connreq_msg *)crp->cr_data;

	if (crp->cr_resid == 0 && crp->cr_ptr == crp->cr_data + sizeof(*reqp)) {
		reqp->creq_datalen = ntohl(reqp->creq_datalen);
		crp->cr_resid = reqp->creq_datalen;
	}

	/* if resid is 0 now, completely done */
	if (crp->cr_resid == 0) {
		ret = usdf_pep_creq_epoll_del(crp);
		if (ret != 0) {
			usdf_cm_msg_connreq_failed(crp, ret);
			return 0;
		}
	
		/* create CONNREQ EQ entry */
		entry_len = sizeof(*entry) + reqp->creq_datalen;
		entry = malloc(entry_len);
		if (entry == NULL) {
			usdf_cm_msg_connreq_failed(crp, -errno);
			return 0;
		}

		entry->fid = &pep->pep_fid.fid;
		entry->info = usdf_pep_conn_info(crp);
		if (entry->info == NULL) {
			free(entry);
			usdf_cm_msg_connreq_failed(crp, -FI_ENOMEM);
			return 0;
		}
		memcpy(entry->data, reqp->creq_data, reqp->creq_datalen);
		ret = usdf_eq_write_internal(pep->pep_eq, FI_CONNREQ, entry,
				entry_len, 0);
		free(entry);
		if (ret != entry_len) {
			usdf_cm_msg_connreq_failed(crp, ret);
			return 0;
		}
	}

	return 0;
}

static int
usdf_pep_listen_cb(void *v)
{
	struct usdf_pep *pep;
	struct sockaddr_in sin;
	struct usdf_connreq *crp;
	struct epoll_event ev;
	socklen_t socklen;
	int ret;
	int s;

	pep = v;

	socklen = sizeof(sin);
	s = accept(pep->pep_sock, &sin, &socklen);
	if (s == -1) {
		/* ignore early failure */
		return 0;
	}
	crp = NULL;
	pthread_spin_lock(&pep->pep_cr_lock);
	if (!TAILQ_EMPTY(&pep->pep_cr_free)) {
		crp = TAILQ_FIRST(&pep->pep_cr_free);
		TAILQ_REMOVE_MARK(&pep->pep_cr_free, crp, cr_link);
		TAILQ_NEXT(crp, cr_link) = NULL;
	}
	pthread_spin_unlock(&pep->pep_cr_lock);

	/* no room for request, just drop it */
	if (crp == NULL) {
		/* XXX send response? */
		close(s);
		return 0;
	}

	crp->cr_sockfd = s;
	crp->cr_pep = pep;
	crp->cr_ptr = crp->cr_data;
	crp->cr_resid = sizeof(struct usdf_connreq_msg);

	crp->cr_pollitem.pi_rtn = usdf_pep_read_connreq;
	crp->cr_pollitem.pi_context = crp;
	ev.events = EPOLLIN;
	ev.data.ptr = &crp->cr_pollitem;

	ret = epoll_ctl(pep->pep_fabric->fab_epollfd, EPOLL_CTL_ADD,
			crp->cr_sockfd, &ev);
	if (ret == -1) {
		crp->cr_pollitem.pi_rtn = NULL;
		usdf_cm_msg_connreq_failed(crp, -errno);
		return 0;
	}

	TAILQ_INSERT_TAIL(&pep->pep_cr_pending, crp, cr_link);

	return 0;
}

static int
usdf_pep_listen(struct fid_pep *fpep)
{
	struct usdf_pep *pep;
	struct epoll_event ev;
	struct usdf_fabric *fp;
	int ret;

	pep = pep_ftou(fpep);
	fp = pep->pep_fabric;

	ret = listen(pep->pep_sock, pep->pep_backlog);
	if (ret != 0) {
		return -errno;
	}

	pep->pep_pollitem.pi_rtn = usdf_pep_listen_cb;
	pep->pep_pollitem.pi_context = pep;
	ev.events = EPOLLIN;
	ev.data.ptr = &pep->pep_pollitem;
	ret = epoll_ctl(fp->fab_epollfd, EPOLL_CTL_ADD, pep->pep_sock, &ev);
	if (ret == -1) {
		return -errno;
	}

	return 0;
}

static ssize_t
usdf_pep_cancel(fid_t fid, void *context)
{
	return -FI_EINVAL;
}

static int
usdf_pep_reject(struct fid_pep *pep, fi_connreq_t connreq,
		const void *param, size_t paramlen)
{
	return -FI_ENOSYS;
}

static void
usdf_pep_free_cr_lists(struct usdf_pep *pep)
{
	struct usdf_connreq *crp;

	while (!TAILQ_EMPTY(&pep->pep_cr_free)) {
		crp = TAILQ_FIRST(&pep->pep_cr_free);
		TAILQ_REMOVE(&pep->pep_cr_free, crp, cr_link);
		free(crp);
	}

	while (!TAILQ_EMPTY(&pep->pep_cr_pending)) {
		crp = TAILQ_FIRST(&pep->pep_cr_pending);
		TAILQ_REMOVE(&pep->pep_cr_pending, crp, cr_link);
		free(crp);
	}
}

static int
usdf_pep_grow_backlog(struct usdf_pep *pep)
{
	struct usdf_connreq *crp;
	size_t extra;

	extra = sizeof(struct usdf_connreq_msg) + pep->pep_cr_max_data;

	while (pep->pep_cr_alloced < pep->pep_backlog) {
		crp = calloc(1, sizeof(*crp) + extra);
		if (crp == NULL) {
			return -FI_ENOMEM;
		}
		pthread_spin_lock(&pep->pep_cr_lock);
		TAILQ_INSERT_TAIL(&pep->pep_cr_free, crp, cr_link);
		++pep->pep_cr_alloced;
		pthread_spin_unlock(&pep->pep_cr_lock);
	}
	return 0;
}

static int
usdf_pep_close(fid_t fid)
{
	struct usdf_pep *pep;

	pep = pep_fidtou(fid);
	if (atomic_get(&pep->pep_refcnt) > 0) {
		return -FI_EBUSY;
	}

	usdf_pep_free_cr_lists(pep);
	close(pep->pep_sock);
	if (pep->pep_eq != NULL) {
		atomic_dec(&pep->pep_eq->eq_refcnt);
	}
	atomic_dec(&pep->pep_fabric->fab_refcnt);
	free(pep);

	return 0;
}

struct fi_ops usdf_pep_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_pep_close,
	.bind = usdf_pep_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open
};

static struct fi_ops_ep usdf_pep_base_ops = {
	.size = sizeof(struct fi_ops_ep),
	.cancel = usdf_pep_cancel,
	.getopt = fi_no_getopt,
	.setopt = fi_no_setopt,
	.tx_ctx = fi_no_tx_ctx,
	.rx_ctx = fi_no_rx_ctx,
	.rx_size_left = fi_no_rx_size_left,
	.tx_size_left = fi_no_tx_size_left,
};

static struct fi_ops_cm usdf_pep_cm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = fi_no_getname,
	.getpeer = fi_no_getpeer,
	.connect = fi_no_connect,
	.listen = usdf_pep_listen,
	.accept = fi_no_accept,
	.reject = usdf_pep_reject,
	.shutdown = fi_no_shutdown,
};

int
usdf_pep_open(struct fid_fabric *fabric, struct fi_info *info,
	    struct fid_pep **pep_o, void *context)
{
	struct usdf_pep *pep;
	struct usdf_fabric *fp;
	int ret;
	int optval;

	if (info->ep_attr->type != FI_EP_MSG) {
		return -FI_ENODEV;
	}

	if ((info->caps & ~USDF_MSG_CAPS) != 0) {
		return -FI_EBADF;
	}

	fp = fab_ftou(fabric);

	pep = calloc(1, sizeof(*pep));
	if (pep == NULL) {
		return -FI_ENOMEM;
	}

	pep->pep_fid.fid.fclass = FI_CLASS_PEP;
	pep->pep_fid.fid.context = context;
	pep->pep_fid.fid.ops = &usdf_pep_ops;
	pep->pep_fid.ops = &usdf_pep_base_ops;
	pep->pep_fid.cm = &usdf_pep_cm_ops;
	pep->pep_fabric = fp;

	pep->pep_sock = socket(AF_INET, SOCK_STREAM, 0);
	if (pep->pep_sock == -1) {
		ret = -errno;
		goto fail;
	}
        ret = fcntl(pep->pep_sock, F_GETFL, 0);
        if (ret == -1) {
                ret = -errno;
                goto fail;
        }
        ret = fcntl(pep->pep_sock, F_SETFL, ret | O_NONBLOCK);
        if (ret == -1) {
                ret = -errno;
                goto fail;
        }

	/* set SO_REUSEADDR to prevent annoying "Address already in use" errors
	 * on successive runs of programs listening on a well known port */
	optval = 1;
	ret = setsockopt(pep->pep_sock, SOL_SOCKET, SO_REUSEADDR, &optval,
				sizeof(optval));
	if (ret == -1) {
		ret = -errno;
		goto fail;
	}

	ret = bind(pep->pep_sock, (struct sockaddr *)info->src_addr,
			info->src_addrlen);
	if (ret == -1) {
		ret = -errno;
		goto fail;
	}

	/* initialize connreq freelist */
	ret = pthread_spin_init(&pep->pep_cr_lock, PTHREAD_PROCESS_PRIVATE);
	if (ret != 0) {
		ret = -ret;
		goto fail;
	}
	TAILQ_INIT(&pep->pep_cr_free);
	TAILQ_INIT(&pep->pep_cr_pending);
	pep->pep_backlog = 10;
	ret = usdf_pep_grow_backlog(pep);
	if (ret != 0) {
		goto fail;
	}

	atomic_init(&pep->pep_refcnt, 0);
	atomic_inc(&fp->fab_refcnt);

	*pep_o = pep_utof(pep);
	return 0;

fail:
	if (pep != NULL) {
		usdf_pep_free_cr_lists(pep);
		if (pep->pep_sock != -1) {
			close(pep->pep_sock);
		}
		free(pep);
	}
	return ret;
}
