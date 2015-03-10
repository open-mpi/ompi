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
#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/epoll.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"

#include "usnic_direct.h"
#include "usdf.h"
#include "usdf_endpoint.h"
#include "usdf_dgram.h"
#include "usdf_msg.h"
#include "usdf_av.h"
#include "usdf_cm.h"

static void
usdf_cm_msg_connreq_cleanup(struct usdf_connreq *crp)
{
	struct usdf_ep *ep;
	struct usdf_pep *pep;
	struct usdf_fabric *fp;

	ep = crp->cr_ep;
	pep = crp->cr_pep;
	if (pep != NULL) {
		fp = pep->pep_fabric;
	} else {
		fp = ep->ep_domain->dom_fabric;
	}

	if (crp->cr_pollitem.pi_rtn != NULL) {
		(void) epoll_ctl(fp->fab_epollfd, EPOLL_CTL_DEL, crp->cr_sockfd, NULL);
		crp->cr_pollitem.pi_rtn = NULL;
	}
	if (crp->cr_sockfd != -1) {
		close(crp->cr_sockfd);
		crp->cr_sockfd = -1;
	}

	/* If there is a passive endpoint, recycle the crp */
	if (pep != NULL) {
		if (TAILQ_ON_LIST(crp, cr_link)) {
			TAILQ_REMOVE(&pep->pep_cr_pending, crp, cr_link);
		}
		TAILQ_INSERT_TAIL(&pep->pep_cr_free, crp, cr_link);
	} else {
		free(crp);
	}
}

static int
usdf_cm_msg_accept_complete(struct usdf_connreq *crp)
{
	struct usdf_ep *ep;
	struct fi_eq_cm_entry entry;
	int ret;

	ep = crp->cr_ep;

	/* post EQ entry */
	entry.fid = ep_utofid(ep);
	entry.info = NULL;
	ret = usdf_eq_write_internal(ep->ep_eq, FI_CONNECTED, &entry,
			sizeof(entry), 0);
	if (ret != sizeof(entry)) {
		usdf_cm_msg_connreq_failed(crp, ret);
		return 0;
	}

	usdf_cm_msg_connreq_cleanup(crp);

	return 0;
}

int
usdf_cm_msg_accept(struct fid_ep *fep, const void *param, size_t paramlen)
{
	struct usdf_ep *ep;
	struct usdf_rx *rx;
	struct usdf_domain *udp;
	struct usdf_fabric *fp;
	struct usdf_connreq *crp;
	struct usdf_connreq_msg *reqp;
	struct usd_qp_impl *qp;
	int ret;
	int n;

	ep = ep_ftou(fep);
	udp = ep->ep_domain;
	fp = udp->dom_fabric;
	crp = ep->e.msg.ep_connreq;
	if (crp == NULL) {
		return -FI_ENOTCONN;
	}
	if (ep->ep_eq == NULL) {
		return -FI_ENOEQ;
	}
	crp->cr_ep = ep;
	reqp = (struct usdf_connreq_msg *)crp->cr_data;

	ep->e.msg.ep_lcl_peer_id = ntohs(reqp->creq_peer_id);

	/* start creating the dest early */
	ret = usd_create_dest_with_mac(udp->dom_dev, reqp->creq_ipaddr,
			reqp->creq_port, reqp->creq_mac,
			&ep->e.msg.ep_dest);
	if (ret != 0) {
		goto fail;
	}

	ret = usdf_ep_msg_get_queues(ep);
	if (ret != 0) {
		goto fail;
	}
	rx = ep->ep_rx;
	qp = to_qpi(rx->rx_qp);

	/* allocate a peer ID */
	ep->e.msg.ep_rem_peer_id = udp->dom_next_peer;
	udp->dom_peer_tab[udp->dom_next_peer] = ep;
	++udp->dom_next_peer;

	crp->cr_ptr = crp->cr_data;
	crp->cr_resid = sizeof(*reqp) + paramlen;

	reqp->creq_peer_id = htons(ep->e.msg.ep_rem_peer_id);
	reqp->creq_ipaddr = fp->fab_dev_attrs->uda_ipaddr_be;
	reqp->creq_port =
		qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;
	memcpy(reqp->creq_mac, fp->fab_dev_attrs->uda_mac_addr, ETH_ALEN);
	reqp->creq_result = htonl(0);
	reqp->creq_datalen = htonl(paramlen);
	memcpy(reqp->creq_data, param, paramlen);

	n = write(crp->cr_sockfd, crp->cr_ptr, crp->cr_resid);
	if (n == -1) {
		usdf_cm_msg_connreq_cleanup(crp);
		ret = -errno;
		goto fail;
	}

	crp->cr_resid -= n;
	if (crp->cr_resid == 0) {
		usdf_cm_msg_accept_complete(crp);
	} else {
		// XXX set up epoll junk to send rest
	}

	return 0;
fail:
	free(ep->e.msg.ep_dest);
	/* XXX release queues */
	return ret;
}

/*
 * Connection request attempt failed
 */
void
usdf_cm_msg_connreq_failed(struct usdf_connreq *crp, int error)
{
        struct usdf_pep *pep;
        struct usdf_ep *ep;
        struct usdf_eq *eq;
	fid_t fid;
        struct fi_eq_err_entry err;

        pep = crp->cr_pep;
        ep = crp->cr_ep;
	if (ep != NULL) {
		fid = ep_utofid(ep);
		eq = ep->ep_eq;
		ep->ep_domain->dom_peer_tab[ep->e.msg.ep_rem_peer_id] = NULL;
	} else {
		fid = pep_utofid(pep);
		eq = pep->pep_eq;
	}

        err.fid = fid;
        err.context = NULL;
        err.data = 0;
        err.err = -error;
        err.prov_errno = 0;
        err.err_data = NULL;
        err.err_data_size = 0;
        usdf_eq_write_internal(eq, 0, &err, sizeof(err), USDF_EVENT_FLAG_ERROR);

        usdf_cm_msg_connreq_cleanup(crp);
}

/*
 * read connection request response from the listener
 */
static int
usdf_cm_msg_connect_cb_rd(void *v)
{
	struct usdf_connreq *crp;
	struct usdf_ep *ep;
	struct usdf_fabric *fp;
	struct usdf_domain *udp;
	struct usdf_connreq_msg *reqp;
	struct fi_eq_cm_entry *entry;
	size_t entry_len;
	int ret;

	crp = v;
	ep = crp->cr_ep;
	fp = ep->ep_domain->dom_fabric;

	ret = read(crp->cr_sockfd, crp->cr_ptr, crp->cr_resid);
	if (ret == -1) {
		usdf_cm_msg_connreq_failed(crp, -errno);
		return 0;
	}

	crp->cr_resid -= ret;
	reqp = (struct usdf_connreq_msg *)crp->cr_data;
	if (crp->cr_resid == 0 && crp->cr_ptr == crp->cr_data + sizeof(*reqp)) {
		reqp->creq_datalen = ntohl(reqp->creq_datalen);
		crp->cr_resid = reqp->creq_datalen;
	}

	/* if resid is 0 now, completely done */
	if (crp->cr_resid == 0) {
		ret = epoll_ctl(fp->fab_epollfd, EPOLL_CTL_DEL,
				crp->cr_sockfd, NULL);
		close(crp->cr_sockfd);
		crp->cr_sockfd = -1;

		entry_len = sizeof(*entry) + reqp->creq_datalen;
		entry = malloc(entry_len);
		if (entry == NULL) {
			usdf_cm_msg_connreq_failed(crp, -errno);
			return 0;
		}
		
		udp = ep->ep_domain;
		ep->e.msg.ep_lcl_peer_id = ntohs(reqp->creq_peer_id);
		ret = usd_create_dest_with_mac(udp->dom_dev, reqp->creq_ipaddr,
				reqp->creq_port, reqp->creq_mac,
				&ep->e.msg.ep_dest);
		if (ret != 0) {
			free(entry);
			usdf_cm_msg_connreq_failed(crp, ret);
			return 0;
		}

		entry->fid = ep_utofid(ep);
		entry->info = NULL;
		memcpy(entry->data, reqp->creq_data, reqp->creq_datalen);
		ret = usdf_eq_write_internal(ep->ep_eq, FI_CONNECTED, entry,
				entry_len, 0);
		free(entry);
		if (ret != entry_len) {
			free(ep->e.msg.ep_dest);
			ep->e.msg.ep_dest = NULL;
			usdf_cm_msg_connreq_failed(crp, ret);
			return 0;
		}

		usdf_cm_msg_connreq_cleanup(crp);
	}
	return 0;
}

/*
 * Write connection request data to the listener
 * Once everything is written, switch over into listening mode to
 * capture the listener response.
 */
static int
usdf_cm_msg_connect_cb_wr(void *v)
{
	struct usdf_connreq *crp;
	struct usdf_ep *ep;
	struct usdf_fabric *fp;
	struct epoll_event ev;
	int ret;

	crp = v;
	ep = crp->cr_ep;
	fp = ep->ep_domain->dom_fabric;

	ret = write(crp->cr_sockfd, crp->cr_ptr, crp->cr_resid);
	if (ret == -1) {
		usdf_cm_msg_connreq_failed(crp, -errno);
		return 0;
	}

	crp->cr_resid -= ret;
	if (crp->cr_resid == 0) {
		crp->cr_pollitem.pi_rtn = usdf_cm_msg_connect_cb_rd;
		ev.events = EPOLLIN;
		ev.data.ptr = &crp->cr_pollitem;
		ret = epoll_ctl(fp->fab_epollfd, EPOLL_CTL_MOD,
				crp->cr_sockfd, &ev);
		if (ret != 0) {
			usdf_cm_msg_connreq_failed(crp, -errno);
			return 0;
		}

		crp->cr_ptr = crp->cr_data;
		crp->cr_resid = sizeof(struct usdf_connreq_msg);
	}
	return 0;
}

int
usdf_cm_msg_connect(struct fid_ep *fep, const void *addr,
		const void *param, size_t paramlen)
{
	struct usdf_connreq *crp;
	struct usdf_ep *ep;
	struct usdf_rx *rx;
	struct usdf_domain *udp;
	const struct sockaddr_in *sin;
	struct epoll_event ev;
	struct usdf_fabric *fp;
	struct usdf_connreq_msg *reqp;
	struct usd_qp_impl *qp;
	int ret;

	ep = ep_ftou(fep);
	udp = ep->ep_domain;
	fp = udp->dom_fabric;
	sin = addr;
	crp = NULL;

	crp = calloc(1, sizeof(*crp) + sizeof(struct usdf_connreq_msg) +
			paramlen);
	if (crp == NULL) {
		ret = -errno;
		goto fail;
	}

	crp->cr_sockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (crp->cr_sockfd == -1) {
		ret = -errno;
		goto fail;
	}

	ret = fcntl(crp->cr_sockfd, F_GETFL, 0);
	if (ret == -1) {
		ret = -errno;
		goto fail;
	}
	ret = fcntl(crp->cr_sockfd, F_SETFL, ret | O_NONBLOCK);
	if (ret == -1) {
		ret = -errno;
		goto fail;
	}

	ret = usdf_ep_msg_get_queues(ep);
	if (ret != 0) {
		goto fail;
	}
	rx = ep->ep_rx;
	qp = to_qpi(rx->rx_qp);

	ret = connect(crp->cr_sockfd, (struct sockaddr *)sin, sizeof(*sin));
	if (ret != 0 && errno != EINPROGRESS) {
		ret = -errno;
		goto fail;
	}

	/* register for notification when connect completes */
	crp->cr_pollitem.pi_rtn = usdf_cm_msg_connect_cb_wr;
	crp->cr_pollitem.pi_context = crp;
	ev.events = EPOLLOUT;
	ev.data.ptr = &crp->cr_pollitem;
	ret = epoll_ctl(fp->fab_epollfd, EPOLL_CTL_ADD, crp->cr_sockfd, &ev);
	if (ret != 0) {
		crp->cr_pollitem.pi_rtn = NULL;
		ret = -errno;
		goto fail;
	}

	/* allocate remote peer ID */
	ep->e.msg.ep_rem_peer_id = udp->dom_next_peer;
	udp->dom_peer_tab[udp->dom_next_peer] = ep;
	++udp->dom_next_peer;

	crp->cr_ep = ep;
	reqp = (struct usdf_connreq_msg *)crp->cr_data;
	crp->cr_ptr = crp->cr_data;
	crp->cr_resid =  sizeof(*reqp) + paramlen;

	reqp->creq_peer_id = htons(ep->e.msg.ep_rem_peer_id);
	reqp->creq_ipaddr = fp->fab_dev_attrs->uda_ipaddr_be;
	reqp->creq_port =
		qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;
	memcpy(reqp->creq_mac, fp->fab_dev_attrs->uda_mac_addr, ETH_ALEN);
	reqp->creq_datalen = htonl(paramlen);
	memcpy(reqp->creq_data, param, paramlen);

	return 0;

fail:
	if (crp != NULL) {
		if (crp->cr_sockfd != -1) {
			close(crp->cr_sockfd);
		}
		free(crp);
	}
	usdf_ep_msg_release_queues(ep);
	return ret;
}

int
usdf_cm_msg_shutdown(struct fid_ep *ep, uint64_t flags)
{
	return -FI_ENOSYS;
}

/*
 * Check a message CQ for completions and progress the send engine as needed,
 * create completions for the app if anything needs to be percolated up
 */
int
usdf_cq_msg_poll(struct usd_cq *ucq, struct usd_completion *comp)
{
	return -EAGAIN;
}

/*
 * Return local address of an EP
 */
int usdf_cm_rdm_getname(fid_t fid, void *addr, size_t *addrlen)
{
	struct usdf_ep *ep;
	struct usdf_rx *rx;
	struct sockaddr_in sin;
	size_t copylen;

	ep = ep_fidtou(fid);
	rx = ep->ep_rx;

	copylen = sizeof(sin);
	if (copylen > *addrlen) {
		copylen = *addrlen;
	}
	*addrlen = sizeof(sin);

	memset(&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr =
		ep->ep_domain->dom_fabric->fab_dev_attrs->uda_ipaddr_be;
	if (rx == NULL || rx->rx_qp == NULL) {
		sin.sin_port = 0;
	} else {
		sin.sin_port = to_qpi(rx->rx_qp)->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;
	}
	memcpy(addr, &sin, copylen);

	if (copylen < sizeof(sin)) {
		return -FI_ETOOSMALL;
	} else {
		return 0;
	}
}
