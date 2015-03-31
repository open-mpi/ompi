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

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_prov.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_errno.h>
#include "fi.h"
#include "fi_enosys.h"

#include "usd.h"
#include "usdf.h"
#include "usdf_endpoint.h"
#include "usdf_rudp.h"
#include "usdf_cq.h"
#include "usdf_cm.h"
#include "usdf_av.h"
#include "usdf_timer.h"
#include "usdf_rdm.h"

static int
usdf_tx_rdm_enable(struct usdf_tx *tx)
{
	struct usdf_rdm_qe *wqe;
	struct usdf_domain *udp;
	struct usdf_cq_hard *hcq;
	struct usd_filter filt;
	int ret;
	int i;

	udp = tx->tx_domain;

	hcq = tx->t.rdm.tx_hcq;
	if (hcq == NULL) {
		return -FI_ENOCQ;
	}

	/* XXX temp until we can allocate WQ and RQ independently */
	filt.uf_type = USD_FTY_UDP;
	filt.uf_filter.uf_udp.u_port = 0;
	ret = usd_create_qp(udp->dom_dev,
			USD_QTR_UDP,
			USD_QTY_NORMAL,
			hcq->cqh_ucq,
			hcq->cqh_ucq,
			udp->dom_fabric->fab_dev_attrs->uda_max_send_credits,
			udp->dom_fabric->fab_dev_attrs->uda_max_recv_credits,
			&filt,
			&tx->tx_qp);
	if (ret != 0) {
		goto fail;
	}
	tx->tx_qp->uq_context = tx;

	/* rdm send queue */
	tx->t.rdm.tx_wqe_buf = malloc(tx->tx_attr.size *
			sizeof(struct usdf_rdm_qe));
	if (tx->t.rdm.tx_wqe_buf == NULL) {
		ret = -errno;
		goto fail;
	}

	/* populate free list */
	TAILQ_INIT(&tx->t.rdm.tx_free_wqe);
	wqe = tx->t.rdm.tx_wqe_buf;
	for (i = 0; i < tx->tx_attr.size; ++i) {
		TAILQ_INSERT_TAIL(&tx->t.rdm.tx_free_wqe, wqe, rd_link);
		++wqe;
	}

	return 0;

fail:
	if (tx->t.rdm.tx_wqe_buf != NULL) {
		free(tx->t.rdm.tx_wqe_buf);
		tx->t.rdm.tx_wqe_buf = NULL;
		TAILQ_INIT(&tx->t.rdm.tx_free_wqe);
	}
	if (tx->tx_qp != NULL) {
		usd_destroy_qp(tx->tx_qp);
	}
	return ret;
}

static int
usdf_rx_rdm_enable(struct usdf_rx *rx)
{
	struct usdf_domain *udp;
	struct usdf_cq_hard *hcq;
	struct usdf_rdm_qe *rqe;
	struct usd_filter filt;
	struct usd_qp_impl *qp;
	uint8_t *ptr;
	size_t mtu;
	int ret;
	int i;

	udp = rx->rx_domain;

	hcq = rx->r.rdm.rx_hcq;
	if (hcq == NULL) {
		return -FI_ENOCQ;
	}

	/* XXX temp until we can allocate WQ and RQ independently */
	filt.uf_type = USD_FTY_UDP_SOCK;
	filt.uf_filter.uf_udp_sock.u_sock = rx->r.rdm.rx_sock;
	ret = usd_create_qp(udp->dom_dev,
			USD_QTR_UDP,
			USD_QTY_NORMAL,
			hcq->cqh_ucq,
			hcq->cqh_ucq,
			udp->dom_fabric->fab_dev_attrs->uda_max_send_credits,
			udp->dom_fabric->fab_dev_attrs->uda_max_recv_credits,
			&filt,
			&rx->rx_qp);
	if (ret != 0) {
		goto fail;
	}
	rx->rx_qp->uq_context = rx;
	qp = to_qpi(rx->rx_qp);

	/* receive buffers */
	mtu = rx->rx_domain->dom_fabric->fab_dev_attrs->uda_mtu;
	ret = usd_alloc_mr(rx->rx_domain->dom_dev,
			qp->uq_rq.urq_num_entries * mtu,
			(void **)&rx->r.rdm.rx_bufs);
	if (ret != 0) {
		goto fail;
	}

	/* post all the buffers */
	ptr = rx->r.rdm.rx_bufs;
	for (i = 0; i < qp->uq_rq.urq_num_entries - 1; ++i) {
		usdf_rdm_post_recv(rx, ptr, mtu);
		ptr += mtu;
	}

	/* rdm recv queue */
	rx->r.rdm.rx_rqe_buf = malloc(rx->rx_attr.size *
			sizeof(struct usdf_rdm_qe));
	if (rx->r.rdm.rx_rqe_buf == NULL) {
		ret = -errno;
		goto fail;
	}

	/* populate free list */
	TAILQ_INIT(&rx->r.rdm.rx_free_rqe);
	rqe = rx->r.rdm.rx_rqe_buf;
	for (i = 0; i < rx->rx_attr.size; ++i) {
		TAILQ_INSERT_TAIL(&rx->r.rdm.rx_free_rqe, rqe, rd_link);
		++rqe;
	}

	return 0;

fail:
	if (rx->r.rdm.rx_rqe_buf != NULL) {
		free(rx->r.rdm.rx_rqe_buf);
		rx->r.rdm.rx_rqe_buf = NULL;
		TAILQ_INIT(&rx->r.rdm.rx_free_rqe);
	}
	if (rx->r.rdm.rx_bufs != NULL) {
		usd_free_mr(rx->r.rdm.rx_bufs);
		rx->r.rdm.rx_bufs = NULL;
	}
	if (rx->rx_qp != NULL) {
		usd_destroy_qp(rx->rx_qp);
	}
	return ret;
}

/*
 * Allocate any missing queue resources for this endpoint
 */
static int
usdf_ep_rdm_get_queues(struct usdf_ep *ep)
{
	struct usdf_tx *tx;
	struct usdf_rx *rx;
	int ret;

	/* Must have TX context at this point */
	tx = ep->ep_tx;
	if (tx == NULL) {
		ret = -FI_EINVAL;
		goto fail;
	}
	if (tx->tx_qp == NULL) {
		ret = usdf_tx_rdm_enable(tx);
		if (ret != 0) {
			goto fail;
		}
	}

	/* Must have RX context at this point */
	rx = ep->ep_rx;
	if (rx == NULL) {
		ret = -FI_EINVAL;
		goto fail;
	}
	if (rx->rx_qp == NULL) {
		ret = usdf_rx_rdm_enable(rx);
		if (ret != 0) {
			goto fail;
		}
	}

	return 0;
fail:
	return ret;
}

static int
usdf_ep_rdm_enable(struct fid_ep *fep)
{
	return usdf_ep_rdm_get_queues(ep_ftou(fep));
}

static int
usdf_ep_rdm_getopt(fid_t fid, int level, int optname,
		  void *optval, size_t *optlen)
{
	struct usdf_ep *ep;
	ep = ep_fidtou(fid);
	(void)ep;

	switch (level) {
	case FI_OPT_ENDPOINT:
		return -FI_ENOPROTOOPT;
	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

static int
usdf_ep_rdm_setopt(fid_t fid, int level, int optname,
		  const void *optval, size_t optlen)
{
	struct usdf_ep *ep;
	ep = ep_fidtou(fid);
	(void)ep;

	switch (level) {
	case FI_OPT_ENDPOINT:
		return -FI_ENOPROTOOPT;
	default:
		return -FI_ENOPROTOOPT;
	}
	return 0;
}

static ssize_t
usdf_ep_rdm_cancel(fid_t fid, void *context)
{
	return 0;
}

int
usdf_rdm_fill_tx_attr(struct fi_tx_attr *txattr)
{
	if (txattr->size > USDF_RDM_MAX_CTX_SIZE ||
	    txattr->iov_limit > USDF_RDM_MAX_SGE) {
		return -FI_ENODATA;
	}

	if (txattr->size == 0) {
		txattr->size = USDF_RDM_DFLT_CTX_SIZE;
	}
	if (txattr->iov_limit == 0) {
		txattr->iov_limit = USDF_RDM_DFLT_SGE;
	}
	return 0;
}

int
usdf_rdm_fill_rx_attr(struct fi_rx_attr *rxattr)
{
	if (rxattr->size > USDF_RDM_MAX_CTX_SIZE ||
	    rxattr->iov_limit > USDF_RDM_MAX_SGE) {
		return -FI_ENODATA;
	}

	if (rxattr->size == 0) {
		rxattr->size = USDF_RDM_DFLT_CTX_SIZE;
	}
	if (rxattr->iov_limit == 0) {
		rxattr->iov_limit = USDF_RDM_DFLT_SGE;
	}
	return 0;
}

/*
 * Find a hard CQ within this soft CQ that services message EPs
 */
static struct usdf_cq_hard *
usdf_ep_rdm_find_cqh(struct usdf_cq *cq)
{
	struct usdf_cq_hard *hcq;

	TAILQ_FOREACH(hcq, &cq->c.soft.cq_list, cqh_link) {
		if (hcq->cqh_progress == usdf_rdm_hcq_progress) {
			return hcq;
		}
	}
	return NULL;
}

static int
usdf_ep_rdm_bind_cq(struct usdf_ep *ep, struct usdf_cq *cq, uint64_t flags)
{
	struct usdf_cq_hard **hcqp;
	struct usdf_cq_hard *hcq;
	int ret;

	/*
	 * The CQ is actually bound the RX or TX ctx, not the EP directly
	 */
	if (flags & FI_SEND) {
		/* if TX is shared, but bind directly */
		if (ep->ep_tx->tx_fid.fid.fclass == FI_CLASS_STX_CTX) {
			return -FI_EINVAL;
		}
		hcqp = &ep->ep_tx->t.rdm.tx_hcq;
	} else {
		/* if RX is shared, but bind directly */
		if (ep->ep_rx->rx_fid.fid.fclass == FI_CLASS_SRX_CTX) {
			return -FI_EINVAL;
		}
		hcqp = &ep->ep_rx->r.rdm.rx_hcq;
	}
	if (*hcqp != NULL) {
		return -FI_EINVAL;
	}

	/* Make sure this CQ is "soft" */
	ret = usdf_cq_make_soft(cq);
	if (ret != 0) {
		return ret;
	}

	/* Use existing rdm CQ if present */
	hcq = usdf_ep_rdm_find_cqh(cq);
	if (hcq == NULL) {
		hcq = malloc(sizeof(*hcq));
		if (hcq == NULL) {
			return -errno;
		}
		ret = usd_create_cq(cq->cq_domain->dom_dev, 8195, /* XXX */
				-1, &hcq->cqh_ucq);
		if (ret != 0) {
			goto fail;
		}
		hcq->cqh_cq = cq;
		atomic_init(&hcq->cqh_refcnt, 0);
		hcq->cqh_progress = usdf_rdm_hcq_progress;
		switch (cq->cq_attr.format) {
		default:
		case FI_CQ_FORMAT_CONTEXT:
			hcq->cqh_post = usdf_cq_post_soft_context;
			break;
		case FI_CQ_FORMAT_MSG:
			hcq->cqh_post = usdf_cq_post_soft_msg;
			break;
		case FI_CQ_FORMAT_DATA:
			hcq->cqh_post = usdf_cq_post_soft_data;
			break;
		}
		TAILQ_INSERT_TAIL(&cq->c.soft.cq_list, hcq, cqh_link);

		/* add to domain progression list */
		TAILQ_INSERT_TAIL(&ep->ep_domain->dom_hcq_list,
				hcq, cqh_dom_link);
	}
	atomic_inc(&hcq->cqh_refcnt);
	atomic_inc(&cq->cq_refcnt);
	*hcqp = hcq;
	return 0;

fail:
	if (hcq != NULL) {
		free(hcq);
	}
	return ret;
}

static int
usdf_ep_rdm_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct usdf_ep *ep;
	struct usdf_cq *cq;

	ep = ep_fidtou(fid);

	switch (bfid->fclass) {

	case FI_CLASS_AV:
		if (ep->e.rdm.ep_av != NULL) {
			return -FI_EINVAL;
		}
		ep->e.rdm.ep_av = av_fidtou(bfid);
		break;


	case FI_CLASS_CQ:
		if (flags & FI_SEND) {
			cq = cq_fidtou(bfid);
			usdf_ep_rdm_bind_cq(ep, cq, FI_SEND);
		}

		if (flags & FI_RECV) {
			cq = cq_fidtou(bfid);
			usdf_ep_rdm_bind_cq(ep, cq, FI_RECV);
		}
		break;

	case FI_CLASS_EQ:
		if (ep->ep_eq != NULL) {
			return -FI_EINVAL;
		}
		ep->ep_eq = eq_fidtou(bfid);
		atomic_inc(&ep->ep_eq->eq_refcnt);
		break;
	default:
		return -FI_EINVAL;
	}

	return 0;
}

/*
 * XXX clean up pending transmits
 */
static int
usdf_rdm_rx_ctx_close(fid_t fid)
{
	struct usdf_rx *rx;
	struct usdf_cq_hard *hcq;

	rx = rx_fidtou(fid);

	if (atomic_get(&rx->rx_refcnt) > 0) {
		return -FI_EBUSY;
	}

	hcq = rx->r.rdm.rx_hcq;
	if (hcq != NULL) {
		atomic_dec(&hcq->cqh_refcnt);
		atomic_dec(&hcq->cqh_cq->cq_refcnt);
	}
	if (rx->r.rdm.rx_sock != -1) {
		close(rx->r.rdm.rx_sock);
	}

	if (rx->rx_qp != NULL) {
		usd_free_mr(rx->r.rdm.rx_bufs);
		free(rx->r.rdm.rx_rqe_buf);
		usd_destroy_qp(rx->rx_qp);
	}
	atomic_dec(&rx->rx_domain->dom_refcnt);

	free(rx);

	return 0;
}

/*
 * XXX clean up pending receives
 */
static int
usdf_rdm_tx_ctx_close(fid_t fid)
{
	struct usdf_tx *tx;
	struct usdf_cq_hard *hcq;

	tx = tx_fidtou(fid);

	if (atomic_get(&tx->tx_refcnt) > 0) {
		return -FI_EBUSY;
	}

	hcq = tx->t.rdm.tx_hcq;
	if (hcq != NULL) {
		atomic_dec(&hcq->cqh_refcnt);
		atomic_dec(&hcq->cqh_cq->cq_refcnt);
	}

	if (tx->tx_qp != NULL) {
		free(tx->t.rdm.tx_wqe_buf);
		usd_destroy_qp(tx->tx_qp);
	}
	atomic_dec(&tx->tx_domain->dom_refcnt);

	free(tx);

	return 0;
}

static int
usdf_rx_rdm_port_bind(struct usdf_rx *rx, struct fi_info *info)
{
	struct sockaddr_in *sin;
	struct sockaddr_in src;
	socklen_t addrlen;
	int ret;

	if (info->src_addr != NULL) {
		if (info->addr_format != FI_SOCKADDR &&
		    info->addr_format != FI_SOCKADDR_IN) {
			return -FI_EINVAL;
		}
		sin = (struct sockaddr_in *)info->src_addr;
	} else {
		memset(&src, 0, sizeof(src));
		sin = &src;
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr =
			rx->rx_domain->dom_fabric->fab_dev_attrs->uda_ipaddr_be;
	}
		
	rx->r.rdm.rx_sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (rx->r.rdm.rx_sock == -1) {
		return -errno;
	}
	ret = bind(rx->r.rdm.rx_sock, (struct sockaddr *)sin, sizeof(*sin));
	if (ret == -1) {
		return -errno;
	}

	addrlen = sizeof(*sin);
	ret = getsockname(rx->r.rdm.rx_sock, (struct sockaddr *)sin, &addrlen);
	if (ret == -1) {
		 return -errno;
	}

	return 0;
}

static int
usdf_ep_rdm_close(fid_t fid)
{
	struct usdf_ep *ep;

	ep = ep_fidtou(fid);

	if (atomic_get(&ep->ep_refcnt) > 0) {
		return -FI_EBUSY;
	}

	if (ep->ep_rx != NULL) {
		atomic_dec(&ep->ep_rx->rx_refcnt);
		if (rx_utofid(ep->ep_rx)->fclass  == FI_CLASS_RX_CTX) {
			(void) usdf_rdm_rx_ctx_close(rx_utofid(ep->ep_rx));
		}
	}

	if (ep->ep_tx != NULL) {
		atomic_dec(&ep->ep_tx->tx_refcnt);
		if (tx_utofid(ep->ep_tx)->fclass  == FI_CLASS_TX_CTX) {
			(void) usdf_rdm_tx_ctx_close(tx_utofid(ep->ep_tx));
		}
	}

	atomic_dec(&ep->ep_domain->dom_refcnt);
	if (ep->ep_eq != NULL) {
		atomic_dec(&ep->ep_eq->eq_refcnt);
	}
	
	free(ep);
	return 0;
}

static struct fi_ops_ep usdf_base_rdm_ops = {
	.size = sizeof(struct fi_ops_ep),
	.cancel = usdf_ep_rdm_cancel,
	.getopt = usdf_ep_rdm_getopt,
	.setopt = usdf_ep_rdm_setopt,
	.tx_ctx = fi_no_tx_ctx,
	.rx_ctx = fi_no_rx_ctx,
	.rx_size_left = fi_no_rx_size_left,
	.tx_size_left = fi_no_tx_size_left,
};

static struct fi_ops_cm usdf_cm_rdm_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = usdf_cm_rdm_getname,
	.getpeer = fi_no_getpeer,
	.connect = fi_no_connect,
	.listen = fi_no_listen,
	.accept = fi_no_accept,
	.reject = fi_no_reject,
	.shutdown = fi_no_shutdown,
};

static struct fi_ops_msg usdf_rdm_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = usdf_rdm_recv,
	.recvv = usdf_rdm_recvv,
	.recvmsg = usdf_rdm_recvmsg,
	.send = usdf_rdm_send,
	.sendv = usdf_rdm_sendv,
	.sendmsg = usdf_rdm_sendmsg,
	.inject = usdf_rdm_inject,
	.senddata = usdf_rdm_senddata,
	.injectdata = fi_no_msg_injectdata,
};

static int usdf_ep_rdm_control(struct fid *fid, int command, void *arg)
{
	struct fid_ep *ep;

	switch (fid->fclass) {
	case FI_CLASS_EP:
		ep = container_of(fid, struct fid_ep, fid);
		switch (command) {
		case FI_ENABLE:
			return usdf_ep_rdm_enable(ep);
			break;
		default:
			return -FI_ENOSYS;
		}
		break;
	default:
		return -FI_ENOSYS;
	}
}

static struct fi_ops usdf_ep_rdm_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_ep_rdm_close,
	.bind = usdf_ep_rdm_bind,
	.control = usdf_ep_rdm_control,
	.ops_open = fi_no_ops_open
};

int
usdf_ep_rdm_open(struct fid_domain *domain, struct fi_info *info,
	    struct fid_ep **ep_o, void *context)
{
	struct usdf_domain *udp;
	struct usdf_tx *tx;
	struct usdf_rx *rx;
	struct usdf_ep *ep;
	int ret;

	ep = NULL;
	rx = NULL;
	tx = NULL;
	if ((info->caps & ~USDF_RDM_CAPS) != 0) {
		return -FI_EBADFLAGS;
	}

	udp = dom_ftou(domain);

	/* allocate peer table if not done */
	if (udp->dom_peer_tab == NULL) {
		udp->dom_peer_tab = calloc(USDF_MAX_PEERS, sizeof(ep));
	}
	if (udp->dom_peer_tab == NULL) {
		ret = -errno;
		goto fail;
	}

	ep = calloc(1, sizeof(*ep));
	if (ep == NULL) {
		ret = -errno;
		goto fail;
	}

	ep->ep_fid.fid.fclass = FI_CLASS_EP;
	ep->ep_fid.fid.context = context;
	ep->ep_fid.fid.ops = &usdf_ep_rdm_ops;
	ep->ep_fid.ops = &usdf_base_rdm_ops;
	ep->ep_fid.cm = &usdf_cm_rdm_ops;
	ep->ep_fid.msg = &usdf_rdm_ops;
	ep->ep_domain = udp;
	ep->ep_caps = info->caps;
	ep->ep_mode = info->mode;

	/* implicitly create TX context if not to be shared */
	if (info->ep_attr == NULL ||
	    info->ep_attr->tx_ctx_cnt != FI_SHARED_CONTEXT) {
		tx = calloc(1, sizeof(*tx));
		if (tx == NULL) {
			ret = -errno;
			goto fail;
		}
		tx->tx_fid.fid.fclass = FI_CLASS_TX_CTX;
		atomic_init(&tx->tx_refcnt, 0);
		tx->tx_domain = udp;
		tx->tx_progress = usdf_rdm_tx_progress;
		atomic_init(&tx->t.rdm.tx_next_msg_id, 1);
		atomic_inc(&udp->dom_refcnt);

		if (info->tx_attr != NULL) {
			ret = usdf_rdm_fill_tx_attr(info->tx_attr);
			if (ret != 0) {
				goto fail;
			}
			tx->tx_attr = *info->tx_attr;
		} else {
			ret = usdf_rdm_fill_tx_attr(&tx->tx_attr);
			if (ret != 0) {
				goto fail;
			}
		}
		TAILQ_INIT(&tx->t.rdm.tx_free_wqe);
		TAILQ_INIT(&tx->t.rdm.tx_rdc_ready);
		TAILQ_INIT(&tx->t.rdm.tx_rdc_have_acks);

		ep->ep_tx = tx;
		atomic_inc(&tx->tx_refcnt);
	}

	/* implicitly create RX context if not to be shared */
	if (info->ep_attr == NULL ||
	    info->ep_attr->rx_ctx_cnt != FI_SHARED_CONTEXT) {
		rx = calloc(1, sizeof(*rx));
		if (rx == NULL) {
			ret = -errno;
			goto fail;
		}

		rx->rx_fid.fid.fclass = FI_CLASS_RX_CTX;
		atomic_init(&rx->rx_refcnt, 0);
		rx->rx_domain = udp;
		rx->r.rdm.rx_tx = tx;
		rx->r.rdm.rx_sock = -1;
		atomic_inc(&udp->dom_refcnt);

		ret = usdf_rx_rdm_port_bind(rx, info);
		if (ret != 0) {
			goto fail;
		}

		if (info->rx_attr != NULL) {
			ret = usdf_rdm_fill_rx_attr(info->rx_attr);
			if (ret != 0) {
				goto fail;
			}
			rx->rx_attr = *info->rx_attr;
		} else {
			ret = usdf_rdm_fill_rx_attr(&rx->rx_attr);
			if (ret != 0) {
				goto fail;
			}
		}
		TAILQ_INIT(&rx->r.rdm.rx_free_rqe);
		TAILQ_INIT(&rx->r.rdm.rx_posted_rqe);

		ep->ep_rx = rx;
		atomic_inc(&rx->rx_refcnt);
	}

	atomic_init(&ep->ep_refcnt, 0);
	atomic_inc(&udp->dom_refcnt);

	*ep_o = ep_utof(ep);
	return 0;
fail:
	if (rx != NULL) {
		if (rx->r.rdm.rx_sock != -1) {
			close(rx->r.rdm.rx_sock);
		}
		free(rx);
		atomic_dec(&udp->dom_refcnt);
	}
	if (tx != NULL) {
		free(tx);
		atomic_dec(&udp->dom_refcnt);
	}
	if (ep != NULL) {
		free(ep);
	}
	return ret;
}
