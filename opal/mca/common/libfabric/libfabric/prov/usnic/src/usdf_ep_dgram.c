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

#include "usnic_direct.h"
#include "usd.h"
#include "usdf.h"
#include "usdf_endpoint.h"
#include "usdf_dgram.h"
#include "usdf_av.h"
#include "usdf_cq.h"

static int
usdf_ep_dgram_enable(struct fid_ep *fep)
{
	struct usdf_ep *ep;
	struct usd_filter filt;
	struct usd_qp_impl *uqp;
	int ret;

	ep = ep_ftou(fep);

	if (ep->e.dg.ep_wcq == NULL) {
		ret = -FI_EOPBADSTATE;
		goto fail;
	}
	if (ep->e.dg.ep_rcq == NULL) {
		ret = -FI_EOPBADSTATE;
		goto fail;
	}

	filt.uf_type = USD_FTY_UDP_SOCK;
	filt.uf_filter.uf_udp_sock.u_sock = ep->e.dg.ep_sock;

	if (ep->ep_caps & USDF_EP_CAP_PIO) {
		ret = usd_create_qp(ep->ep_domain->dom_dev,
				USD_QTR_UDP,
				USD_QTY_PIO,
				ep->e.dg.ep_wcq->c.hard.cq_cq,
				ep->e.dg.ep_rcq->c.hard.cq_cq,
				127,	// XXX
				127,	// XXX
				&filt,
				&ep->e.dg.ep_qp);
	} else {
		ret = -FI_EAGAIN;
	}

	if (ret != 0) {
		ret = usd_create_qp(ep->ep_domain->dom_dev,
				USD_QTR_UDP,
				USD_QTY_NORMAL,
				ep->e.dg.ep_wcq->c.hard.cq_cq,
				ep->e.dg.ep_rcq->c.hard.cq_cq,
				ep->ep_wqe,
				ep->ep_rqe,
				&filt,
				&ep->e.dg.ep_qp);
	}
	if (ret != 0) {
		goto fail;
	}
	ep->e.dg.ep_qp->uq_context = ep;

	/*
	 * Allocate a memory region big enough to hold a header for each
	 * RQ entry
	 */
	uqp = to_qpi(ep->e.dg.ep_qp);
	ep->e.dg.ep_hdr_ptr = calloc(uqp->uq_rq.urq_num_entries,
			sizeof(ep->e.dg.ep_hdr_ptr[0]));
	if (ep->e.dg.ep_hdr_ptr == NULL) {
		ret = -FI_ENOMEM;
		goto fail;
	}

	ret = usd_alloc_mr(ep->ep_domain->dom_dev,
		usd_get_recv_credits(ep->e.dg.ep_qp) * USDF_HDR_BUF_ENTRY,
			&ep->e.dg.ep_hdr_buf);
	if (ret != 0) {
		goto fail;
	}

	return 0;

fail:
	if (ep->e.dg.ep_hdr_ptr != NULL) {
		free(ep->e.dg.ep_hdr_ptr);
		ep->e.dg.ep_hdr_ptr = NULL;
	}
	if (ep->e.dg.ep_qp != NULL) {
		usd_destroy_qp(ep->e.dg.ep_qp);
		ep->e.dg.ep_qp = NULL;
	}
	return ret;
}

static int
usdf_ep_dgram_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
	struct usdf_ep *ep;
	struct usdf_cq *cq;
	int ret;

	ep = ep_fidtou(fid);

	switch (bfid->fclass) {

	case FI_CLASS_AV:
		if (ep->e.dg.ep_av != NULL) {
			return -FI_EINVAL;
		}
		ep->e.dg.ep_av = av_fidtou(bfid);
		break;

	case FI_CLASS_CQ:
		cq = cq_fidtou(bfid);

		/* actually, could look through CQ list for a hard
		 * CQ with function usd_poll_cq() and use that... XXX
		 */
		if (usdf_cq_is_soft(cq)) {
			return -FI_EINVAL;
		}
		if (cq->c.hard.cq_cq == NULL) {
			ret = usdf_cq_create_cq(cq);
			if (ret != 0) {
				return ret;
			}
		}

		if (flags & FI_SEND) {
			if (ep->e.dg.ep_wcq != NULL) {
				return -FI_EINVAL;
			}
			ep->e.dg.ep_wcq = cq;
			atomic_inc(&cq->cq_refcnt);
		}

		if (flags & FI_RECV) {
			if (ep->e.dg.ep_rcq != NULL) {
				return -FI_EINVAL;
			}
			ep->e.dg.ep_rcq = cq;
			atomic_inc(&cq->cq_refcnt);
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

static void
usdf_ep_dgram_deref_cq(struct usdf_cq *cq)
{
	struct usdf_cq_hard *hcq;
	void (*rtn)(struct usdf_cq_hard *hcq);

	if (cq == NULL) {
		return;
	}
	atomic_dec(&cq->cq_refcnt);

        switch (cq->cq_attr.format) {
        case FI_CQ_FORMAT_CONTEXT:
                rtn = usdf_progress_hard_cq_context;
                break;
        case FI_CQ_FORMAT_MSG:
                rtn = usdf_progress_hard_cq_msg;
                break;
        case FI_CQ_FORMAT_DATA:
                rtn = usdf_progress_hard_cq_data;
                break;
        default:
                return;
        }

	if (usdf_cq_is_soft(cq)) {
		TAILQ_FOREACH(hcq, &cq->c.soft.cq_list, cqh_link) {
			if (hcq->cqh_progress == rtn) {
				atomic_dec(&hcq->cqh_refcnt);
				return;
			}
		}
	}
}

static int
usdf_ep_dgram_close(fid_t fid)
{
	struct usdf_ep *ep;

	ep = ep_fidtou(fid);

	if (atomic_get(&ep->ep_refcnt) > 0) {
		return -FI_EBUSY;
	}

	if (ep->e.dg.ep_qp != NULL) {
		usd_destroy_qp(ep->e.dg.ep_qp);
	}
	atomic_dec(&ep->ep_domain->dom_refcnt);
	if (ep->ep_eq != NULL) {
		atomic_dec(&ep->ep_eq->eq_refcnt);
	}
	usdf_ep_dgram_deref_cq(ep->e.dg.ep_wcq);
	usdf_ep_dgram_deref_cq(ep->e.dg.ep_rcq);

	if (ep->e.dg.ep_sock != -1) {
		close(ep->e.dg.ep_sock);
	}

	free(ep);
	return 0;
}

static struct fi_ops_ep usdf_base_dgram_ops = {
	.size = sizeof(struct fi_ops_ep),
	.cancel = fi_no_cancel,
	.getopt = fi_no_getopt,
	.setopt = fi_no_setopt,
	.tx_ctx = fi_no_tx_ctx,
	.rx_ctx = fi_no_rx_ctx,
	.rx_size_left = usdf_dgram_rx_size_left,
	.tx_size_left = usdf_dgram_tx_size_left,
};

static struct fi_ops_ep usdf_base_dgram_prefix_ops = {
	.size = sizeof(struct fi_ops_ep),
	.cancel = fi_no_cancel,
	.getopt = fi_no_getopt,
	.setopt = fi_no_setopt,
	.tx_ctx = fi_no_tx_ctx,
	.rx_ctx = fi_no_rx_ctx,
	.rx_size_left = usdf_dgram_prefix_rx_size_left,
	.tx_size_left = usdf_dgram_prefix_tx_size_left,
};

static struct fi_ops_msg usdf_dgram_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = usdf_dgram_recv,
	.recvv = usdf_dgram_recvv,
	.recvmsg = usdf_dgram_recvmsg,
	.send = usdf_dgram_send,
	.sendv = usdf_dgram_sendv,
	.sendmsg = usdf_dgram_sendmsg,
	.inject = usdf_dgram_inject,
	.senddata = usdf_dgram_senddata,
	.injectdata = fi_no_msg_injectdata,
};

static struct fi_ops_msg usdf_dgram_prefix_ops = {
	.size = sizeof(struct fi_ops_msg),
	.recv = usdf_dgram_prefix_recv,
	.recvv = usdf_dgram_prefix_recvv,
	.recvmsg = usdf_dgram_prefix_recvmsg,
	.send = usdf_dgram_prefix_send,
	.sendv = usdf_dgram_prefix_sendv,
	.sendmsg = usdf_dgram_prefix_sendmsg,
	.inject = usdf_dgram_inject,
	.senddata = usdf_dgram_senddata,
	.injectdata = fi_no_msg_injectdata,
};

static struct fi_ops_cm usdf_cm_dgram_ops = {
	.size = sizeof(struct fi_ops_cm),
	.getname = fi_no_getname,
	.getpeer = fi_no_getpeer,
	.connect = fi_no_connect,
	.listen = fi_no_listen,
	.accept = fi_no_accept,
	.reject = fi_no_reject,
	.shutdown = fi_no_shutdown,
};

static int usdf_ep_dgram_control(struct fid *fid, int command, void *arg)
{
	struct fid_ep *ep;

	switch (fid->fclass) {
	case FI_CLASS_EP:
		ep = container_of(fid, struct fid_ep, fid);
		switch (command) {
		case FI_ENABLE:
			return usdf_ep_dgram_enable(ep);
			break;
		default:
			return -FI_ENOSYS;
		}
		break;
	default:
		return -FI_ENOSYS;
	}
}

static struct fi_ops usdf_ep_dgram_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_ep_dgram_close,
	.bind = usdf_ep_dgram_bind,
	.control = usdf_ep_dgram_control,
	.ops_open = fi_no_ops_open
};

int
usdf_ep_dgram_open(struct fid_domain *domain, struct fi_info *info,
	    struct fid_ep **ep_o, void *context)
{
	struct usdf_domain *udp;
	struct usdf_ep *ep;
	int ret;

	if ((info->caps & ~USDF_DGRAM_CAPS) != 0) {
		return -FI_EBADF;
	}

	udp = dom_ftou(domain);

	ep = calloc(1, sizeof(*ep));
	if (ep == NULL) {
		return -FI_ENOMEM;
	}

	ep->e.dg.ep_sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (ep->e.dg.ep_sock == -1) {
		ret = -errno;
		goto fail;
	}
	if (info->src_addr != NULL) {
		if (info->addr_format == FI_SOCKADDR ||
		    info->addr_format == FI_SOCKADDR_IN) {
			ret = usdf_ep_port_bind(ep, info);
			if (ret != 0) {
				goto fail;
			}
		}
	}

	ep->ep_fid.fid.fclass = FI_CLASS_EP;
	ep->ep_fid.fid.context = context;
	ep->ep_fid.fid.ops = &usdf_ep_dgram_ops;
	ep->ep_fid.cm = &usdf_cm_dgram_ops;
	ep->ep_domain = udp;
	ep->ep_caps = info->caps;
	ep->ep_mode = info->mode;
	if (info->tx_attr != NULL && info->tx_attr->size != 0) {
		ep->ep_wqe = info->tx_attr->size;
	} else {
		ep->ep_wqe =
			udp->dom_fabric->fab_dev_attrs->uda_max_send_credits;
	}
	if (info->rx_attr != NULL && info->rx_attr->size != 0) {
		ep->ep_rqe = info->rx_attr->size;
	} else {
		ep->ep_rqe =
			udp->dom_fabric->fab_dev_attrs->uda_max_recv_credits;
	}

	if (ep->ep_mode & FI_MSG_PREFIX) {
		if (info->ep_attr == NULL) {
			ret = -FI_EBADF;
			goto fail;
		}

		ep->ep_fid.ops = &usdf_base_dgram_prefix_ops;
		info->ep_attr->msg_prefix_size = USDF_HDR_BUF_ENTRY;
		ep->ep_fid.msg = &usdf_dgram_prefix_ops;
	} else {
		ep->ep_fid.ops = &usdf_base_dgram_ops;
		ep->ep_fid.msg = &usdf_dgram_ops;
	}
	atomic_init(&ep->ep_refcnt, 0);
	atomic_inc(&udp->dom_refcnt);

	*ep_o = ep_utof(ep);
	return 0;

fail:
	if (ep != NULL) {
		if (ep->e.dg.ep_sock != -1) {
			close(ep->e.dg.ep_sock);
		}
		free(ep);
	}
	return ret;
}
