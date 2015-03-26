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
# include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>

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
#include "usdf.h"
#include "usdf_rdm.h"
#include "usdf_timer.h"

static int
usdf_domain_bind(struct fid *fid, struct fid *bfid, uint64_t flags)
{
        struct usdf_domain *udp;

        udp = dom_fidtou(fid);

        switch (bfid->fclass) {
        case FI_CLASS_EQ:
                if (udp->dom_eq != NULL) {
                        return -FI_EINVAL;
                }
                udp->dom_eq = eq_fidtou(bfid);
                atomic_inc(&udp->dom_eq->eq_refcnt);
                break;
        default:
                return -FI_EINVAL;
        }

        return 0;
}

static void
usdf_dom_rdc_free_data(struct usdf_domain *udp)
{
	struct usdf_rdm_connection *rdc;
	int i;

	if (udp->dom_rdc_hashtab != NULL) {

		pthread_spin_lock(&udp->dom_progress_lock);
		for (i = 0; i < USDF_RDM_HASH_SIZE; ++i) {
			rdc = udp->dom_rdc_hashtab[i];
			while (rdc != NULL) {
				usdf_timer_reset(udp->dom_fabric,
						rdc->dc_timer, 0);
				rdc = rdc->dc_hash_next;
			}
		}
		pthread_spin_unlock(&udp->dom_progress_lock);

		/* XXX probably want a timeout here... */
		while (atomic_get(&udp->dom_rdc_free_cnt) < 
		       udp->dom_rdc_total) {
			pthread_yield();
		}

		free(udp->dom_rdc_hashtab);
		udp->dom_rdc_hashtab = NULL;
	}

	while (!SLIST_EMPTY(&udp->dom_rdc_free)) {
		rdc = SLIST_FIRST(&udp->dom_rdc_free);
		SLIST_REMOVE_HEAD(&udp->dom_rdc_free, dc_addr_link);
		usdf_timer_free(udp->dom_fabric, rdc->dc_timer);
		free(rdc);
	}
}

static int
usdf_dom_rdc_alloc_data(struct usdf_domain *udp)
{
	struct usdf_rdm_connection *rdc;
	int ret;
	int i;

	udp->dom_rdc_hashtab = calloc(USDF_RDM_HASH_SIZE,
			sizeof(*udp->dom_rdc_hashtab));
	if (udp->dom_rdc_hashtab == NULL) {
		return -FI_ENOMEM;
	}
	SLIST_INIT(&udp->dom_rdc_free);
	atomic_init(&udp->dom_rdc_free_cnt, 0);
	for (i = 0; i < USDF_RDM_FREE_BLOCK; ++i) {
		rdc = calloc(1, sizeof(*rdc));
		if (rdc == NULL) {
			return -FI_ENOMEM;
		}
		ret = usdf_timer_alloc(usdf_rdm_rdc_timeout, rdc,
				&rdc->dc_timer);
		if (ret != 0) {
			free(rdc);
			return ret;
		}
		rdc->dc_flags = USDF_DCS_UNCONNECTED | USDF_DCF_NEW_RX;
		rdc->dc_next_rx_seq = 0;
		rdc->dc_next_tx_seq = 0;
		rdc->dc_last_rx_ack = rdc->dc_next_tx_seq - 1;
		TAILQ_INIT(&rdc->dc_wqe_posted);
		TAILQ_INIT(&rdc->dc_wqe_sent);
		SLIST_INSERT_HEAD(&udp->dom_rdc_free, rdc, dc_addr_link);
		atomic_inc(&udp->dom_rdc_free_cnt);
	}
	udp->dom_rdc_total = USDF_RDM_FREE_BLOCK;
	return 0;
}

static int
usdf_domain_close(fid_t fid)
{
	struct usdf_domain *udp;
	int ret;

	udp = container_of(fid, struct usdf_domain, dom_fid.fid);
	if (atomic_get(&udp->dom_refcnt) > 0) {
		return -FI_EBUSY;
	}

	if (udp->dom_dev != NULL) {
		ret = usd_close(udp->dom_dev);
		if (ret != 0) {
			return ret;
		}
	}
	usdf_dom_rdc_free_data(udp);

	if (udp->dom_eq != NULL) {
		atomic_dec(&udp->dom_eq->eq_refcnt);
	}
	atomic_dec(&udp->dom_fabric->fab_refcnt);
	LIST_REMOVE(udp, dom_link);
	fi_freeinfo(udp->dom_info);
	free(udp);

	return 0;
}

static struct fi_ops usdf_fid_ops = {
	.size = sizeof(struct fi_ops),
	.close = usdf_domain_close,
	.bind = usdf_domain_bind,
	.control = fi_no_control,
	.ops_open = fi_no_ops_open,
};

static struct fi_ops_mr usdf_domain_mr_ops = {
	.size = sizeof(struct fi_ops_mr),
	.reg = usdf_reg_mr,
	.regv = fi_no_mr_regv,
	.regattr = fi_no_mr_regattr,
};

static struct fi_ops_domain usdf_domain_ops = {
	.size = sizeof(struct fi_ops_domain),
	.av_open = usdf_av_open,
	.cq_open = usdf_cq_open,
	.endpoint = usdf_endpoint_open,
	.scalable_ep = fi_no_scalable_ep,
	.cntr_open = fi_no_cntr_open,
	.poll_open = fi_no_poll_open,
	.stx_ctx = fi_no_stx_context,
	.srx_ctx = fi_no_srx_context,
};

int
usdf_domain_open(struct fid_fabric *fabric, struct fi_info *info,
	   struct fid_domain **domain, void *context)
{
	struct usdf_fabric *fp;
	struct usdf_domain *udp;
	struct sockaddr_in *sin;
	size_t addrlen;
	int ret;

	udp = calloc(1, sizeof *udp);
	if (udp == NULL) {
		USDF_DEBUG("unable to alloc mem for domain\n");
		ret = -FI_ENOMEM;
		goto fail;
	}

	fp = fab_fidtou(fabric);

	USDF_DEBUG("uda_devname=%s\n", fp->fab_dev_attrs->uda_devname);

	/*
	 * Make sure address format is good and matches this fabric
	 */
	switch (info->addr_format) {
	case FI_SOCKADDR:
		addrlen = sizeof(struct sockaddr);
		break;
	case FI_SOCKADDR_IN:
		addrlen = sizeof(struct sockaddr_in);
		break;
	default:
		ret = -FI_EINVAL;
		goto fail;
	}
	sin = info->src_addr;
	if (info->src_addrlen != addrlen || sin->sin_family != AF_INET ||
	    sin->sin_addr.s_addr != fp->fab_dev_attrs->uda_ipaddr_be) {
		ret = -FI_EINVAL;
		goto fail;
	}

	ret = usd_open(fp->fab_dev_attrs->uda_devname, &udp->dom_dev);
	if (ret != 0) {
		goto fail;
	}

	udp->dom_fid.fid.fclass = FI_CLASS_DOMAIN;
	udp->dom_fid.fid.context = context;
	udp->dom_fid.fid.ops = &usdf_fid_ops;
	udp->dom_fid.ops = &usdf_domain_ops;
	udp->dom_fid.mr = &usdf_domain_mr_ops;

	ret = pthread_spin_init(&udp->dom_progress_lock,
			PTHREAD_PROCESS_PRIVATE);
	if (ret != 0) {
		ret = -ret;
		goto fail;
	}
	TAILQ_INIT(&udp->dom_tx_ready);
	TAILQ_INIT(&udp->dom_hcq_list);

	udp->dom_info = fi_dupinfo(info);
	if (udp->dom_info == NULL) {
		ret = -FI_ENOMEM;
		goto fail;
	}
	if (udp->dom_info->dest_addr != NULL) {
		free(udp->dom_info->dest_addr);
		udp->dom_info->dest_addr = NULL;
	}

	ret = usdf_dom_rdc_alloc_data(udp);
	if (ret != 0) {
		goto fail;
	}

	udp->dom_fabric = fp;
	LIST_INSERT_HEAD(&fp->fab_domain_list, udp, dom_link);
	atomic_init(&udp->dom_refcnt, 0);
	atomic_inc(&fp->fab_refcnt);

	*domain = &udp->dom_fid;
	return 0;

fail:
	if (udp != NULL) {
		if (udp->dom_info != NULL) {
			fi_freeinfo(udp->dom_info);
		}
		if (udp->dom_dev != NULL) {
			usd_close(udp->dom_dev);
		}
		usdf_dom_rdc_free_data(udp);
		free(udp);
	}
	return ret;
}
