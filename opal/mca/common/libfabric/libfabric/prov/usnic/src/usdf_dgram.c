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

#include "usnic_direct.h"
#include "usd.h"
#include "usd_post.h"
#include "usdf.h"

ssize_t
usdf_dgram_recv(struct fid_ep *fep, void *buf, size_t len,
		void *desc, void *context)
{
	struct usdf_ep *ep;
	struct usd_qp_impl *qp;
	struct usd_recv_desc rxd;
	uint32_t index;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->ep_qp);

	index = qp->uq_rq.urq_post_index;
	rxd.urd_context = context;
	rxd.urd_iov[0].iov_base = (uint8_t *)ep->ep_hdr_buf +
		(index * USDF_HDR_BUF_ENTRY) +
		(USDF_HDR_BUF_ENTRY - sizeof(struct usd_udp_hdr));
	rxd.urd_iov[0].iov_len = sizeof(struct usd_udp_hdr);
	rxd.urd_iov[1].iov_base = buf;
	rxd.urd_iov[1].iov_len = len;
	rxd.urd_iov_cnt = 2;
	rxd.urd_next = NULL;

	ep->ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;
	index = (index + 1) & qp->uq_rq.urq_post_index_mask;
	ep->ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;

	return usd_post_recv(ep->ep_qp, &rxd);
}

ssize_t
usdf_dgram_recvv(struct fid_ep *fep, const struct iovec *iov, void **desc,
                 size_t count, void *context)
{
	struct usdf_ep *ep;
	struct usd_recv_desc rxd;
	struct usd_qp_impl *qp;
	uint32_t index;
	int i;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->ep_qp);

	rxd.urd_context = context;
	rxd.urd_iov[0].iov_base = ep->ep_hdr_buf + 
		qp->uq_rq.urq_post_index * USDF_HDR_BUF_ENTRY;
	rxd.urd_iov[0].iov_len = sizeof(struct usd_udp_hdr);
	memcpy(&rxd.urd_iov[1], iov, sizeof(*iov) * count);
	rxd.urd_iov_cnt = count + 1;
	rxd.urd_next = NULL;

	index = qp->uq_rq.urq_post_index;
	for (i = 0; i < count; ++i) {
		ep->ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;
		index = (index + 1) & qp->uq_rq.urq_post_index_mask;
	}

	return usd_post_recv(ep->ep_qp, &rxd);
}

ssize_t
usdf_dgram_recvfrom(struct fid_ep *fep, void *buf, size_t len, void *desc,
		    fi_addr_t src_addr, void *context)
{
	return -FI_ENOSYS;
}

static inline ssize_t
_usdf_dgram_sendto(struct usdf_ep *ep, struct usd_dest *dest, 
		const void *buf, size_t len,  void *context)
{
	if (len <= USD_SEND_MAX_COPY - sizeof(struct usd_udp_hdr)) {
		return usd_post_send_one_copy(ep->ep_qp, dest, buf, len,
			USD_SF_SIGNAL, context);
	} else {
		return usd_post_send_one(ep->ep_qp, dest, buf, len,
			USD_SF_SIGNAL, context);
	}
}

ssize_t
usdf_dgram_sendto(struct fid_ep *fep, const void *buf, size_t len, void *desc,
		  fi_addr_t dest_addr, void *context)
{
	struct usdf_ep *ep;
	struct usd_dest *dest;
	int ret;

	ep = ep_ftou(fep);

	dest = (struct usd_dest *)(uintptr_t)dest_addr;
	return _usdf_dgram_sendto(ep, dest, buf, len, context);

	return ret;
}

ssize_t
usdf_dgram_send(struct fid_ep *fep, const void *buf, size_t len,
		void *desc, void *context)
{
	struct usdf_ep *ep;
	struct usd_dest *dest;
	int ret;

	ep = ep_ftou(fep);
	dest = ep->ep_dest;
	if (dest == NULL) {
		return -FI_ENOTCONN;
	}

	return _usdf_dgram_sendto(ep, dest, buf, len, context);

	return ret;
}

ssize_t
usdf_dgram_senddata(struct fid_ep *ep, const void *buf, size_t len,
		    void *desc, uint64_t data, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_dgram_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
                 size_t count, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_dgram_sendmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_dgram_inject(struct fid_ep *ep, const void *buf, size_t len)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_dgram_injectto(struct fid_ep *ep, const void *buf, size_t len,
		    fi_addr_t dest_addr)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_dgram_senddatato(struct fid_ep *ep, const void *buf, size_t len,
		      void *desc, uint64_t data, fi_addr_t dest_addr,
		      void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_dgram_recvmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return -FI_ENOSYS;
}

/*
 * Versions that rely on user to reserve space for header at start of buffer
 */
ssize_t
usdf_dgram_prefix_recv(struct fid_ep *fep, void *buf, size_t len,
		void *desc, void *context)
{
	struct usdf_ep *ep;
	struct usd_qp_impl *qp;
	struct usd_recv_desc rxd;
	uint32_t index;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->ep_qp);

	index = qp->uq_rq.urq_post_index;
	rxd.urd_context = context;
	rxd.urd_iov[0].iov_base = (uint8_t *)buf +
		USDF_HDR_BUF_ENTRY - sizeof(struct usd_udp_hdr);
	rxd.urd_iov[0].iov_len = len;
	rxd.urd_iov_cnt = 1;
	rxd.urd_next = NULL;

	ep->ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;

	return usd_post_recv(ep->ep_qp, &rxd);
}

ssize_t
usdf_dgram_prefix_recvv(struct fid_ep *fep, const struct iovec *iov,
		void **desc, size_t count, void *context)
{
	struct usdf_ep *ep;
	struct usd_recv_desc rxd;
	struct usd_qp_impl *qp;
	uint32_t index;
	int i;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->ep_qp);

	rxd.urd_context = context;
	memcpy(&rxd.urd_iov[0], iov, sizeof(*iov) * count);
	rxd.urd_iov[0].iov_base = (uint8_t *)rxd.urd_iov[0].iov_base +
		USDF_HDR_BUF_ENTRY - sizeof(struct usd_udp_hdr);

	rxd.urd_iov_cnt = count;
	rxd.urd_next = NULL;

	index = qp->uq_rq.urq_post_index;
	for (i = 0; i < count; ++i) {
		ep->ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;
		index = (index + 1) & qp->uq_rq.urq_post_index_mask;
	}

	return usd_post_recv(ep->ep_qp, &rxd);
}

ssize_t
usdf_dgram_prefix_sendto(struct fid_ep *fep, const void *buf, size_t len,
        void *desc, fi_addr_t dest_addr, void *context)
{
    struct usdf_ep *ep;
    struct usd_dest *dest;
    struct usd_qp_impl *qp;
    struct usd_udp_hdr *hdr;
    struct usd_wq *wq;
    uint32_t last_post;
    struct usd_wq_post_info *info;

    ep = ep_ftou(fep);
    dest = (struct usd_dest *)(uintptr_t)dest_addr;

    qp = to_qpi(ep->ep_qp);
    wq = &qp->uq_wq;

    hdr = (struct usd_udp_hdr *) buf - 1;
    memcpy(hdr, &dest->ds_dest.ds_udp.u_hdr, sizeof(*hdr));

    /* adjust lengths and insert source port */
    hdr->uh_ip.tot_len = htons(len + sizeof(struct usd_udp_hdr) -
        sizeof(struct ether_header));
    hdr->uh_udp.len = htons((sizeof(struct usd_udp_hdr) -
        sizeof(struct ether_header) -
        sizeof(struct iphdr)) + len);
    hdr->uh_udp.source =
        qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

    last_post = _usd_post_send_one(wq, hdr,
            len + sizeof(struct usd_udp_hdr), 1);

    info = &wq->uwq_post_info[last_post];
    info->wp_context = context;
    info->wp_len = len;

    return 0;
}
