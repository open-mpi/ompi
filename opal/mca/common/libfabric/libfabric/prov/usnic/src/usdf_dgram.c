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

#include "usd.h"
#include "usd_post.h"

#include "usdf.h"
#include "usdf_dgram.h"
#include "usdf_av.h"

ssize_t
usdf_dgram_recv(struct fid_ep *fep, void *buf, size_t len,
		void *desc, fi_addr_t src_addr, void *context)
{
	struct usdf_ep *ep;
	struct usd_qp_impl *qp;
	struct usd_recv_desc rxd;
	uint32_t index;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->e.dg.ep_qp);

	index = qp->uq_rq.urq_post_index;
	rxd.urd_context = context;
	rxd.urd_iov[0].iov_base = (uint8_t *)ep->e.dg.ep_hdr_buf +
		(index * USDF_HDR_BUF_ENTRY) +
		(USDF_HDR_BUF_ENTRY - sizeof(struct usd_udp_hdr));
	rxd.urd_iov[0].iov_len = sizeof(struct usd_udp_hdr);
	rxd.urd_iov[1].iov_base = buf;
	rxd.urd_iov[1].iov_len = len;
	rxd.urd_iov_cnt = 2;
	rxd.urd_next = NULL;

	ep->e.dg.ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;
	index = (index + 1) & qp->uq_rq.urq_post_index_mask;
	ep->e.dg.ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;

	return usd_post_recv(ep->e.dg.ep_qp, &rxd);
}

ssize_t
usdf_dgram_recvv(struct fid_ep *fep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t src_addr, void *context)
{
	struct usdf_ep *ep;
	struct usd_recv_desc rxd;
	struct usd_qp_impl *qp;
	uint32_t index;
	int i;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->e.dg.ep_qp);

	rxd.urd_context = context;
	rxd.urd_iov[0].iov_base = ep->e.dg.ep_hdr_buf +
		qp->uq_rq.urq_post_index * USDF_HDR_BUF_ENTRY;
	rxd.urd_iov[0].iov_len = sizeof(struct usd_udp_hdr);
	memcpy(&rxd.urd_iov[1], iov, sizeof(*iov) * count);
	rxd.urd_iov_cnt = count + 1;
	rxd.urd_next = NULL;

	index = qp->uq_rq.urq_post_index;
	for (i = 0; i < count; ++i) {
		ep->e.dg.ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;
		index = (index + 1) & qp->uq_rq.urq_post_index_mask;
	}

	return usd_post_recv(ep->e.dg.ep_qp, &rxd);
}

ssize_t
usdf_dgram_recvmsg(struct fid_ep *fep, const struct fi_msg *msg, uint64_t flags)
{
	return usdf_dgram_recvv(fep, msg->msg_iov, msg->desc,
		msg->iov_count, (fi_addr_t)msg->addr, msg->context);
}

static inline ssize_t
_usdf_dgram_send(struct usdf_ep *ep, struct usdf_dest *dest,
		const void *buf, size_t len,  void *context)
{
	if (len <= USD_SEND_MAX_COPY - sizeof(struct usd_udp_hdr)) {
		return usd_post_send_one_copy(ep->e.dg.ep_qp,
			&dest->ds_dest, buf, len, USD_SF_SIGNAL, context);
	} else {
		return usd_post_send_one(ep->e.dg.ep_qp, &dest->ds_dest,
			buf, len, USD_SF_SIGNAL, context);
	}
}

ssize_t
usdf_dgram_send(struct fid_ep *fep, const void *buf, size_t len, void *desc,
		fi_addr_t dest_addr, void *context)
{
	struct usdf_ep *ep;
	struct usdf_dest *dest;

	ep = ep_ftou(fep);

	dest = (struct usdf_dest *)(uintptr_t) dest_addr;
	return _usdf_dgram_send(ep, dest, buf, len, context);
}

ssize_t
usdf_dgram_senddata(struct fid_ep *fep, const void *buf, size_t len,
			void *desc, uint64_t data, fi_addr_t dest_addr,
			void *context)
{
	return -FI_ENOSYS;
}

static ssize_t
_usdf_dgram_send_iov_copy(struct usdf_ep *ep, struct usd_dest *dest,
		const struct iovec *iov, size_t count, void *context)
{
	struct usd_wq *wq;
 	struct usd_qp_impl *qp;
	struct usd_udp_hdr *hdr;
	uint32_t last_post;
	struct usd_wq_post_info *info;
	uint8_t *copybuf;
	size_t len;
	unsigned i;

	qp = to_qpi(ep->e.dg.ep_qp);
	wq = &qp->uq_wq;
	copybuf = wq->uwq_copybuf +
			wq->uwq_post_index * USD_SEND_MAX_COPY;

	hdr = (struct usd_udp_hdr *)copybuf;
	memcpy(hdr, &dest->ds_dest.ds_udp.u_hdr, sizeof(*hdr));
	hdr->uh_udp.source =
		qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

	len = sizeof(*hdr);
	for (i = 0; i < count; i++) {
		memcpy(copybuf + len, iov[i].iov_base, iov[i].iov_len);
		len += iov[i].iov_len;
	}

	/* adjust lengths */
	hdr->uh_ip.tot_len = htons(len - sizeof(struct ether_header));
	hdr->uh_udp.len = htons(len - sizeof(struct ether_header) -
			sizeof(struct iphdr));

	last_post = _usd_post_send_one(wq, hdr, len, 1);

	info = &wq->uwq_post_info[last_post];
	info->wp_context = context;
	info->wp_len = len;

	return 0;
}

ssize_t
usdf_dgram_sendv(struct fid_ep *fep, const struct iovec *iov, void **desc,
		 size_t count, fi_addr_t dest_addr, void *context)
{
	struct usdf_ep *ep;
	struct usd_dest *dest;
	struct usd_wq *wq;
 	struct usd_qp_impl *qp;
	struct usd_udp_hdr *hdr;
	uint32_t last_post;
	struct usd_wq_post_info *info;
	uint8_t *copybuf;
	size_t len;
	struct iovec send_iov[USDF_DGRAM_MAX_SGE];
	int i;

	ep = ep_ftou(fep);
	dest = (struct usd_dest *)(uintptr_t) dest_addr;

	len = 0;
	for (i = 0; i < count; i++) {
		len += iov[i].iov_len;
	}

	if (len + sizeof(struct usd_udp_hdr) > USD_SEND_MAX_COPY) {
		qp = to_qpi(ep->e.dg.ep_qp);
		wq = &qp->uq_wq;
		copybuf = wq->uwq_copybuf +
				wq->uwq_post_index * USD_SEND_MAX_COPY;
		hdr = (struct usd_udp_hdr *)copybuf;
		memcpy(hdr, &dest->ds_dest.ds_udp.u_hdr, sizeof(*hdr));

		/* adjust lengths and insert source port */
		hdr->uh_ip.tot_len = htons(len + sizeof(struct usd_udp_hdr) -
			sizeof(struct ether_header));
		hdr->uh_udp.len = htons((sizeof(struct usd_udp_hdr) -
			sizeof(struct ether_header) -
			sizeof(struct iphdr)) + len);
		hdr->uh_udp.source =
			qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

		send_iov[0].iov_base = hdr;
		send_iov[0].iov_len = sizeof(*hdr);
		memcpy(&send_iov[1], iov, sizeof(struct iovec) * count);
		last_post = _usd_post_send_iov(wq, send_iov, count + 1, 1);
		info = &wq->uwq_post_info[last_post];
		info->wp_context = context;
		info->wp_len = len;
	} else {
		_usdf_dgram_send_iov_copy(ep, dest, iov, count, context);
	}
	return 0;
}

ssize_t
usdf_dgram_sendmsg(struct fid_ep *fep, const struct fi_msg *msg, uint64_t flags)
{
	return usdf_dgram_sendv(fep, msg->msg_iov, msg->desc, msg->iov_count,
				(fi_addr_t)msg->addr, msg->context);
}

ssize_t
usdf_dgram_inject(struct fid_ep *fep, const void *buf, size_t len,
		  fi_addr_t dest_addr)
{
	struct usdf_ep *ep;
	struct usdf_dest *dest;
	struct usd_wq *wq;
 	struct usd_qp_impl *qp;
	struct usd_udp_hdr *hdr;
	uint32_t last_post;
	struct usd_wq_post_info *info;
	uint8_t *copybuf;

	if (len + sizeof(struct usd_udp_hdr) > USD_SEND_MAX_COPY) {
		return -FI_ENOSPC;
	}

	ep = ep_ftou(fep);
	dest = (struct usdf_dest *)(uintptr_t)dest_addr;

	qp = to_qpi(ep->e.dg.ep_qp);
	wq = &qp->uq_wq;
	copybuf = wq->uwq_copybuf +
			wq->uwq_post_index * USD_SEND_MAX_COPY;

	hdr = (struct usd_udp_hdr *)copybuf;
	memcpy(hdr, &dest->ds_dest.ds_dest.ds_udp.u_hdr, sizeof(*hdr));
	hdr->uh_udp.source =
		qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;
	hdr->uh_ip.tot_len = htons(len + sizeof(*hdr)
				- sizeof(struct ether_header));
	hdr->uh_udp.len = htons(len + sizeof(*hdr) -
				sizeof(struct ether_header) -
				sizeof(struct iphdr));

	memcpy(hdr + 1, buf, len);

	last_post = _usd_post_send_one(wq, hdr, len + sizeof(*hdr), 1);

	info = &wq->uwq_post_info[last_post];
	info->wp_context = NULL;
	info->wp_len = len;

	return 0;
}

ssize_t usdf_dgram_rx_size_left(struct fid_ep *fep)
{
	struct usdf_ep *ep;

	if (fep == NULL)
		return -FI_EINVAL;

	ep = ep_ftou(fep);

	if (ep->e.dg.ep_qp == NULL)
		return -FI_EOPBADSTATE; /* EP not enabled */

	/* NOTE-SIZE-LEFT: divide by constant right now, rather than keeping
	 * track of the rx_attr->iov_limit value we gave to the user.  This
	 * sometimes under-reports the number of RX ops that could be posted,
	 * but it avoids touching a cache line that we don't otherwise need.
	 *
	 * sendv/recvv could potentially post iov_limit+1 descriptors
	 */
	return usd_get_recv_credits(ep->e.dg.ep_qp) / (USDF_DGRAM_DFLT_SGE + 1);
}

ssize_t usdf_dgram_tx_size_left(struct fid_ep *fep)
{
	struct usdf_ep *ep;

	if (fep == NULL)
		return -FI_EINVAL;

	ep = ep_ftou(fep);

	if (ep->e.dg.ep_qp == NULL)
		return -FI_EOPBADSTATE; /* EP not enabled */

	/* see NOTE-SIZE-LEFT */
	return usd_get_send_credits(ep->e.dg.ep_qp) / (USDF_DGRAM_DFLT_SGE + 1);
}

/*
 * Versions that rely on user to reserve space for header at start of buffer
 */
ssize_t
usdf_dgram_prefix_recv(struct fid_ep *fep, void *buf, size_t len,
		void *desc, fi_addr_t src_addr, void *context)
{
	struct usdf_ep *ep;
	struct usd_qp_impl *qp;
	struct usd_recv_desc rxd;
	uint32_t index;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->e.dg.ep_qp);

	index = qp->uq_rq.urq_post_index;
	rxd.urd_context = context;
	rxd.urd_iov[0].iov_base = (uint8_t *)buf +
		USDF_HDR_BUF_ENTRY - sizeof(struct usd_udp_hdr);
	rxd.urd_iov[0].iov_len = len -
		(USDF_HDR_BUF_ENTRY - sizeof(struct usd_udp_hdr));
	rxd.urd_iov_cnt = 1;
	rxd.urd_next = NULL;

	ep->e.dg.ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;

	return usd_post_recv(ep->e.dg.ep_qp, &rxd);
}

ssize_t
usdf_dgram_prefix_recvv(struct fid_ep *fep, const struct iovec *iov,
		void **desc, size_t count, fi_addr_t src_addr, void *context)
{
	struct usdf_ep *ep;
	struct usd_recv_desc rxd;
	struct usd_qp_impl *qp;
	uint32_t index;
	int i;

	ep = ep_ftou(fep);
	qp = to_qpi(ep->e.dg.ep_qp);

	rxd.urd_context = context;
	memcpy(&rxd.urd_iov[0], iov, sizeof(*iov) * count);
	rxd.urd_iov[0].iov_base = (uint8_t *)rxd.urd_iov[0].iov_base +
		USDF_HDR_BUF_ENTRY - sizeof(struct usd_udp_hdr);
	rxd.urd_iov[0].iov_len -= (USDF_HDR_BUF_ENTRY -
					sizeof(struct usd_udp_hdr));

	rxd.urd_iov_cnt = count;
	rxd.urd_next = NULL;

	index = qp->uq_rq.urq_post_index;
	for (i = 0; i < count; ++i) {
		ep->e.dg.ep_hdr_ptr[index] = rxd.urd_iov[0].iov_base;
		index = (index + 1) & qp->uq_rq.urq_post_index_mask;
	}

	return usd_post_recv(ep->e.dg.ep_qp, &rxd);
}

ssize_t
usdf_dgram_prefix_recvmsg(struct fid_ep *fep, const struct fi_msg *msg, uint64_t flags)
{
	return usdf_dgram_recvv(fep, msg->msg_iov, msg->desc,
		msg->iov_count, (fi_addr_t)msg->addr, msg->context);
}

ssize_t
usdf_dgram_prefix_send(struct fid_ep *fep, const void *buf, size_t len,
		void *desc, fi_addr_t dest_addr, void *context)
{
	struct usdf_ep *ep;
	struct usdf_dest *dest;
	struct usd_qp_impl *qp;
	struct usd_udp_hdr *hdr;
	struct usd_wq *wq;
	uint32_t last_post;
	struct usd_wq_post_info *info;

	ep = ep_ftou(fep);
	dest = (struct usdf_dest *)(uintptr_t)dest_addr;

	qp = to_qpi(ep->e.dg.ep_qp);
	wq = &qp->uq_wq;

	hdr = (struct usd_udp_hdr *) buf - 1;
	memcpy(hdr, &dest->ds_dest.ds_dest.ds_udp.u_hdr, sizeof(*hdr));

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

ssize_t
usdf_dgram_prefix_sendv(struct fid_ep *fep, const struct iovec *iov, void **desc,
		size_t count, fi_addr_t dest_addr, void *context)
{
	struct usdf_ep *ep;
	struct usd_dest *dest;
	struct usd_wq *wq;
 	struct usd_qp_impl *qp;
	struct usd_udp_hdr *hdr;
	uint32_t last_post;
	struct usd_wq_post_info *info;
	struct iovec send_iov[USDF_DGRAM_MAX_SGE];
	size_t len;
	unsigned i;

	ep = ep_ftou(fep);
	dest = (struct usd_dest *)(uintptr_t) dest_addr;

	len = 0;
	for (i = 0; i < count; i++) {
		len += iov[i].iov_len;
	}

	if (len + sizeof(struct usd_udp_hdr) > USD_SEND_MAX_COPY) {
		qp = to_qpi(ep->e.dg.ep_qp);
		wq = &qp->uq_wq;
		hdr = (struct usd_udp_hdr *) iov[0].iov_base - 1;
		memcpy(hdr, &dest->ds_dest.ds_udp.u_hdr, sizeof(*hdr));

		/* adjust lengths and insert source port */
		hdr->uh_ip.tot_len = htons(len + sizeof(struct usd_udp_hdr) -
			sizeof(struct ether_header));
		hdr->uh_udp.len = htons((sizeof(struct usd_udp_hdr) -
			sizeof(struct ether_header) -
			sizeof(struct iphdr)) + len);
		hdr->uh_udp.source =
			qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

		memcpy(send_iov, iov, sizeof(struct iovec) * count);
		send_iov[0].iov_base = hdr;
		send_iov[0].iov_len += sizeof(*hdr);

		last_post = _usd_post_send_iov(wq, send_iov, count, 1);
		info = &wq->uwq_post_info[last_post];
		info->wp_context = context;
		info->wp_len = len;
	} else {
		_usdf_dgram_send_iov_copy(ep, dest, iov, count, context);
	}
	return 0;
}

ssize_t
usdf_dgram_prefix_sendmsg(struct fid_ep *fep, const struct fi_msg *msg, uint64_t flags)
{
	return usdf_dgram_prefix_sendv(fep, msg->msg_iov, msg->desc, msg->iov_count,
				(fi_addr_t)msg->addr, msg->context);
}

ssize_t usdf_dgram_prefix_rx_size_left(struct fid_ep *fep)
{
	struct usdf_ep *ep;

	if (fep == NULL)
		return -FI_EINVAL;

	ep = ep_ftou(fep);

	if (ep->e.dg.ep_qp == NULL)
		return -FI_EOPBADSTATE; /* EP not enabled */

	/* prefix_recvv can post up to iov_limit descriptors
	 *
	 * also see NOTE-SIZE-LEFT */
	return (usd_get_recv_credits(ep->e.dg.ep_qp) / USDF_DGRAM_DFLT_SGE);
}

ssize_t usdf_dgram_prefix_tx_size_left(struct fid_ep *fep)
{
	struct usdf_ep *ep;

	if (fep == NULL)
		return -FI_EINVAL;

	ep = ep_ftou(fep);

	if (ep->e.dg.ep_qp == NULL)
		return -FI_EOPBADSTATE; /* EP not enabled */

	/* prefix_sendvcan post up to iov_limit descriptors
	 *
	 * also see NOTE-SIZE-LEFT */
	return (usd_get_send_credits(ep->e.dg.ep_qp) / USDF_DGRAM_DFLT_SGE);
}


