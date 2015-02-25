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
#include "usdf_rudp.h"
#include "usdf_msg.h"
#include "usdf_timer.h"
#include "usdf_progress.h"

static inline void
usdf_msg_ep_ready(struct usdf_ep *ep)
{
	 struct usdf_tx *tx;

	 tx = ep->ep_tx;
	 if (!TAILQ_ON_LIST(ep, e.msg.ep_link)) {

		ep->e.msg.ep_fairness_credits = USDF_MSG_FAIRNESS_CREDITS;
		TAILQ_INSERT_TAIL(&tx->t.msg.tx_ep_ready, ep, e.msg.ep_link);

		/* Make sure TX is on domain ready list */
		if (!TAILQ_ON_LIST(tx, tx_link)) {
			TAILQ_INSERT_TAIL(&tx->tx_domain->dom_tx_ready,
				tx, tx_link);
		}
	 }
}

static inline void
usdf_msg_rewind_qe(struct usdf_msg_qe *qe, size_t rewind, size_t mtu)
{
	size_t cur_resid;
	size_t cur_iov;
	size_t bytes;
	size_t len;

	if (qe->ms_resid == 0) {
		bytes = qe->ms_length % mtu;
		cur_resid = 0;
	} else {
		bytes = mtu;
		cur_resid = qe->ms_iov_resid;
	}
	bytes += (rewind - 1) * mtu;
	qe->ms_resid += bytes;

	cur_iov = qe->ms_cur_iov;
	while (bytes > 0) {
		len = qe->ms_iov[cur_iov].iov_len - cur_resid;
		if (len >= bytes) {
			len = bytes;
			cur_resid += len;
		} else {
			--cur_iov;
			cur_resid = 0;
		}
		bytes -= len;
	}

	qe->ms_cur_iov = cur_iov;
	qe->ms_cur_ptr = qe->ms_iov[cur_iov].iov_base +
		qe->ms_iov[cur_iov].iov_len - cur_resid;
	qe->ms_iov_resid = cur_resid;
}

/*
 * semi-native rx buffer post, i want to eventually avoid using the 
 * vnic_*() calls
 */
static inline int
_usdf_msg_post_recv(struct usdf_rx *rx, void *buf, size_t len)
{
	struct usd_rq *rq;
	struct vnic_rq *vrq;
	struct rq_enet_desc *desc;
	struct usd_qp_impl *qp;

	qp = to_qpi(rx->rx_qp);
	rq = &qp->uq_rq;
	vrq = &rq->urq_vnic_rq;

	rq->urq_context[rq->urq_post_index] = buf;
	rq->urq_post_index = (rq->urq_post_index + 1)
		& rq->urq_post_index_mask;

	desc = rq->urq_next_desc;
	rq_enet_desc_enc(desc, (dma_addr_t) buf,
			RQ_ENET_TYPE_ONLY_SOP, len);
	wmb();
	iowrite32(rq->urq_post_index, &vrq->ctrl->posted_index);

	rq->urq_next_desc = (struct rq_enet_desc *)
				((uintptr_t)rq->urq_desc_ring
					+ ((rq->urq_post_index)<<4));
	rq->urq_recv_credits -= 1;

	return 0;
}

/*
 * Allow external access to the inline
 */
int
usdf_msg_post_recv(struct usdf_rx *rx, void *buf, size_t len)
{
	return _usdf_msg_post_recv(rx, buf, len);
}

ssize_t
usdf_msg_recv(struct fid_ep *fep, void *buf, size_t len,
		void *desc, fi_addr_t src_addr, void *context)
{
	struct usdf_ep *ep;
	struct usdf_rx *rx;
	struct usdf_msg_qe *rqe;
	struct usdf_domain *udp;

	ep = ep_ftou(fep);
	rx = ep->ep_rx;
	udp = ep->ep_domain;

	if (TAILQ_EMPTY(&rx->r.msg.rx_free_rqe)) {
		return -FI_EAGAIN;
	}

	pthread_spin_lock(&udp->dom_progress_lock);

	rqe = TAILQ_FIRST(&rx->r.msg.rx_free_rqe);
	TAILQ_REMOVE(&rx->r.msg.rx_free_rqe, rqe, ms_link);

	rqe->ms_context = context;
	rqe->ms_iov[0].iov_base = buf;
	rqe->ms_iov[0].iov_len = len;
	rqe->ms_last_iov = 0;

	rqe->ms_cur_iov = 0;
	rqe->ms_cur_ptr = buf;
	rqe->ms_iov_resid = len;
	rqe->ms_length = 0;

	TAILQ_INSERT_TAIL(&rx->r.msg.rx_posted_rqe, rqe, ms_link);

	pthread_spin_unlock(&udp->dom_progress_lock);

	return 0;
}

ssize_t
usdf_msg_recvv(struct fid_ep *fep, const struct iovec *iov, void **desc,
                 size_t count, fi_addr_t src_addr, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_msg_send(struct fid_ep *fep, const void *buf, size_t len, void *desc,
		fi_addr_t dest_addr, void *context)
{
	struct usdf_ep *ep;
	struct usdf_tx *tx;
	struct usdf_msg_qe *wqe;
	struct usdf_domain *udp;

	ep = ep_ftou(fep);
	tx = ep->ep_tx;
	udp = ep->ep_domain;

	if (TAILQ_EMPTY(&tx->t.msg.tx_free_wqe)) {
		return -FI_EAGAIN;
	}

	pthread_spin_lock(&udp->dom_progress_lock);

	wqe = TAILQ_FIRST(&tx->t.msg.tx_free_wqe);
	TAILQ_REMOVE(&tx->t.msg.tx_free_wqe, wqe, ms_link);

	wqe->ms_context = context;
	wqe->ms_iov[0].iov_base = (void *)buf;
	wqe->ms_iov[0].iov_len = len;
	wqe->ms_last_iov = 0;

	wqe->ms_cur_iov = 0;
	wqe->ms_cur_ptr = buf;
	wqe->ms_iov_resid = len;
	wqe->ms_resid = len;
	wqe->ms_length = len;

	/* add send to EP, and add EP to TX list if not present */
	TAILQ_INSERT_TAIL(&ep->e.msg.ep_posted_wqe, wqe, ms_link);
	usdf_msg_ep_ready(ep);

	pthread_spin_unlock(&udp->dom_progress_lock);

	usdf_domain_progress(udp);

	return 0;
}

ssize_t
usdf_msg_senddata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		uint64_t data, fi_addr_t dest_addr, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_msg_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
                 size_t count, fi_addr_t dest_addr, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_msg_sendmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_msg_inject(struct fid_ep *ep, const void *buf, size_t len,
		fi_addr_t dest_addr)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_msg_recvmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return -FI_ENOSYS;
}

static void
usdf_msg_send_complete(struct usdf_ep *ep, struct usdf_msg_qe *wqe)
{
	TAILQ_REMOVE(&ep->e.msg.ep_posted_wqe, wqe, ms_link);

	wqe->ms_last_seq = ep->e.msg.ep_next_tx_seq - 1;
	TAILQ_INSERT_TAIL(&ep->e.msg.ep_sent_wqe, wqe, ms_link);
}

static inline void
usdf_msg_send_segment(struct usdf_tx *tx, struct usdf_ep *ep)
{
	struct usdf_msg_qe *msg;
	struct rudp_pkt *hdr;
	struct usd_wq *wq;
	uint32_t index;
	size_t cur_iov;
	size_t cur_resid;
	size_t resid;
	const uint8_t *cur_ptr;
	const uint8_t *send_ptr;
	size_t sge_len;
	uint8_t *ptr;
	struct usd_wq_post_info *info;

	msg = TAILQ_FIRST(&ep->e.msg.ep_posted_wqe);
	wq = &(to_qpi(tx->tx_qp)->uq_wq);

	index = wq->uwq_post_index;
	hdr = (struct rudp_pkt *)(wq->uwq_copybuf + index * USD_SEND_MAX_COPY);

	memcpy(hdr, &ep->e.msg.ep_dest->ds_dest.ds_udp.u_hdr,
			sizeof(struct usd_udp_hdr));
	hdr->msg.src_peer_id = htons(ep->e.msg.ep_lcl_peer_id);

	resid = msg->ms_resid;
	cur_iov = msg->ms_cur_iov;
	cur_ptr = msg->ms_cur_ptr;
	cur_resid = msg->ms_iov_resid;

	/* save first seq for message */
	if (cur_iov == 0 && cur_resid == msg->ms_iov[0].iov_len) {
		msg->ms_first_seq = ep->e.msg.ep_next_tx_seq;
	}

	if (resid < USD_SEND_MAX_COPY - sizeof(*hdr)) {
		hdr->msg.opcode = htons(RUDP_OP_LAST);
		hdr->msg.m.rc_data.length = htons(resid);
		hdr->msg.m.rc_data.seqno = htons(ep->e.msg.ep_next_tx_seq);
		++ep->e.msg.ep_next_tx_seq;

		sge_len = resid;
		ptr = (uint8_t *)(hdr + 1);
		while (resid > 0) {
			memcpy(ptr, cur_ptr, cur_resid);
			ptr += msg->ms_iov_resid;
			resid -= msg->ms_iov_resid;
			++cur_iov;
			cur_ptr = msg->ms_iov[cur_iov].iov_base;
			cur_resid = msg->ms_iov[cur_iov].iov_len;
		}

		/* add packet lengths */
		hdr->hdr.uh_ip.tot_len = htons(
				sge_len + sizeof(struct rudp_pkt) -
				sizeof(struct ether_header));
		hdr->hdr.uh_udp.len = htons(
				(sizeof(struct rudp_pkt) -
				 sizeof(struct ether_header) -
				 sizeof(struct iphdr)) + sge_len);

		index = _usd_post_send_one(wq, hdr,
				sge_len + sizeof(*hdr), 1);
	} else {
		struct vnic_wq *vwq;
		u_int8_t offload_mode = 0, eop;
		u_int16_t mss = 7, header_length = 0, vlan_tag = 0;
		u_int8_t vlan_tag_insert = 0, loopback = 0, fcoe_encap = 0;
		struct wq_enet_desc *desc;
		size_t space;
		size_t num_sge;
		size_t sent;

		vwq = &wq->uwq_vnic_wq;
		desc = wq->uwq_next_desc;
		space = ep->ep_domain->dom_fabric->fab_dev_attrs->uda_mtu -
			sizeof(*hdr);
		num_sge = 1;

		/* encode header desc */
		eop = 0;
		wq_enet_desc_enc(desc, (uintptr_t)hdr, sizeof(*hdr),
			mss, header_length, offload_mode, eop, 0, fcoe_encap,
			vlan_tag_insert, vlan_tag, loopback);
		
		do {
			desc = (struct wq_enet_desc *)
				((uintptr_t)wq->uwq_desc_ring + (index << 4));
			index = (index + 1) & wq->uwq_post_index_mask;

			send_ptr = cur_ptr;
			if (cur_resid >= space) {
				sge_len = space;
				eop = 1;
				cur_resid -= sge_len;
				cur_ptr += sge_len;
			} else {
				sge_len = cur_resid;
				if (num_sge == USDF_MSG_MAX_SGE - 1 ||
				    cur_resid == resid) {
					eop = 1;
				}
				++cur_iov;
				cur_ptr = msg->ms_iov[cur_iov].iov_base;
				cur_resid = msg->ms_iov[cur_iov].iov_len;
			}

			wq_enet_desc_enc(desc, (uintptr_t)send_ptr, sge_len,
				mss, header_length, offload_mode, eop, eop,
				fcoe_encap, vlan_tag_insert,
				vlan_tag, loopback);

			++num_sge;
			space -= sge_len;
			resid -= sge_len;
		} while (space > 0 && num_sge <= USDF_MSG_MAX_SGE && resid > 0);

		/* add packet lengths */
		sent = ep->ep_domain->dom_fabric->fab_dev_attrs->uda_mtu -
			space;
		hdr->hdr.uh_ip.tot_len = htons(
				sent + sizeof(struct rudp_pkt) -
				sizeof(struct ether_header));
		hdr->hdr.uh_udp.len = htons(
				(sizeof(struct rudp_pkt) -
				 sizeof(struct ether_header) -
				 sizeof(struct iphdr)) + sent);
#if 0
if ((random() % 177) == 0 && resid == 0) {
	hdr->hdr.uh_eth.ether_type = 0;
//printf("BORK seq %u\n", ep->e.msg.ep_next_tx_seq);
}
#endif

		if (resid == 0) {
			hdr->msg.opcode = htons(RUDP_OP_LAST);
		} else {
			hdr->msg.opcode = htons(RUDP_OP_FIRST);
		}
		hdr->msg.m.rc_data.length = htons(sent);
		hdr->msg.m.rc_data.seqno = htons(ep->e.msg.ep_next_tx_seq);
		++ep->e.msg.ep_next_tx_seq;
					
		wmb();
		iowrite64(index, &vwq->ctrl->posted_index);

		wq->uwq_next_desc = (struct wq_enet_desc *)
		 ((uintptr_t)wq->uwq_desc_ring + (index << 4));
		wq->uwq_post_index = (index + 1) & wq->uwq_post_index_mask;
		wq->uwq_send_credits -= num_sge;
	}

	info = &wq->uwq_post_info[index];
	info->wp_context = tx;
	info->wp_len = sge_len;

	/* If send complete, remove from send list */
	if (resid == 0) {
		usdf_msg_send_complete(ep, msg);
	} else {
		msg->ms_resid = resid;
		msg->ms_iov_resid = cur_resid;
		msg->ms_cur_iov = cur_iov;
		msg->ms_cur_ptr = cur_ptr;
	}

	/* set ACK timer */
	usdf_timer_set(ep->ep_domain->dom_fabric, ep->e.msg.ep_ack_timer, 
			USDF_RUDP_ACK_TIMEOUT);
}

static inline void
usdf_msg_send_ack(struct usdf_tx *tx, struct usdf_ep *ep)
{
	struct rudp_pkt *hdr;
	struct usd_wq *wq;
	uint32_t last_post;
	struct usd_wq_post_info *info;
	uint16_t seq;

	wq = &(to_qpi(tx->tx_qp)->uq_wq);

	hdr = (struct rudp_pkt *) (wq->uwq_copybuf +
			wq->uwq_post_index * USD_SEND_MAX_COPY);

	memcpy(hdr, &ep->e.msg.ep_dest->ds_dest.ds_udp.u_hdr,
			sizeof(struct usd_udp_hdr));

	hdr->msg.src_peer_id = htons(ep->e.msg.ep_lcl_peer_id);
	if (ep->e.msg.ep_send_nak) {
		hdr->msg.opcode = htons(RUDP_OP_NAK);
		seq = ep->e.msg.ep_next_rx_seq;
		hdr->msg.m.nak.nak_seq = htons(seq);
		ep->e.msg.ep_send_nak = 0;
	} else {
		hdr->msg.opcode = htons(RUDP_OP_ACK);
		seq = ep->e.msg.ep_next_rx_seq - 1;
		hdr->msg.m.ack.ack_seq = htons(seq);
	}

	/* add packet lengths */
	hdr->hdr.uh_ip.tot_len = htons(
			sizeof(struct rudp_pkt) -
			sizeof(struct ether_header));
	hdr->hdr.uh_udp.len = htons(sizeof(struct rudp_pkt) -
			 sizeof(struct ether_header) - sizeof(struct iphdr));

	last_post = _usd_post_send_one(wq, hdr, sizeof(*hdr), 1);

	info = &wq->uwq_post_info[last_post];
	info->wp_context = tx;
	info->wp_len = 0;
}

/*
 * If this TX has sends to do and is not on domain ready list, then
 * this completion means we can go back on the domain ready list
 */
static void
usdf_msg_send_completion(struct usd_completion *comp)
{
	struct usdf_tx *tx;

	tx = comp->uc_context;

	if (!TAILQ_EMPTY(&tx->t.msg.tx_ep_ready) &&
	    !TAILQ_ON_LIST(tx, tx_link)) {
		TAILQ_INSERT_TAIL(&tx->tx_domain->dom_tx_ready, tx, tx_link);
	}
}

/*
 * Keep progressing sends on this queue until:
 * a) no more send credits on the queue (it's full)
 * or
 * b) all endpoints are complete or blocked awaiting ACKs
 */
void
usdf_msg_tx_progress(struct usdf_tx *tx)
{
	struct usdf_ep *ep;
	struct usd_qp_impl *qp;

	qp = to_qpi(tx->tx_qp);
	while (qp->uq_wq.uwq_send_credits > 1 &&
			!TAILQ_EMPTY(&tx->t.msg.tx_ep_have_acks)) {
		ep = TAILQ_FIRST(&tx->t.msg.tx_ep_have_acks);
		TAILQ_REMOVE_MARK(&tx->t.msg.tx_ep_have_acks,
				ep, e.msg.ep_ack_link);

		usdf_msg_send_ack(tx, ep);
	}

	while (qp->uq_wq.uwq_send_credits > 1 &&
			!TAILQ_EMPTY(&tx->t.msg.tx_ep_ready)) {
		ep = TAILQ_FIRST(&tx->t.msg.tx_ep_ready);

		/*
		 * Send next segment on this EP. This will also remove the
		 * current send from the EP send list if it completes
		 */
		usdf_msg_send_segment(tx, ep);

		--ep->e.msg.ep_seq_credits;
		if (TAILQ_EMPTY(&ep->e.msg.ep_posted_wqe)) {
			TAILQ_REMOVE_MARK(&tx->t.msg.tx_ep_ready,
					ep, e.msg.ep_link);
		} else {
			--ep->e.msg.ep_fairness_credits;
			if (ep->e.msg.ep_seq_credits == 0) {
				TAILQ_REMOVE_MARK(&tx->t.msg.tx_ep_ready,
						ep, e.msg.ep_link);
				ep->e.msg.ep_fairness_credits =
					USDF_MSG_FAIRNESS_CREDITS;

			/* fairness credits exhausted, go to back of the line */
			} else if (ep->e.msg.ep_fairness_credits == 0) {
				TAILQ_REMOVE(&tx->t.msg.tx_ep_ready,
						ep, e.msg.ep_link);
				TAILQ_INSERT_TAIL(&tx->t.msg.tx_ep_ready,
						ep, e.msg.ep_link);
				ep->e.msg.ep_fairness_credits =
					USDF_MSG_FAIRNESS_CREDITS;
			}
		}
	}
}

static void inline
usdf_msg_recv_complete(struct usdf_ep *ep, struct usdf_msg_qe *rqe)
{
	struct usdf_cq_hard *hcq;

	hcq = ep->ep_rx->r.msg.rx_hcq;
	hcq->cqh_post(hcq, rqe->ms_context, rqe->ms_length);

	TAILQ_INSERT_HEAD(&ep->ep_rx->r.msg.rx_free_rqe, rqe, ms_link);
}

static inline void
usdf_msg_ep_has_ack(struct usdf_ep *ep)
{
	struct usdf_tx *tx;
	struct usdf_domain *udp;

	if (!TAILQ_ON_LIST(ep, e.msg.ep_ack_link)) {
		tx = ep->ep_tx;
		udp = ep->ep_domain;
		TAILQ_INSERT_TAIL(&tx->t.msg.tx_ep_have_acks, ep,
				e.msg.ep_ack_link);
		/* Add TX to domain list if not present */
		if (!TAILQ_ON_LIST(tx, tx_link)) {
			TAILQ_INSERT_TAIL(&udp->dom_tx_ready, tx, tx_link);
		}

	}
}

static inline int
usdf_msg_check_seq(struct usdf_ep *ep, struct rudp_pkt *pkt)
{
	uint16_t seq;
	int ret;

	seq = ntohs(pkt->msg.m.rc_data.seqno);

	/* Drop bad seq, send NAK if seq from the future */
	if (seq != ep->e.msg.ep_next_rx_seq) {
		if (RUDP_SEQ_GT(seq, ep->e.msg.ep_next_rx_seq)) {
			ep->e.msg.ep_send_nak = 1;
		}
		ret = -1;
	} else {
		++ep->e.msg.ep_next_rx_seq;
		ret = 0;
	}
	usdf_msg_ep_has_ack(ep);

	return ret;
}

static inline void
usdf_msg_process_ack(struct usdf_ep *ep, uint16_t seq)
{
	struct usdf_cq_hard *hcq;
	struct usdf_msg_qe *wqe;
	uint16_t max_ack;
	unsigned credits;

	/* don't try to ACK what we don't think we've sent */
	max_ack = ep->e.msg.ep_next_tx_seq - 1;
	if (RUDP_SEQ_GT(seq, max_ack)) {
		seq = max_ack;
	}

	hcq = ep->ep_tx->t.msg.tx_hcq;
	while (!TAILQ_EMPTY(&ep->e.msg.ep_sent_wqe)) {
		wqe = TAILQ_FIRST(&ep->e.msg.ep_sent_wqe);
		if (RUDP_SEQ_LE(wqe->ms_last_seq, seq)) {
			TAILQ_REMOVE(&ep->e.msg.ep_sent_wqe, wqe, ms_link);
			hcq->cqh_post(hcq, wqe->ms_context, wqe->ms_length);

			TAILQ_INSERT_HEAD(&ep->ep_tx->t.msg.tx_free_wqe,
					wqe, ms_link);
		} else {
			break;
		}
	}

	credits = RUDP_SEQ_DIFF(seq, ep->e.msg.ep_last_rx_ack);
	if (ep->e.msg.ep_seq_credits == 0 && credits > 0 &&
			!TAILQ_EMPTY(&ep->e.msg.ep_posted_wqe)) {
		usdf_msg_ep_ready(ep);
	}
	ep->e.msg.ep_seq_credits += credits;
	ep->e.msg.ep_last_rx_ack = seq;

	/* If all ACKed, cancel timer, else reset it */
	if (seq == max_ack) {
		usdf_timer_cancel(ep->ep_domain->dom_fabric,
				ep->e.msg.ep_ack_timer);
	} else {
		usdf_timer_reset(ep->ep_domain->dom_fabric,
			ep->e.msg.ep_ack_timer, USDF_RUDP_ACK_TIMEOUT);
	}
}

static inline void
usdf_process_nak(struct usdf_ep *ep, uint16_t seq)
{
	struct usdf_msg_qe *wqe;
	size_t rewind;

	/* Ignore NAKs of future packets */
	if (RUDP_SEQ_GE(seq, ep->e.msg.ep_next_tx_seq)) {
		return;
	}

	/*
	 * Move any WQEs that contain NAKed sequences back to the 
	 * posted list.  We set ms_resid == 0 here because final set to zero
	 * is optimized out of the fastpath
	 */
	while (!TAILQ_EMPTY(&ep->e.msg.ep_sent_wqe)) {
		wqe = TAILQ_LAST(&ep->e.msg.ep_sent_wqe, usdf_msg_qe_head);
		TAILQ_REMOVE(&ep->e.msg.ep_sent_wqe, wqe, ms_link);
		wqe->ms_resid = 0;
		TAILQ_INSERT_HEAD(&ep->e.msg.ep_posted_wqe, wqe, ms_link);
	}
	wqe = TAILQ_FIRST(&ep->e.msg.ep_posted_wqe);

	/* reset WQE to old sequence # */
	if (wqe->ms_resid == 0) {
		rewind = RUDP_SEQ_DIFF(wqe->ms_last_seq, seq) + 1;
	} else {
		rewind = RUDP_SEQ_DIFF(ep->e.msg.ep_next_tx_seq, seq);
	}
	if (rewind > 0) {
		ep->e.msg.ep_seq_credits = USDF_RUDP_SEQ_CREDITS;
		ep->e.msg.ep_next_tx_seq = seq;

		usdf_msg_rewind_qe(wqe, rewind,
			ep->ep_domain->dom_fabric->fab_dev_attrs->uda_mtu -
			sizeof(struct rudp_pkt));

		usdf_msg_ep_ready(ep);
	}
}

void
usdf_msg_ep_timeout(void *vep)
{
	struct usdf_ep *ep;
	struct usdf_domain *udp;
	uint16_t nak;

	ep = vep;
	udp = ep->ep_domain;

	pthread_spin_lock(&udp->dom_progress_lock);
	nak = ep->e.msg.ep_last_rx_ack + 1;

	usdf_process_nak(ep, nak);
	pthread_spin_unlock(&udp->dom_progress_lock);
}

static inline void
usdf_msg_rx_ack(struct usdf_ep *ep, struct rudp_pkt *pkt)
{
	uint16_t seq;
	seq = ntohs(pkt->msg.m.ack.ack_seq);
	usdf_msg_process_ack(ep, seq);
}

static inline void
usdf_msg_rx_nak(struct usdf_ep *ep, struct rudp_pkt *pkt)
{
	uint16_t seq;

	seq = ntohs(pkt->msg.m.nak.nak_seq);
	usdf_msg_process_ack(ep, seq);

	usdf_process_nak(ep, seq);
}

/*
 * Handle a receive on a queue servicing a message endpoint
 */
static inline void
usdf_msg_handle_recv(struct usdf_domain *udp, struct usd_completion *comp)
{
	struct rudp_pkt *pkt;
	struct usdf_msg_qe *rqe;
	struct usdf_ep *ep;
	struct usd_qp *qp;
	struct usdf_rx *rx;
	uint32_t peer_id;
	uint32_t opcode;
	uint8_t *rx_ptr;
	uint8_t *rqe_ptr;
	size_t cur_iov;
	size_t iov_resid;
	size_t rxlen;
	size_t copylen;
	int ret;

	pkt = comp->uc_context;
	opcode = ntohs(pkt->msg.opcode);
	peer_id = ntohs(pkt->msg.src_peer_id);
	if (peer_id > USDF_MAX_PEERS) {
		qp = comp->uc_qp;
		rx = qp->uq_context;
		goto dropit;
	}
	ep = udp->dom_peer_tab[peer_id];
	if (ep == NULL) {
		qp = comp->uc_qp;
		rx = qp->uq_context;
		goto dropit;
	}
	rx = ep->ep_rx;

	switch (opcode) {
	case RUDP_OP_ACK:
		usdf_msg_rx_ack(ep, pkt);
		break;

	case RUDP_OP_NAK:
		usdf_msg_rx_nak(ep, pkt);
		break;

	case RUDP_OP_FIRST:
		ret = usdf_msg_check_seq(ep, pkt);
		if (ret == -1) {
			goto dropit;
		}

		rqe = ep->e.msg.ep_cur_recv;
		if (rqe == NULL) {
			if (TAILQ_EMPTY(&rx->r.msg.rx_posted_rqe)) {
				goto dropit;
			}
			rqe = TAILQ_FIRST(&rx->r.msg.rx_posted_rqe);
			TAILQ_REMOVE(&rx->r.msg.rx_posted_rqe, rqe, ms_link);
			ep->e.msg.ep_cur_recv = rqe;
		}

		rx_ptr = (uint8_t *)(pkt + 1);
		rxlen = ntohs(pkt->msg.m.rc_data.length);
		rqe->ms_length += rxlen;
		rqe_ptr = (uint8_t *)rqe->ms_cur_ptr;
		iov_resid = rqe->ms_iov_resid;
		cur_iov = rqe->ms_cur_iov;
		while (rxlen > 0) {
			copylen = MIN(rxlen, iov_resid);
			memcpy(rqe_ptr, rx_ptr, copylen);
			rx_ptr += copylen;
			rxlen -= copylen;
			iov_resid -= copylen;
			if (iov_resid == 0) {
				if (cur_iov == rqe->ms_last_iov) {
					break;
				}
				++cur_iov;
				rqe_ptr = rqe->ms_iov[cur_iov].iov_base;
				iov_resid = rqe->ms_iov[cur_iov].iov_len;
			} else {
				rqe_ptr += copylen;
			}
		}
		break;

	case RUDP_OP_LAST:
		ret = usdf_msg_check_seq(ep, pkt);
		if (ret == -1) {
			goto dropit;
		}

		rqe = ep->e.msg.ep_cur_recv;
		if (rqe == NULL) {
			rqe = TAILQ_FIRST(&rx->r.msg.rx_posted_rqe);
			if (rqe == NULL) {
				goto dropit;
			}
			TAILQ_REMOVE(&rx->r.msg.rx_posted_rqe, rqe, ms_link);
			ep->e.msg.ep_cur_recv = rqe;
		}

		rx_ptr = (uint8_t *)(pkt + 1);
		rxlen = ntohs(pkt->msg.m.rc_data.length);
		rqe->ms_length += rxlen;
		rqe_ptr = (uint8_t *)rqe->ms_cur_ptr;
		iov_resid = rqe->ms_iov_resid;
		cur_iov = rqe->ms_cur_iov;
		while (rxlen > 0) {
			copylen = MIN(rxlen, iov_resid);
			memcpy(rqe_ptr, rx_ptr, copylen);
			rx_ptr += copylen;
			rxlen -= copylen;
			iov_resid -= copylen;
			if (iov_resid == 0) {
				if (cur_iov == rqe->ms_last_iov) {
					break;
				}
				++cur_iov;
				rqe_ptr = rqe->ms_iov[cur_iov].iov_base;
				iov_resid = rqe->ms_iov[cur_iov].iov_len;
			} else {
				rqe_ptr += copylen;
			}
		}
		if (rxlen > 0) {
			rqe->ms_length -= rxlen;
/* printf("RQE truncated XXX\n"); */
		} else {
			usdf_msg_recv_complete(ep, rqe);
		}
		break;
	default:
		break;
	}

dropit:
	/* repost buffer */
	_usdf_msg_post_recv(rx, pkt,
			rx->rx_domain->dom_fabric->fab_dev_attrs->uda_mtu);
}

/*
 * Process message completions
 */
void
usdf_msg_hcq_progress(struct usdf_cq_hard *hcq)
{
	struct usd_completion comp;

	while (usd_poll_cq(hcq->cqh_ucq, &comp) != -EAGAIN) {
		switch (comp.uc_type) {
		case USD_COMPTYPE_SEND:
			usdf_msg_send_completion(&comp);
			break;
		case USD_COMPTYPE_RECV:
			usdf_msg_handle_recv(hcq->cqh_cq->cq_domain, &comp);
			break;
		}
	}
}
