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
#include "usdf_rdm.h"
#include "usdf_timer.h"
#include "usdf_av.h"
#include "usdf_progress.h"

#define PRINTF if (0) printf

static inline void
usdf_rdm_rdc_ready(struct usdf_rdm_connection *rdc, struct usdf_tx *tx)
{
	/* skip if we have pending send messages */
	if (!TAILQ_EMPTY(&rdc->dc_wqe_sent)) {
PRINTF("SKIP rdc %p ready due to pending wqe\n", rdc);
		return;
	}
	if (!TAILQ_ON_LIST(rdc, dc_tx_link)) {
		rdc->dc_fairness_credits = USDF_RDM_FAIRNESS_CREDITS;
		TAILQ_INSERT_TAIL(&tx->t.rdm.tx_rdc_ready, rdc, dc_tx_link);

		/* Make sure TX is on domain ready list */
		if (!TAILQ_ON_LIST(tx, tx_link)) {
			TAILQ_INSERT_TAIL(&tx->tx_domain->dom_tx_ready,
				tx, tx_link);
		}
	}
else PRINTF("RDC %p already on list\n", rdc);
}

static inline uint16_t
usdf_rdm_rdc_hash_helper(uint16_t *ipaddr, uint16_t port)
{
	uint16_t hash_index;

	hash_index = ipaddr[0];
	hash_index ^= ipaddr[1];
	hash_index ^= port;

	return hash_index & USDF_RDM_HASH_MASK;
}

static inline uint16_t
usdf_rdm_rdc_hash_hdr(struct usd_udp_hdr *hdr)
{
	return usdf_rdm_rdc_hash_helper((uint16_t *)&hdr->uh_ip.saddr,
			hdr->uh_udp.source);
}

static inline int
usdf_rdm_rdc_hdr_match(struct usdf_rdm_connection *rdc, struct usd_udp_hdr *hdr)
{
	return hdr->uh_ip.saddr == rdc->dc_hdr.uh_ip.daddr &&
	    hdr->uh_udp.source == rdc->dc_hdr.uh_udp.dest;
}

static inline int
usdf_rdm_rdc_addr_match(struct usdf_rdm_connection *rdc, uint16_t *ipaddr,
		 uint16_t port)
{
	return *(uint32_t *)ipaddr == rdc->dc_hdr.uh_ip.daddr &&
	    port == rdc->dc_hdr.uh_udp.dest;
}

/*
 * Find a matching RDM connection on this domain
 */
static inline struct usdf_rdm_connection *
usdf_rdm_rdc_addr_lookup(struct usdf_domain *udp, uint16_t *ipaddr,
		uint16_t port)
{
	uint16_t hash_index;
	struct usdf_rdm_connection *rdc;

	hash_index = usdf_rdm_rdc_hash_helper(ipaddr, port);

	rdc = udp->dom_rdc_hashtab[hash_index];

	while (rdc != NULL) {
		if (usdf_rdm_rdc_addr_match(rdc, ipaddr, port)) {
			return rdc;
		}
		rdc = rdc->dc_hash_next;
	}

	return NULL;
}

/*
 * Find a matching RDM connection on this domain
 */
static inline struct usdf_rdm_connection *
usdf_rdm_rdc_hdr_lookup(struct usdf_domain *udp, struct usd_udp_hdr *hdr)
{
	uint16_t hash_index;
	struct usdf_rdm_connection *rdc;

	hash_index = usdf_rdm_rdc_hash_hdr(hdr);

	rdc = udp->dom_rdc_hashtab[hash_index];

	while (rdc != NULL) {
		if (usdf_rdm_rdc_hdr_match(rdc, hdr)) {
			return rdc;
		}
		rdc = rdc->dc_hash_next;
	}

	return NULL;
}

/*
 * Insert rdc into domain hash table
 */
static inline void
usdf_rdm_rdc_insert(struct usdf_domain *udp, struct usdf_rdm_connection *rdc)
{
	uint16_t hash_index;

	hash_index = usdf_rdm_rdc_hash_helper(
		(uint16_t *)&rdc->dc_hdr.uh_ip.daddr,
		rdc->dc_hdr.uh_udp.dest);
PRINTF("insert rdc %p at %u\n", rdc, hash_index);

	rdc->dc_hash_next = udp->dom_rdc_hashtab[hash_index];
	udp->dom_rdc_hashtab[hash_index] = rdc;
}

static inline void
usdf_rdm_rdc_remove(struct usdf_domain *udp, struct usdf_rdm_connection *rdc)
{
	uint16_t hash_index;
	struct usdf_rdm_connection *prev;

	hash_index = usdf_rdm_rdc_hash_helper(
		(uint16_t *)&rdc->dc_hdr.uh_ip.daddr,
		rdc->dc_hdr.uh_udp.dest);
PRINTF("remove rdc %p from %u\n", rdc, hash_index);

	if (udp->dom_rdc_hashtab[hash_index] == rdc) {
		udp->dom_rdc_hashtab[hash_index] = rdc->dc_hash_next;
	} else {
		prev = udp->dom_rdc_hashtab[hash_index];
		while (prev->dc_hash_next != rdc) {
			prev = prev->dc_hash_next;
		}
		prev->dc_hash_next = rdc->dc_hash_next;
	}
}

/*
 * Get a new RDC from domain list.
 */
static inline struct usdf_rdm_connection *
usdf_rdc_alloc(struct usdf_domain *udp)
{
	struct usdf_rdm_connection *rdc;

	if (SLIST_EMPTY(&udp->dom_rdc_free)) {
		return NULL;	// XXX alloc a new batch
	} else {
		rdc = SLIST_FIRST(&udp->dom_rdc_free);
		SLIST_REMOVE_HEAD(&udp->dom_rdc_free, dc_addr_link);
		atomic_dec(&udp->dom_rdc_free_cnt);
	}
	return rdc;
}

/*
 * Get an RDM connection for this send.  If there is a connection for this
 * TX queue already attached to this destination, use that.
 * If not, check to see if one if in the connection cache (possibly put
 * there by receive).  If there is not one there either, grab a new one
 * and put it in the cache and also attch to this dest.
 */
static inline struct usdf_rdm_connection *
usdf_rdm_rdc_tx_get(struct usdf_dest *dest, struct usdf_ep *ep)
{
	struct usdf_rdm_connection *rdc;
	struct usdf_tx *tx;
	struct usdf_rx *rx;
	struct usd_qp_impl *qp;
	struct usdf_domain *udp;

	tx = ep->ep_tx;
	rx = ep->ep_rx;

	SLIST_FOREACH(rdc, &dest->ds_rdm_rdc_list, dc_addr_link) {
		if (rdc->dc_tx == tx) {
			return rdc;
		}
	}

	udp = tx->tx_domain;
	rdc = usdf_rdm_rdc_addr_lookup(udp,
		(uint16_t *)&dest->ds_dest.ds_dest.ds_udp.u_hdr.uh_ip.daddr,
		dest->ds_dest.ds_dest.ds_udp.u_hdr.uh_udp.dest);

	if (rdc == NULL) {
		rdc = usdf_rdc_alloc(udp);
		if (rdc == NULL) {
			return NULL;
		}
		memcpy(&rdc->dc_hdr,
			&dest->ds_dest.ds_dest.ds_udp.u_hdr,
			sizeof(rdc->dc_hdr));

		qp = to_qpi(rx->rx_qp);
		rdc->dc_tx = tx;
		rdc->dc_hdr.uh_udp.source = 
		    qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

		usdf_rdm_rdc_insert(udp, rdc);

		/* start eviction timer */
		usdf_timer_set(tx->tx_domain->dom_fabric, rdc->dc_timer,
				USDF_RDM_RDC_TIMEOUT);
	}

	/* Add to list for this dest */
	SLIST_INSERT_HEAD(&dest->ds_rdm_rdc_list, rdc, dc_addr_link);
	rdc->dc_dest = dest;
	rdc->dc_seq_credits = USDF_RUDP_SEQ_CREDITS;
	rdc->dc_next_tx_seq = 0;

	return rdc;
}

/*
 * See if there is matching connectoin in hash table.  If not, grab a new one.
 */
static inline struct usdf_rdm_connection *
usdf_rdm_rdc_rx_get(struct usdf_rx *rx, struct rudp_pkt *pkt)
{
	struct usdf_rdm_connection *rdc;
	struct usdf_domain *udp;
	struct usdf_tx *tx;

	udp = rx->rx_domain;
	tx = rx->r.rdm.rx_tx;

	/* if pkt->msg.src_peer_id != 0, live connection, just look up */

	rdc = usdf_rdm_rdc_hdr_lookup(udp, &pkt->hdr);
	if (rdc == NULL) {
		rdc = usdf_rdc_alloc(udp);
		if (rdc == NULL) {
			return NULL;
		}

		memcpy(&rdc->dc_hdr, pkt, sizeof(rdc->dc_hdr));
		memcpy(rdc->dc_hdr.uh_eth.ether_shost,
				pkt->hdr.uh_eth.ether_dhost, ETH_ALEN);
		memcpy(rdc->dc_hdr.uh_eth.ether_dhost,
				pkt->hdr.uh_eth.ether_shost, ETH_ALEN);
		rdc->dc_hdr.uh_ip.saddr = pkt->hdr.uh_ip.daddr;
		rdc->dc_hdr.uh_ip.daddr = pkt->hdr.uh_ip.saddr;
		rdc->dc_hdr.uh_udp.dest = pkt->hdr.uh_udp.source;
		rdc->dc_hdr.uh_udp.source = pkt->hdr.uh_udp.dest;

		rdc->dc_next_rx_seq = 0;
		rdc->dc_tx = tx;
		usdf_rdm_rdc_insert(udp, rdc);

		/* start eviction timer */
		usdf_timer_set(tx->tx_domain->dom_fabric, rdc->dc_timer,
				USDF_RDM_RDC_TIMEOUT);
	}
	return rdc;
}

/*
 * Rewind a queue entry by "rewind" packets
 */
static inline void
usdf_rdm_rewind_qe(struct usdf_rdm_qe *qe, size_t rewind, size_t mtu)
{
	size_t cur_resid;
	size_t cur_iov;
	size_t bytes;
	size_t len;

	if (qe->rd_resid == 0) {
		bytes = qe->rd_length % mtu;
		cur_resid = 0;
	} else {
		bytes = mtu;
		cur_resid = qe->rd_iov_resid;
	}
	bytes += (rewind - 1) * mtu;
	qe->rd_resid += bytes;

	cur_iov = qe->rd_cur_iov;
	while (bytes > 0) {
		len = qe->rd_iov[cur_iov].iov_len - cur_resid;
		if (len >= bytes) {
			len = bytes;
			cur_resid += len;
		} else {
			--cur_iov;
			cur_resid = 0;
		}
		bytes -= len;
	}

	qe->rd_cur_iov = cur_iov;
	qe->rd_cur_ptr = qe->rd_iov[cur_iov].iov_base +
		qe->rd_iov[cur_iov].iov_len - cur_resid;
	qe->rd_iov_resid = cur_resid;
}

/*
 * semi-native rx buffer post, i want to eventually avoid using the 
 * vnic_*() calls
 */
static inline int
_usdf_rdm_post_recv(struct usdf_rx *rx, void *buf, size_t len)
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
usdf_rdm_post_recv(struct usdf_rx *rx, void *buf, size_t len)
{
	return _usdf_rdm_post_recv(rx, buf, len);
}

ssize_t
usdf_rdm_recv(struct fid_ep *fep, void *buf, size_t len,
		void *desc, fi_addr_t src_addr, void *context)
{
	struct usdf_ep *ep;
	struct usdf_rx *rx;
	struct usdf_rdm_qe *rqe;
	struct usdf_domain *udp;

	ep = ep_ftou(fep);
	rx = ep->ep_rx;
	udp = ep->ep_domain;

	if (TAILQ_EMPTY(&rx->r.rdm.rx_free_rqe)) {
		return -FI_EAGAIN;
	}

	pthread_spin_lock(&udp->dom_progress_lock);

	rqe = TAILQ_FIRST(&rx->r.rdm.rx_free_rqe);
	TAILQ_REMOVE(&rx->r.rdm.rx_free_rqe, rqe, rd_link);

	rqe->rd_context = context;
	rqe->rd_iov[0].iov_base = buf;
	rqe->rd_iov[0].iov_len = len;
	rqe->rd_last_iov = 0;

	rqe->rd_cur_iov = 0;
	rqe->rd_cur_ptr = buf;
	rqe->rd_iov_resid = len;
	rqe->rd_length = 0;
PRINTF("RECV post rqe=%p len=%lu\n", rqe, len);

	TAILQ_INSERT_TAIL(&rx->r.rdm.rx_posted_rqe, rqe, rd_link);

	pthread_spin_unlock(&udp->dom_progress_lock);

	return 0;
}

ssize_t
usdf_rdm_recvv(struct fid_ep *fep, const struct iovec *iov, void **desc,
                 size_t count, fi_addr_t src_addr, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_rdm_send(struct fid_ep *fep, const void *buf, size_t len, void *desc,
		fi_addr_t dest_addr, void *context)
{
	struct usdf_ep *ep;
	struct usdf_tx *tx;
	struct usdf_rdm_qe *wqe;
	struct usdf_domain *udp;
	struct usdf_dest *dest;
	struct usdf_rdm_connection *rdc;
	uint32_t msg_id;

	ep = ep_ftou(fep);
	tx = ep->ep_tx;
	udp = ep->ep_domain;
	dest = (struct usdf_dest *)dest_addr;

	if (TAILQ_EMPTY(&tx->t.rdm.tx_free_wqe)) {
		return -FI_EAGAIN;
	}

	pthread_spin_lock(&udp->dom_progress_lock);

	rdc = usdf_rdm_rdc_tx_get(dest, ep);
	if (rdc == NULL) {
		pthread_spin_unlock(&udp->dom_progress_lock);
		return -FI_EAGAIN;
	}

	wqe = TAILQ_FIRST(&tx->t.rdm.tx_free_wqe);
	TAILQ_REMOVE(&tx->t.rdm.tx_free_wqe, wqe, rd_link);

	wqe->rd_context = context;

	msg_id = atomic_inc(&tx->t.rdm.tx_next_msg_id);
	wqe->rd_msg_id_be = htonl(msg_id);

	wqe->rd_iov[0].iov_base = (void *)buf;
	wqe->rd_iov[0].iov_len = len;
	wqe->rd_last_iov = 0;

	wqe->rd_cur_iov = 0;
	wqe->rd_cur_ptr = buf;
	wqe->rd_iov_resid = len;
	wqe->rd_resid = len;
	wqe->rd_length = len;

	/* add send to TX list */
	TAILQ_INSERT_TAIL(&rdc->dc_wqe_posted, wqe, rd_link);
	usdf_rdm_rdc_ready(rdc, tx);

	pthread_spin_unlock(&udp->dom_progress_lock);
PRINTF("SEND posted len=%lu, ID = %d\n", len, msg_id);

	usdf_domain_progress(udp);

	return 0;
}

ssize_t
usdf_rdm_senddata(struct fid_ep *ep, const void *buf, size_t len, void *desc,
		uint64_t data, fi_addr_t dest_addr, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_rdm_sendv(struct fid_ep *ep, const struct iovec *iov, void **desc,
                 size_t count, fi_addr_t dest_addr, void *context)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_rdm_sendmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_rdm_inject(struct fid_ep *ep, const void *buf, size_t len,
		fi_addr_t dest_addr)
{
	return -FI_ENOSYS;
}

ssize_t
usdf_rdm_recvmsg(struct fid_ep *ep, const struct fi_msg *msg, uint64_t flags)
{
	return -FI_ENOSYS;
}

/*
 * All segments send, stall this TXD until message completely ACKed
 */
static inline void
usdf_rdm_send_sent(struct usdf_tx *tx, struct usdf_rdm_connection *rdc)
{
	struct usdf_rdm_qe *wqe;

	wqe = TAILQ_FIRST(&rdc->dc_wqe_posted);
	TAILQ_REMOVE(&rdc->dc_wqe_posted, wqe, rd_link);
	TAILQ_INSERT_TAIL(&rdc->dc_wqe_sent, wqe, rd_link);

#if 0
	/* remove this RDC from TX */
if (!TAILQ_ON_LIST(rdc, dc_tx_link) abort();
	TAILQ_REMOVE_MARK(&tx->t.rdm.tx_rdc_ready, rdc, dc_tx_link);
#endif
}

static inline void
usdf_rdm_send_segment(struct usdf_tx *tx, struct usdf_rdm_connection *rdc)
{
	struct rudp_pkt *hdr;
	struct usdf_rdm_qe *wqe;
	struct usd_qp_impl *qp;
	struct usd_wq *wq;
	uint32_t index;
	size_t cur_iov;
	size_t cur_resid;
	size_t resid;
	const uint8_t *cur_ptr;
	const uint8_t *send_ptr;
	size_t sent;
	uint8_t *ptr;
	struct usd_wq_post_info *info;
	uint16_t opcode;

	wqe = TAILQ_FIRST(&rdc->dc_wqe_posted);
	qp = to_qpi(tx->tx_qp);
	wq = &(qp->uq_wq);

	index = wq->uwq_post_index;
	hdr = (struct rudp_pkt *)(wq->uwq_copybuf + index * USD_SEND_MAX_COPY);

	memcpy(hdr, &rdc->dc_hdr, sizeof(struct usd_udp_hdr));

	resid = wqe->rd_resid;
	cur_iov = wqe->rd_cur_iov;
	cur_ptr = wqe->rd_cur_ptr;
	cur_resid = wqe->rd_iov_resid;

	if (cur_ptr == wqe->rd_iov[0].iov_base) {
		opcode = RUDP_OP_FIRST;
	} else {
		opcode = RUDP_OP_MID;
	}

	if (resid < USD_SEND_MAX_COPY - sizeof(*hdr)) {
		opcode |= RUDP_OP_LAST;
		hdr->msg.opcode = htons(opcode);
		hdr->msg.msg_id = wqe->rd_msg_id_be;
		hdr->msg.m.rc_data.length = htons(resid);
		hdr->msg.m.rc_data.seqno = htons(rdc->dc_next_tx_seq);
		++rdc->dc_next_tx_seq;

		ptr = (uint8_t *)(hdr + 1);
		sent = resid;
		while (resid > 0) {
			memcpy(ptr, cur_ptr, cur_resid);
			ptr += wqe->rd_iov_resid;
			resid -= wqe->rd_iov_resid;
			++cur_iov;
			cur_ptr = wqe->rd_iov[cur_iov].iov_base;
			cur_resid = wqe->rd_iov[cur_iov].iov_len;
		}

		/* add packet lengths */
		hdr->hdr.uh_ip.tot_len = htons(
				sent + sizeof(struct rudp_pkt) -
				sizeof(struct ether_header));
		hdr->hdr.uh_udp.len = htons(
				(sizeof(struct rudp_pkt) -
				 sizeof(struct ether_header) -
				 sizeof(struct iphdr)) + sent);
PRINTF("TX 1seg=%lu, s/i = %u/%u\n", sent, ntohs(hdr->msg.m.rc_data.seqno), ntohl(hdr->msg.msg_id));

		index = _usd_post_send_one(wq, hdr,
				sent + sizeof(*hdr), 1);
	} else {
		struct vnic_wq *vwq;
		u_int8_t offload_mode = 0, eop;
		u_int16_t mss = 7, header_length = 0, vlan_tag = 0;
		u_int8_t vlan_tag_insert = 0, loopback = 0, fcoe_encap = 0;
		struct wq_enet_desc *desc;
		size_t space;
		size_t num_sge;
		size_t sge_len;

		vwq = &wq->uwq_vnic_wq;
		desc = wq->uwq_next_desc;
		space = tx->tx_domain->dom_fabric->fab_dev_attrs->uda_mtu -
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
				if (num_sge == USDF_RDM_MAX_SGE - 1 ||
				    cur_resid == resid) {
					eop = 1;
				}
				++cur_iov;
				cur_ptr = wqe->rd_iov[cur_iov].iov_base;
				cur_resid = wqe->rd_iov[cur_iov].iov_len;
			}

			wq_enet_desc_enc(desc, (uintptr_t)send_ptr, sge_len,
				mss, header_length, offload_mode, eop, eop,
				fcoe_encap, vlan_tag_insert,
				vlan_tag, loopback);

			++num_sge;
			space -= sge_len;
			resid -= sge_len;
		} while (space > 0 && num_sge <= USDF_RDM_MAX_SGE && resid > 0);

		/* add packet lengths */
		sent = tx->tx_domain->dom_fabric->fab_dev_attrs->uda_mtu -
			sizeof(*hdr) - space;
//printf("SEND sent=%lu resid=%lu\n", sent, resid);
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
//printf("BORK seq %u, ID %u\n", rdc->dc_next_tx_seq, ntohl(wqe->rd_msg_id_be));
}
#endif

		if (resid == 0) {
			opcode |= RUDP_OP_LAST;
		}
		hdr->msg.opcode = htons(opcode);
		hdr->msg.msg_id = wqe->rd_msg_id_be;
		hdr->msg.m.rc_data.length = htons(sent);
		hdr->msg.m.rc_data.seqno = htons(rdc->dc_next_tx_seq);
		++rdc->dc_next_tx_seq;
PRINTF("TX sge=%lu, s/i = %u/%u\n", sent, ntohs(hdr->msg.m.rc_data.seqno), ntohl(hdr->msg.msg_id));
					
		wmb();
		iowrite64(index, &vwq->ctrl->posted_index);

		wq->uwq_next_desc = (struct wq_enet_desc *)
		 ((uintptr_t)wq->uwq_desc_ring + (index << 4));
		wq->uwq_post_index = (index + 1) & wq->uwq_post_index_mask;
		wq->uwq_send_credits -= num_sge;
	}

	info = &wq->uwq_post_info[index];
	info->wp_context = tx;
	info->wp_len = sent;

	/* If send complete, wait for last ack on this message */
	if (resid == 0) {
		wqe->rd_resid = 0;
		usdf_rdm_send_sent(tx, rdc);
	} else {
		wqe->rd_resid = resid;
		wqe->rd_iov_resid = cur_resid;
		wqe->rd_cur_iov = cur_iov;
		wqe->rd_cur_ptr = cur_ptr;
	}

	/* set ack timer */
	usdf_timer_set(tx->tx_domain->dom_fabric, rdc->dc_timer,
			USDF_RUDP_ACK_TIMEOUT);
}

static inline void
usdf_rdm_send_ack(struct usdf_tx *tx, struct usdf_rdm_connection *rdc)
{
	struct rudp_pkt *hdr;
	struct usd_wq *wq;
	uint32_t last_post;
	struct usd_wq_post_info *info;
	uint16_t seq;

	wq = &(to_qpi(tx->tx_qp)->uq_wq);
	hdr = (struct rudp_pkt *) (wq->uwq_copybuf +
			wq->uwq_post_index * USD_SEND_MAX_COPY);

	memcpy(hdr, &rdc->dc_hdr, sizeof(struct usd_udp_hdr));

	if (rdc->dc_send_nak) {
		hdr->msg.opcode = htons(RUDP_OP_NAK);
		seq = rdc->dc_ack_seq + 1;
		hdr->msg.m.nak.nak_seq = htons(seq);
		rdc->dc_send_nak = 0;
PRINTF("TX NAK seq=%d\n", seq);
	} else {
		hdr->msg.opcode = htons(RUDP_OP_ACK);
		seq = rdc->dc_ack_seq;
		hdr->msg.m.ack.ack_seq = htons(seq);
PRINTF("TXACK seq=%u:%u\n", seq, rdc->dc_rx_msg_id);
	}
	hdr->msg.msg_id = htonl(rdc->dc_ack_msg_id);

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
usdf_rdm_send_completion(struct usd_completion *comp)
{
	struct usdf_tx *tx;

	tx = comp->uc_context;

	if (!TAILQ_EMPTY(&tx->t.rdm.tx_rdc_ready) &&
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
usdf_rdm_tx_progress(struct usdf_tx *tx)
{
	struct usdf_rdm_connection *rdc;
	struct usd_qp_impl *qp;

	qp = to_qpi(tx->tx_qp);
	while (qp->uq_wq.uwq_send_credits > 1 &&
			!TAILQ_EMPTY(&tx->t.rdm.tx_rdc_have_acks)) {
		rdc = TAILQ_FIRST(&tx->t.rdm.tx_rdc_have_acks);
		TAILQ_REMOVE_MARK(&tx->t.rdm.tx_rdc_have_acks,
				rdc, dc_ack_link);

		usdf_rdm_send_ack(tx, rdc);
	}

	while (qp->uq_wq.uwq_send_credits > 1 &&
			!TAILQ_EMPTY(&tx->t.rdm.tx_rdc_ready)) {
		rdc = TAILQ_FIRST(&tx->t.rdm.tx_rdc_ready);

		/*
		 * Send next segment on this connection. This will also
		 * remove the current WQE from the RDC list if it
		 * completes.
		 */
		usdf_rdm_send_segment(tx, rdc);

		--rdc->dc_seq_credits;
		if (!TAILQ_EMPTY(&rdc->dc_wqe_sent)) {
			TAILQ_REMOVE_MARK(&tx->t.rdm.tx_rdc_ready,
				rdc, dc_tx_link);
		} else if (TAILQ_EMPTY(&rdc->dc_wqe_posted)) {
			TAILQ_REMOVE_MARK(&tx->t.rdm.tx_rdc_ready,
				rdc, dc_tx_link);
		} else {
			--rdc->dc_fairness_credits;
			if (rdc->dc_seq_credits == 0) {
				TAILQ_REMOVE_MARK(&tx->t.rdm.tx_rdc_ready,
					rdc, dc_tx_link);
				rdc->dc_fairness_credits =
					USDF_RDM_FAIRNESS_CREDITS;

			/* fairness credits exhausted, go to back of the line */
			} else if (rdc->dc_fairness_credits == 0) {
				TAILQ_REMOVE(&tx->t.rdm.tx_rdc_ready,
					rdc, dc_tx_link);
				TAILQ_INSERT_TAIL(&tx->t.rdm.tx_rdc_ready,
					rdc, dc_tx_link);
				rdc->dc_fairness_credits =
					USDF_RDM_FAIRNESS_CREDITS;
			}
		}
	}
}

static void inline
usdf_rdm_recv_complete(struct usdf_rx *rx, struct usdf_rdm_connection *rdc,
		struct usdf_rdm_qe *rqe)
{
	struct usdf_cq_hard *hcq;

PRINTF("RECV complete ID=%u len=%lu\n", rdc->dc_rx_msg_id, rqe->rd_length);
	hcq = rx->r.rdm.rx_hcq;
	hcq->cqh_post(hcq, rqe->rd_context, rqe->rd_length);

	TAILQ_INSERT_HEAD(&rx->r.rdm.rx_free_rqe, rqe, rd_link);

	rdc->dc_cur_rqe = NULL;
}

static inline void
usdf_rdm_rdc_has_ack(struct usdf_rdm_connection *rdc)
{
	struct usdf_tx *tx;
	struct usdf_domain *udp;

	if (!TAILQ_ON_LIST(rdc, dc_ack_link)) {
		tx = rdc->dc_tx;
		udp = tx->tx_domain;
		TAILQ_INSERT_TAIL(&tx->t.rdm.tx_rdc_have_acks, rdc,
				dc_ack_link);
		/* Add TX to domain list if not present */
		if (!TAILQ_ON_LIST(tx, tx_link)) {
			TAILQ_INSERT_TAIL(&udp->dom_tx_ready, tx, tx_link);
		}
	}
}

static inline void
usdf_set_ack_nak(struct usdf_rdm_connection *rdc, uint32_t msg_id,
		uint16_t seq, uint16_t nak)
{
	/* if newly on list or msg_id > cur, use all new values */
	if (!TAILQ_ON_LIST(rdc, dc_ack_link) ||
	    RUDP_MSGID_GT(msg_id, rdc->dc_ack_msg_id)) {
		rdc->dc_ack_msg_id = msg_id;
		rdc->dc_ack_seq = seq;
		rdc->dc_send_nak = nak;

	/* If same msg_id and new seq, use new seq */
	} else if (msg_id == rdc->dc_ack_msg_id &&
		   RUDP_SEQ_GE(seq, rdc->dc_ack_seq)) {
		rdc->dc_ack_seq = seq;
		rdc->dc_send_nak = nak;
	}
		
	usdf_rdm_rdc_has_ack(rdc);
}

static inline void
usdf_set_ack(struct usdf_rdm_connection *rdc, uint32_t msg_id, uint16_t seq)
{
	usdf_set_ack_nak(rdc, msg_id, seq, 0);
}

static inline void
usdf_set_nak(struct usdf_rdm_connection *rdc, uint32_t msg_id, uint16_t seq)
{
	usdf_set_ack_nak(rdc, msg_id, seq, 1);
}

static inline struct usdf_rdm_qe *
usdf_rdm_check_seq_id(struct usdf_rdm_connection *rdc, struct usdf_rx *rx,
		struct rudp_pkt *pkt)
{
	uint16_t seq;
	uint32_t msg_id;
	int32_t msg_delta;
	struct usdf_rdm_qe *rqe;

	seq = ntohs(pkt->msg.m.rc_data.seqno);
	msg_id = ntohl(pkt->msg.msg_id);
	if (rdc->dc_flags & USDF_DCF_NEW_RX) {
		msg_delta = 1;
	} else {
		msg_delta = RUDP_SEQ_DIFF(msg_id, rdc->dc_rx_msg_id);
	}
	rqe = rdc->dc_cur_rqe;
PRINTF("RXSEQ %u:%u, msg_delt=%d, rqe=%p\n", seq, msg_id, msg_delta, rqe);

	/* old message ID */
	if (msg_delta < 0) {
		return NULL;		/* just DROP */

	/* current message ID */
	} else if (msg_delta == 0) {
		if (RUDP_SEQ_LT(seq, rdc->dc_next_rx_seq)) {
PRINTF("old SEQ, ACK %u\n", (uint16_t)(rdc->dc_next_rx_seq));
			usdf_set_ack(rdc, msg_id, rdc->dc_next_rx_seq);
		} else if (seq == rdc->dc_next_rx_seq) {
PRINTF("old SEQ, ACK %u\n", (uint16_t)(rdc->dc_next_rx_seq));
			usdf_set_ack(rdc, msg_id, rdc->dc_next_rx_seq);
			++rdc->dc_next_rx_seq;
		} else {
PRINTF("future SEQ, NAK %u\n", rdc->dc_next_rx_seq);
			usdf_set_nak(rdc, msg_id, rdc->dc_next_rx_seq - 1);
			rqe = NULL;
		}

	/* future message ID */ 
	} else {
		if (rqe != NULL) {
			return NULL;	/* DROP */
		} else if (seq != 0) {
			usdf_set_nak(rdc, msg_id, -1);
		} else if (TAILQ_EMPTY(&rx->r.rdm.rx_posted_rqe)) {
printf("RX overrun?????\n");
			usdf_set_nak(rdc, msg_id, -1);
		} else {
			rqe = TAILQ_FIRST(&rx->r.rdm.rx_posted_rqe);
			TAILQ_REMOVE(&rx->r.rdm.rx_posted_rqe, rqe, rd_link);
			rdc->dc_flags &= ~USDF_DCF_NEW_RX;
			rdc->dc_cur_rqe = rqe;
			rdc->dc_rx_msg_id = msg_id;
			usdf_set_ack(rdc, msg_id, 0);
			rdc->dc_next_rx_seq = 1;
PRINTF("start new msg, rqe=%p\n", rqe);
		}
	}
	return rqe;
}

static inline void
usdf_rdm_process_ack(struct usdf_rdm_connection *rdc, 
		struct usdf_tx *tx, uint16_t seq, uint32_t msg_id)
{
	struct usdf_cq_hard *hcq;
	struct usdf_rdm_qe *wqe;
	struct usdf_fabric *fp;
	uint16_t max_ack;
	unsigned credits;

	/* find assocoated send, drop if none */
	if (!TAILQ_EMPTY(&rdc->dc_wqe_sent)) {
		wqe = TAILQ_FIRST(&rdc->dc_wqe_sent);
	} else if (!TAILQ_EMPTY(&rdc->dc_wqe_posted)) {
		wqe = TAILQ_FIRST(&rdc->dc_wqe_posted);
	} else {
PRINTF("ACK no WQEs\n");
		return;
	}

	/* drop if not for this message */
	if (msg_id != ntohl(wqe->rd_msg_id_be)) {
PRINTF("ACK ID %u != %u\n", msg_id, ntohl(wqe->rd_msg_id_be));
		return;
	}

	/* don't try to ACK what we don't think we've sent */
	max_ack = rdc->dc_next_tx_seq - 1;
PRINTF("ACK %u max = %u\n", seq, max_ack);
	if (RUDP_SEQ_GT(seq, max_ack)) {
		seq = max_ack;
	}

	credits = RUDP_SEQ_DIFF(seq, rdc->dc_last_rx_ack);
	if (rdc->dc_seq_credits == 0 && credits > 0 &&
			!TAILQ_EMPTY(&rdc->dc_wqe_posted)) {
		usdf_rdm_rdc_ready(rdc, tx);
	}
	rdc->dc_seq_credits += credits;
	rdc->dc_last_rx_ack = seq;

	/*
	 * Look at the current send - if this ACK is for the last sequence we
	 * have sent and the message is fully sent, post a completion and move
	 * on to the next send.
	 */
	fp = tx->tx_domain->dom_fabric;
	if (seq == max_ack) {
		hcq = tx->t.rdm.tx_hcq;
		if (!TAILQ_EMPTY(&rdc->dc_wqe_sent)) {
			if (wqe->rd_resid == 0) {
				TAILQ_REMOVE(&rdc->dc_wqe_sent, wqe, rd_link);
PRINTF("send ID=%u complete\n", msg_id);
				hcq->cqh_post(hcq, wqe->rd_context,
						wqe->rd_length);

				TAILQ_INSERT_HEAD(&tx->t.rdm.tx_free_wqe,
					wqe, rd_link);

				/* prepare for next message */
				rdc->dc_next_tx_seq = 0;
				rdc->dc_last_rx_ack = rdc->dc_next_tx_seq - 1;
PRINTF("posted %s, sent %s\n", TAILQ_EMPTY(&rdc->dc_wqe_posted)?"empty":"occupied", TAILQ_EMPTY(&rdc->dc_wqe_sent)?"empty":"occupied");
				if (!TAILQ_EMPTY(&rdc->dc_wqe_posted)) {
					usdf_rdm_rdc_ready(rdc, tx);
				}
			}
		}

		/* revert to eviction timeout */
		usdf_timer_reset(fp, rdc->dc_timer, USDF_RDM_RDC_TIMEOUT);
	} else {
		usdf_timer_reset(fp, rdc->dc_timer, USDF_RUDP_ACK_TIMEOUT);
	}
}

static inline void
usdf_rdm_process_nak(struct usdf_rdm_connection *rdc, struct usdf_tx *tx,
		uint16_t seq, uint32_t msg_id)
{
	struct usdf_rdm_qe *wqe;
	struct usdf_fabric *fp;
	uint32_t wqe_msg_id;
	int rewind;

	/* Ignore NAKs of future packets */
	/* XXX or non-matching msg id */

	/* In unconnected case, only one msg in flight.  If wqe_sent != NULL,
	 * apply to that, else apply to wqe_posted
	 */
	if (!TAILQ_EMPTY(&rdc->dc_wqe_sent)) {
		wqe = TAILQ_FIRST(&rdc->dc_wqe_sent);
		wqe_msg_id = ntohl(wqe->rd_msg_id_be);
PRINTF("NAK %u:%u, next = %u:%u\n", seq, msg_id, rdc->dc_next_tx_seq, wqe_msg_id);
		if (msg_id != wqe_msg_id) {
			return;
		}
		TAILQ_REMOVE(&rdc->dc_wqe_sent, wqe, rd_link);
		TAILQ_INSERT_HEAD(&rdc->dc_wqe_posted, wqe, rd_link);
	} else if (!TAILQ_EMPTY(&rdc->dc_wqe_posted)) {
		wqe = TAILQ_FIRST(&rdc->dc_wqe_posted);
		wqe_msg_id = ntohl(wqe->rd_msg_id_be);
PRINTF("NAK %u:%u, next = %u:%u (posted)\n", seq, msg_id, rdc->dc_next_tx_seq, wqe_msg_id);
		if (msg_id != wqe_msg_id) {
			return;
		}
	} else {
PRINTF("NAK Nothing send or posted\n");
		return;
	}

	/* reset WQE to old sequence # */
	rewind = RUDP_SEQ_DIFF(rdc->dc_next_tx_seq, seq);
PRINTF("rewind = %d\n", rewind);
	if (rewind > 0) {
		rdc->dc_seq_credits = USDF_RUDP_SEQ_CREDITS;
		rdc->dc_next_tx_seq = seq;

		fp = rdc->dc_tx->tx_domain->dom_fabric;
		usdf_rdm_rewind_qe(wqe, rewind,
			fp->fab_dev_attrs->uda_mtu - sizeof(struct rudp_pkt));

		usdf_rdm_rdc_ready(rdc, tx);
	}
}

/*
 * RDC timeout could be because of needing to retransmit a packet, or it 
 * could be cache eviction timer
 */
void
usdf_rdm_rdc_timeout(void *vrdc)
{
	struct usdf_rdm_connection *rdc;
	struct usdf_rdm_qe *wqe;
	struct usdf_domain *udp;
	struct usdf_dest *dest;
	uint16_t nak;

	rdc = vrdc;
	udp = rdc->dc_tx->tx_domain;
PRINTF("RDC timer fire\n");

	pthread_spin_lock(&udp->dom_progress_lock);

	if (!TAILQ_EMPTY(&rdc->dc_wqe_sent)) {
		wqe = TAILQ_FIRST(&rdc->dc_wqe_sent);
		goto gotnak;
	} else if (!TAILQ_EMPTY(&rdc->dc_wqe_posted)) {
		wqe = TAILQ_FIRST(&rdc->dc_wqe_posted);
		goto gotnak;

	/* If inactive, remove from hash list */
	} else if (rdc->dc_cur_rqe == NULL &&
		   !TAILQ_ON_LIST(rdc, dc_tx_link) &&
		   !TAILQ_ON_LIST(rdc, dc_ack_link)) {

		dest = rdc->dc_dest;
		if (dest != NULL) {
			SLIST_REMOVE(&dest->ds_rdm_rdc_list, rdc,
				usdf_rdm_connection, dc_addr_link);
		}

		rdc->dc_dest = NULL;
		rdc->dc_flags = USDF_DCS_UNCONNECTED | USDF_DCF_NEW_RX;
		rdc->dc_next_rx_seq = 0;
		usdf_rdm_rdc_remove(udp, rdc);

		SLIST_INSERT_HEAD(&udp->dom_rdc_free, rdc, dc_addr_link);
		atomic_inc(&udp->dom_rdc_free_cnt);

	} else {
		usdf_timer_set(udp->dom_fabric, rdc->dc_timer,
				USDF_RDM_RDC_TIMEOUT);
	}
	goto done;

gotnak:
	/* wqe set above */
	nak = rdc->dc_last_rx_ack + 1;
PRINTF("TIMEOUT nak=%u:%u\n", nak, ntohl(wqe->rd_msg_id_be));
	usdf_rdm_process_nak(rdc, rdc->dc_tx, nak, ntohl(wqe->rd_msg_id_be));

done:
	pthread_spin_unlock(&udp->dom_progress_lock);
}

static inline void
usdf_rdm_rx_ack(struct usdf_rdm_connection *rdc, struct usdf_tx *tx,
		struct rudp_pkt *pkt)
{
	uint16_t seq;
	uint32_t msg_id;

	seq = ntohs(pkt->msg.m.nak.nak_seq);
	msg_id = ntohl(pkt->msg.msg_id);
PRINTF("RXACK %u:%u\n", seq, msg_id);
	usdf_rdm_process_ack(rdc, tx, seq, msg_id);
}

static inline void
usdf_rdm_rx_nak(struct usdf_rdm_connection *rdc, struct usdf_tx *tx,
		struct rudp_pkt *pkt)
{
	uint16_t seq;
	uint32_t msg_id;

	seq = ntohs(pkt->msg.m.nak.nak_seq);
	msg_id = ntohl(pkt->msg.msg_id);
	usdf_rdm_process_ack(rdc, tx, seq - 1, msg_id);

	usdf_rdm_process_nak(rdc, tx, seq, msg_id);
}

/*
 * Handle a receive on a queue servicing a message endpoint
 */
static inline void
usdf_rdm_handle_recv(struct usdf_domain *udp, struct usd_completion *comp)
{
	struct rudp_pkt *pkt;
	struct usdf_rdm_qe *rqe;
	struct usdf_rdm_connection *rdc;
	struct usd_qp *qp;
	struct usdf_rx *rx;
	uint32_t opcode;
	uint8_t *rx_ptr;
	uint8_t *rqe_ptr;
	size_t cur_iov;
	size_t iov_resid;
	size_t rxlen;
	size_t copylen;

	qp = comp->uc_qp;
	rx = qp->uq_context;
	pkt = comp->uc_context;
	opcode = ntohs(pkt->msg.opcode);

	rdc = usdf_rdm_rdc_rx_get(rx, pkt);
	if (rdc == NULL) {
		goto repost;
	}
//printf("RX opcode=%u\n", opcode);

	switch (opcode) {
	case RUDP_OP_ACK:
		usdf_rdm_rx_ack(rdc, rx->r.rdm.rx_tx, pkt);
		goto repost;

	case RUDP_OP_NAK:
		usdf_rdm_rx_nak(rdc, rx->r.rdm.rx_tx, pkt);
		goto repost;
	default:
		break;
	}

	if ((opcode & ~RUDP_OP_DATA_MASK) != 0) {
		goto repost;
	}

	/* check sequence # and msg_id */
	rqe = usdf_rdm_check_seq_id(rdc, rx, pkt);
	if (rqe == NULL) {
		goto repost;
	}

	/* Consume the data in the packet */
	rxlen = ntohs(pkt->msg.m.rc_data.length);
	rqe->rd_length += rxlen;

	rx_ptr = (uint8_t *)(pkt + 1);
	rqe_ptr = (uint8_t *)rqe->rd_cur_ptr;
	iov_resid = rqe->rd_iov_resid;
	cur_iov = rqe->rd_cur_iov;
	while (rxlen > 0) {
		copylen = MIN(rxlen, iov_resid);
		memcpy(rqe_ptr, rx_ptr, copylen);
		rx_ptr += copylen;
		rxlen -= copylen;
		iov_resid -= copylen;
		if (iov_resid == 0) {
			if (cur_iov == rqe->rd_last_iov) {
				break;
			}
			++cur_iov;
			rqe_ptr = rqe->rd_iov[cur_iov].iov_base;
			iov_resid = rqe->rd_iov[cur_iov].iov_len;
		} else {
			rqe_ptr += copylen;
		}
	}

	if (rxlen > 0) {
		rqe->rd_length -= rxlen;
/* printf("RQE truncated XXX\n"); */
	} else if (opcode & RUDP_OP_LAST) {
		usdf_rdm_recv_complete(rx, rdc, rqe);
	}

repost:
	/* repost buffer */
	_usdf_rdm_post_recv(rx, pkt,
			rx->rx_domain->dom_fabric->fab_dev_attrs->uda_mtu);
}

/*
 * Process message completions
 */
void
usdf_rdm_hcq_progress(struct usdf_cq_hard *hcq)
{
	struct usd_completion comp;
	int loop;

	loop = 100;
	while (loop-- > 0 && usd_poll_cq(hcq->cqh_ucq, &comp) != -EAGAIN) {
		switch (comp.uc_type) {
		case USD_COMPTYPE_SEND:
			usdf_rdm_send_completion(&comp);
			break;
		case USD_COMPTYPE_RECV:
			usdf_rdm_handle_recv(hcq->cqh_cq->cq_domain, &comp);
			break;
		}
	}
}
