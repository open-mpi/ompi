/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
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
 *
 * LICENSE_END
 *
 *
 */

#ifndef _USD_POST_H_
#define _USD_POST_H_

#include "usd.h"
#include "usd_util.h"

static inline uint32_t
_usd_post_send_one(
    struct usd_wq *wq,
    const void *packet,
    size_t length,
    u_int8_t cq_entry)
{
    struct vnic_wq *vwq;
    uint32_t index;
    struct wq_enet_desc *desc;
    uint64_t wr;
    u_int8_t offload_mode = 0, eop = 1;
    u_int16_t mss = 7, header_length = 0, vlan_tag = 0;
    u_int8_t vlan_tag_insert = 0, loopback = 0, fcoe_encap = 0;

    vwq = &wq->uwq_vnic_wq;
    desc = wq->uwq_next_desc;
    index = wq->uwq_post_index;

    wq_enet_desc_enc(desc, (uintptr_t)packet, length,
        mss, header_length, offload_mode,
        eop, cq_entry, fcoe_encap,
        vlan_tag_insert, vlan_tag, loopback);
    wmb();

    wr = vnic_cached_posted_index((dma_addr_t)packet, length, index);
    iowrite64(wr, &vwq->ctrl->posted_index);

    wq->uwq_next_desc = (struct wq_enet_desc *)
        ((uintptr_t)wq->uwq_desc_ring + (index<<4));
    wq->uwq_post_index = (index+1) & wq->uwq_post_index_mask;
    wq->uwq_send_credits--;

    return index;
}

static inline uint32_t
_usd_post_send_two(
    struct usd_wq *wq,
    const void *hdr,
    size_t hdrlen,
    const void *pkt,
    size_t pktlen,
    u_int8_t cq_entry)
{
    struct vnic_wq *vwq;
    uint32_t index;
    struct wq_enet_desc *desc;
    uint64_t wr;
    u_int8_t offload_mode = 0, eop;
    u_int16_t mss = 7, header_length = 0, vlan_tag = 0;
    u_int8_t vlan_tag_insert = 0, loopback = 0, fcoe_encap = 0;

    vwq = &wq->uwq_vnic_wq;
    desc = wq->uwq_next_desc;
    index = wq->uwq_post_index;

    eop = 0;
    wq_enet_desc_enc(desc, (uintptr_t)hdr, hdrlen,
        mss, header_length, offload_mode,
        eop, 0, fcoe_encap,
        vlan_tag_insert, vlan_tag, loopback);

    desc = (struct wq_enet_desc *) ((uintptr_t)wq->uwq_desc_ring + (index<<4));
    index = (index+1) & wq->uwq_post_index_mask;

    eop = 1;
    wq_enet_desc_enc(desc, (uintptr_t)pkt, pktlen,
        mss, header_length, offload_mode,
        eop, cq_entry, fcoe_encap,
        vlan_tag_insert, vlan_tag, loopback);
    wmb();

    wr = vnic_cached_posted_index((dma_addr_t)hdr, hdrlen, index);
    iowrite64(wr, &vwq->ctrl->posted_index);

    wq->uwq_next_desc = (struct wq_enet_desc *)
        ((uintptr_t)wq->uwq_desc_ring + (index<<4));
    wq->uwq_post_index = (index+1) & wq->uwq_post_index_mask;
    wq->uwq_send_credits -= 2;

    return index;
}

#endif /* _USD_POST_H_ */
