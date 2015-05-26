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

#include <pthread.h>

#include "usd.h"
#include "usd_post.h"

static int
usd_post_send_one_udp_pio(
    struct usd_qp *uqp,
    struct usd_dest *dest,
    const void *buf,
    size_t len,
    uint32_t flags,
    void *context)
{
    struct usd_qp_impl *qp;
    struct usd_udp_hdr *hdr;
    struct usd_wq *wq;
    struct usd_wq_post_info *info;
    struct vnic_wq *vwq;
    uint32_t index;
    struct wq_enet_desc *desc;
    char *v_pkt;
    uint64_t p_pkt;
    uint64_t *s, *d;
    uint32_t copylen;
    uint8_t *copybuf;

    u_int8_t offload_mode = 0, eop = 1;
    u_int16_t mss = 7, header_length = 0, vlan_tag = 0;
    u_int8_t vlan_tag_insert = 0, loopback = 0, fcoe_encap = 0;

    qp = to_qpi(uqp);
    wq = &qp->uq_wq;

    hdr = &dest->ds_dest.ds_udp.u_hdr;

    /* adjust lengths and insert source port */
    hdr->uh_ip.tot_len = htons(len + sizeof(struct usd_udp_hdr) -
                sizeof(struct ether_header));
    hdr->uh_udp.len = htons((sizeof(struct usd_udp_hdr) -
                             sizeof(struct ether_header) -
                             sizeof(struct iphdr)) + len);
    hdr->uh_udp.source = 
        qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

    vwq = &wq->uwq_vnic_wq;
    desc = wq->uwq_next_desc;
    index = wq->uwq_post_index;

    v_pkt = wq->pio_v_pkt_buf + index * 256;
    p_pkt = wq->pio_p_pkt_buf + index * 256;
    copylen = (len + sizeof(*hdr) + 7) & ~7;
//printf("len = %lu, p_pkt = 0x%lx, index = %d\n", len, p_pkt, index);
    d = (uint64_t *)v_pkt;
    d[0] = ((uint64_t *)hdr)[0];
    d[1] = ((uint64_t *)hdr)[1];
    d[2] = ((uint64_t *)hdr)[2];
    d[3] = ((uint64_t *)hdr)[3];
    d[4] = ((uint64_t *)hdr)[4];

    d += 5;
    copybuf = wq->uwq_copybuf;
    memcpy(copybuf + 2, buf, len);
    s = (uint64_t *)copybuf;

    /* 40 bytes already copied */
    while (copylen > 40) {
        *d++ = *s++;
        copylen -= 8;
    }

    /* encode in shadow ring and write 64 bytes */
    wq_enet_desc_enc(desc, (uintptr_t)p_pkt, len + sizeof(*hdr),
        mss, header_length, offload_mode,
        eop, USD_SF_ISSET(flags, SIGNAL), fcoe_encap,
        vlan_tag_insert, vlan_tag, loopback);

    d = (uint64_t *)((uintptr_t)wq->pio_v_wq_addr + (uintptr_t)desc -
        (uintptr_t)wq->uwq_desc_ring);
    s = (uint64_t *)desc;
    d[0] = s[0];
    d[1] = s[1];

    wmb();

//printf("post %lu[%d] p=0x%lx\n", len + sizeof(*hdr), index, p_pkt);
    iowrite32(index, &vwq->ctrl->posted_index);

    wq->uwq_next_desc = (struct wq_enet_desc *)
        ((uintptr_t)wq->uwq_desc_ring + (index<<4));
    wq->uwq_post_index = (index+1) & wq->uwq_post_index_mask;
    wq->uwq_send_credits--;

    info = &wq->uwq_post_info[index];
    info->wp_context = context;
    info->wp_len = len;

    return 0;
}

/*
 * 2 WQEs - our header plus user header in 1st one, user packet in 2nd
 */
static int
usd_post_send_two_udp_pio(
    struct usd_qp *uqp,
    struct usd_dest *dest,
    const void *uhdr,
    size_t uhdrlen,
    const void *pkt,
    size_t pktlen,
    uint32_t flags,
    void *context)
{
    struct usd_qp_impl *qp;
    struct usd_udp_hdr *hdr;
    struct usd_wq *wq;
    struct usd_wq_post_info *info;
    struct vnic_wq *vwq;
    uint32_t index;
    struct wq_enet_desc *desc;
    char *v_pkt;
    uint64_t p_pkt;
    uint64_t *s, *d;
    uint32_t copylen;
    uint8_t *copybuf;
    size_t len;

    u_int8_t offload_mode = 0, eop = 1;
    u_int16_t mss = 7, header_length = 0, vlan_tag = 0;
    u_int8_t vlan_tag_insert = 0, loopback = 0, fcoe_encap = 0;

    qp = to_qpi(uqp);
    wq = &qp->uq_wq;

    hdr = &dest->ds_dest.ds_udp.u_hdr;
    len = uhdrlen + pktlen;

    /* adjust lengths and insert source port */
    hdr->uh_ip.tot_len = htons(len + sizeof(struct usd_udp_hdr) -
                sizeof(struct ether_header));
    hdr->uh_udp.len = htons((sizeof(struct usd_udp_hdr) -
                             sizeof(struct ether_header) -
                             sizeof(struct iphdr)) + len);
    hdr->uh_udp.source = 
        qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

    vwq = &wq->uwq_vnic_wq;
    desc = wq->uwq_next_desc;
    index = wq->uwq_post_index;

    v_pkt = wq->pio_v_pkt_buf + index * 256;
    p_pkt = wq->pio_p_pkt_buf + index * 256;
    copylen = (len + sizeof(*hdr) + 7) & ~7;
//printf("len = %lu, p_pkt = 0x%lx, index = %d\n", len, p_pkt, index);
    d = (uint64_t *)v_pkt;
    d[0] = ((uint64_t *)hdr)[0];
    d[1] = ((uint64_t *)hdr)[1];
    d[2] = ((uint64_t *)hdr)[2];
    d[3] = ((uint64_t *)hdr)[3];
    d[4] = ((uint64_t *)hdr)[4];

    d += 5;
    copybuf = wq->uwq_copybuf;
    memcpy(copybuf + 2, uhdr, uhdrlen);
    memcpy(copybuf + 2 + uhdrlen, pkt, pktlen);
    s = (uint64_t *)copybuf;

    /* 40 bytes already copied */
    while (copylen > 40) {
        *d++ = *s++;
        copylen -= 8;
    }

    /* encode in shadow ring and write 64 bytes */
    wq_enet_desc_enc(desc, (uintptr_t)p_pkt, len + sizeof(*hdr),
        mss, header_length, offload_mode,
        eop, USD_SF_ISSET(flags, SIGNAL), fcoe_encap,
        vlan_tag_insert, vlan_tag, loopback);

    d = (uint64_t *)((uintptr_t)wq->pio_v_wq_addr + (uintptr_t)desc -
        (uintptr_t)wq->uwq_desc_ring);
    s = (uint64_t *)desc;
    d[0] = s[0];
    d[1] = s[1];

    wmb();

//printf("post %lu[%d] p=0x%lx\n", len + sizeof(*hdr), index, p_pkt);
    iowrite32(index, &vwq->ctrl->posted_index);

    wq->uwq_next_desc = (struct wq_enet_desc *)
        ((uintptr_t)wq->uwq_desc_ring + (index<<4));
    wq->uwq_post_index = (index+1) & wq->uwq_post_index_mask;
    wq->uwq_send_credits--;

    info = &wq->uwq_post_info[index];
    info->wp_context = context;
    info->wp_len = len;

    return 0;
}

struct usd_qp_ops usd_qp_ops_udp_pio = {
    .qo_post_send_one = usd_post_send_one_udp_pio,
    .qo_post_send_one_prefixed = usd_post_send_one_udp_pio,
    .qo_post_send_one_copy = usd_post_send_one_udp_pio,
    .qo_post_send_two_copy = usd_post_send_two_udp_pio,
};
