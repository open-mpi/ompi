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
usd_post_send_one_udp_normal(
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
    uint32_t last_post;
    uint8_t *copybuf;
    struct usd_wq_post_info *info;

    qp = to_qpi(uqp);
    wq = &qp->uq_wq;
    copybuf = wq->uwq_copybuf + wq->uwq_post_index * USD_SEND_MAX_COPY;

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

    last_post = _usd_post_send_two(wq, hdr, sizeof(*hdr), buf, len,
                                   USD_SF_ISSET(flags, SIGNAL));

    info = &wq->uwq_post_info[last_post];
    info->wp_context = context;
    info->wp_len = len;

    return 0;
}

static int
usd_post_send_one_copy_udp_normal(
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
    uint8_t *copybuf;
    uint32_t last_post;
    struct usd_wq_post_info *info;

    qp = to_qpi(uqp);
    wq = &qp->uq_wq;
    copybuf = wq->uwq_copybuf + wq->uwq_post_index * USD_SEND_MAX_COPY;

    hdr = (struct usd_udp_hdr *) copybuf;
    memcpy(hdr, &dest->ds_dest.ds_udp.u_hdr, sizeof(*hdr));
    memcpy(hdr + 1, buf, len);

    /* adjust lengths and insert source port */
    hdr->uh_ip.tot_len = htons(len + sizeof(struct usd_udp_hdr) -
                               sizeof(struct ether_header));
    hdr->uh_udp.len = htons((sizeof(struct usd_udp_hdr) -
                             sizeof(struct ether_header) -
                             sizeof(struct iphdr)) + len);
    hdr->uh_udp.source = 
        qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

    last_post =
        _usd_post_send_one(wq, hdr, len + sizeof(struct usd_udp_hdr),
                           USD_SF_ISSET(flags, SIGNAL));

    info = &wq->uwq_post_info[last_post];
    info->wp_context = context;
    info->wp_len = len;

    return 0;
}

static int
usd_post_send_one_prefixed_udp_normal(
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
    uint32_t last_post;
    struct usd_wq_post_info *info;

    qp = to_qpi(uqp);
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

    last_post =
        _usd_post_send_one(wq, hdr, len + sizeof(struct usd_udp_hdr),
                           USD_SF_ISSET(flags, SIGNAL));

    info = &wq->uwq_post_info[last_post];
    info->wp_context = context;
    info->wp_len = len;

    return 0;
}

/*
 * 2 WQEs - our header plus user header in 1st one, user packet in 2nd
 */
static int
usd_post_send_two_copy_udp_normal(
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
    uint8_t *copybuf;
    size_t tot_ulen;
    uint32_t last_post;
    struct usd_wq_post_info *info;

    qp = to_qpi(uqp);
    wq = &qp->uq_wq;
    copybuf = wq->uwq_copybuf + wq->uwq_post_index * USD_SEND_MAX_COPY;

    hdr = (struct usd_udp_hdr *) copybuf;
    memcpy(hdr, &dest->ds_dest.ds_udp.u_hdr, sizeof(*hdr));
    memcpy(hdr + 1, uhdr, uhdrlen);
    memcpy((char *) (hdr + 1) + uhdrlen, pkt, pktlen);

    /* adjust lengths and insert source port */
    tot_ulen = uhdrlen + pktlen;
    hdr->uh_ip.tot_len = htons(tot_ulen + sizeof(struct usd_udp_hdr) -
                               sizeof(struct ether_header));
    hdr->uh_udp.len = htons((sizeof(struct usd_udp_hdr) -
                             sizeof(struct ether_header) -
                             sizeof(struct iphdr)) + tot_ulen);
    hdr->uh_udp.source = 
        qp->uq_attrs.uqa_local_addr.ul_addr.ul_udp.u_addr.sin_port;

    last_post =
        _usd_post_send_one(wq, hdr, uhdrlen + sizeof(*hdr) + pktlen,
                           USD_SF_ISSET(flags, SIGNAL));

    info = &wq->uwq_post_info[last_post];
    info->wp_context = context;
    info->wp_len = uhdrlen + pktlen;

    return 0;
}

struct usd_qp_ops usd_qp_ops_udp_normal = {
    .qo_post_send_one = usd_post_send_one_udp_normal,
    .qo_post_send_one_prefixed = usd_post_send_one_prefixed_udp_normal,
    .qo_post_send_one_copy = usd_post_send_one_copy_udp_normal,
    .qo_post_send_two_copy = usd_post_send_two_copy_udp_normal,
};
