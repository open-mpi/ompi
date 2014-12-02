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

#include <errno.h>


#include "usd.h"
#include "usd_util.h"
#include "cq_enet_desc.h"

static inline int
usd_desc_to_rq_comp(
    struct usd_cq_impl *cq,
    struct cq_desc *desc,
    uint16_t qid,
    uint16_t q_index,
    struct usd_completion *comp)
{
    struct usd_rq *rq;
    struct usd_qp_impl *qp;
    struct cq_enet_rq_desc *edesc;
    uint16_t bytes_written_flags;
    uint32_t bytes_written;
    uint32_t ci_flags;
    uint32_t ipudpok;
    unsigned credits;

    edesc = (struct cq_enet_rq_desc *)desc;
    rq = cq->ucq_rq_map[qid];
    qp = usd_container_of(rq, struct usd_qp_impl, uq_rq);

    bytes_written_flags = le16_to_cpu(edesc->bytes_written_flags);
    bytes_written = bytes_written_flags & CQ_ENET_RQ_DESC_BYTES_WRITTEN_MASK;
    ci_flags = le16_to_cpu(edesc->completed_index_flags);

    if (ci_flags & CQ_ENET_RQ_DESC_FLAGS_EOP) {
        comp->uc_bytes = bytes_written + rq->urq_accum_bytes;
        rq->urq_accum_bytes = 0;
    } else {
        rq->urq_accum_bytes += bytes_written;
        return -1;
    }

    comp->uc_context = rq->urq_context[q_index];
    comp->uc_qp = &qp->uq_qp;

    ipudpok = CQ_ENET_RQ_DESC_FLAGS_IPV4_CSUM_OK |
        CQ_ENET_RQ_DESC_FLAGS_TCP_UDP_CSUM_OK;
    if (bytes_written_flags & CQ_ENET_RQ_DESC_FLAGS_TRUNCATED ||
            (edesc->flags & ipudpok) != ipudpok) {
        if (edesc->flags & CQ_ENET_RQ_DESC_FLAGS_FCS_OK ||
                bytes_written != 0)
            comp->uc_status = USD_COMPSTAT_ERROR_CRC;
        else
            comp->uc_status = USD_COMPSTAT_ERROR_TRUNC;
    } else {
        comp->uc_status = USD_COMPSTAT_SUCCESS;
    }

    /* needs a little work in multi-SGE case, all credits currently not
     * reported as released until next RX
     */
    credits = (q_index - rq->urq_last_comp) & rq->urq_post_index_mask;
    rq->urq_vnic_rq.ring.desc_avail += credits;
    rq->urq_last_comp = q_index;

    return 0;
}

static inline void
usd_desc_to_wq_comp(
    struct usd_cq_impl *cq,
    uint16_t qid,
    uint16_t q_index,
    struct usd_completion *comp)
{
    struct usd_wq *wq;
    struct usd_qp_impl *qp;
    struct usd_wq_post_info *info;
    unsigned credits;

    wq = cq->ucq_wq_map[qid];
    qp = usd_container_of(wq, struct usd_qp_impl, uq_wq);
    comp->uc_qp = &qp->uq_qp;

    info = &wq->uwq_post_info[(q_index+1)&wq->uwq_post_index_mask];
    comp->uc_context = info->wp_context;
    comp->uc_bytes = info->wp_len;
    comp->uc_status = USD_COMPSTAT_SUCCESS;

    credits = (q_index - wq->uwq_last_comp) & wq->uwq_post_index_mask;
    wq->uwq_send_credits += credits;
    wq->uwq_last_comp = q_index;
}

int
usd_poll_cq_multi(
    struct usd_cq *ucq,
    int max_comps,
    struct usd_completion *comps)
{
    int ret;
    int n;

    for (n = 0; n < max_comps; ++n) {
        ret = usd_poll_cq(ucq, comps + n);
        if (ret == -EAGAIN) {
            return n;
        }
    }
    return max_comps;
}

int
usd_poll_cq(
    struct usd_cq *ucq,
    struct usd_completion *comp)
{
    struct usd_cq_impl *cq;
    struct cq_desc *cq_desc;
    uint8_t color;
    uint8_t last_color;
    uint8_t type_color;
    uint8_t type;
    uint16_t qid;
    uint16_t q_index;

    cq = to_cqi(ucq);

retry:
    /* check for a completion */
    cq_desc = (struct cq_desc *)((uint8_t *)cq->ucq_desc_ring +
                (cq->ucq_next_desc << 4));
    last_color = cq->ucq_last_color;

    type_color = cq_desc->type_color;
    type = type_color & 0x7f;
    color = type_color >> CQ_DESC_COLOR_SHIFT;
    qid = le16_to_cpu(cq_desc->q_number) & CQ_DESC_Q_NUM_MASK;
    q_index = le16_to_cpu(cq_desc->completed_index) & CQ_DESC_COMP_NDX_MASK;

    if (color == last_color) {
        return -EAGAIN;
    } else {

        /* bookkeeping */
        cq->ucq_next_desc++;
        cq->ucq_last_color ^= (cq->ucq_next_desc >> cq->ucq_color_shift);
        cq->ucq_next_desc &= cq->ucq_cqe_mask;

        rmb();

        comp->uc_type = (enum usd_completion_type) type;

        if (type == USD_COMPTYPE_RECV) {
            if (usd_desc_to_rq_comp(cq, cq_desc, qid, q_index, comp) == -1) {
                goto retry;
            }
        } else if (type == USD_COMPTYPE_SEND) {
            usd_desc_to_wq_comp(cq, qid, q_index, comp);
        } else {
            comp->uc_status = USD_COMPSTAT_ERROR_INTERNAL;
        }
        return 0;
    }
}
