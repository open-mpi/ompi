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

#include "usd.h"
#include "usd_post.h"

unsigned
usd_get_send_credits(
    struct usd_qp *uqp)
{
    struct usd_qp_impl *qp;

    qp = to_qpi(uqp);

    return qp->uq_wq.uwq_send_credits;
}

unsigned
usd_get_recv_credits(
    struct usd_qp *uqp)
{
    struct usd_qp_impl *qp;

    qp = to_qpi(uqp);

    return qp->uq_rq.urq_recv_credits;
}

int
usd_post_recv(
    struct usd_qp *uqp,
    struct usd_recv_desc *recv_list)
{
    struct usd_qp_impl *qp;
    struct usd_rq *rq;
    struct vnic_rq *vrq;
    struct rq_enet_desc *desc;
    struct iovec *iovp;
    uint32_t index;
    uint32_t count;
    unsigned i;

    qp = to_qpi(uqp);
    rq = &qp->uq_rq;
    vrq = &rq->urq_vnic_rq;
    desc = rq->urq_next_desc;
    index = rq->urq_post_index;

    iovp = recv_list->urd_iov;
    count = 0;

    while (recv_list != NULL) {
        rq->urq_context[index] = recv_list->urd_context;
        rq_enet_desc_enc(desc, (dma_addr_t) iovp[0].iov_base,
                         RQ_ENET_TYPE_ONLY_SOP, iovp[0].iov_len);
        count++;

        index = (index+1) & rq->urq_post_index_mask;
        desc = (struct rq_enet_desc *) ((uintptr_t)rq->urq_desc_ring
                                            + (index<<4));

        for (i = 1; i < recv_list->urd_iov_cnt; ++i) {
            rq->urq_context[index] = recv_list->urd_context;
            rq_enet_desc_enc(desc, (dma_addr_t) iovp[i].iov_base,
                             RQ_ENET_TYPE_NOT_SOP, iovp[i].iov_len);
            count++;

            index = (index+1) & rq->urq_post_index_mask;
            desc = (struct rq_enet_desc *) ((uintptr_t)rq->urq_desc_ring
                                            + (index<<4));
        }
        recv_list = recv_list->urd_next;
    }

    wmb();
    iowrite32(index, &vrq->ctrl->posted_index);

    rq->urq_next_desc = desc;
    rq->urq_post_index = index;
    rq->urq_recv_credits -= count;

    return 0;
}
