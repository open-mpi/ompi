/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2020 Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_SM_SEND_FRAG_H
#define MCA_BTL_SM_SEND_FRAG_H

#include "opal_config.h"

static inline mca_btl_sm_frag_t *mca_btl_sm_frag_alloc(opal_free_list_t *list,
                                                       struct mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_sm_frag_t *frag = (mca_btl_sm_frag_t *) opal_free_list_get(list);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    frag->endpoint = endpoint;
    return frag;
}

static inline void mca_btl_sm_frag_return(mca_btl_sm_frag_t *frag)
{
    if (frag->hdr) {
        frag->hdr->flags = 0;
    }

    frag->segments[0].seg_addr.pval = (char *) (frag->hdr + 1);
    frag->base.des_segment_count = 1;

    opal_free_list_return(frag->my_list, (opal_free_list_item_t *) frag);
}

#define MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, endpoint) \
    (frag) = mca_btl_sm_frag_alloc(&mca_btl_sm_component.sm_frags_eager, endpoint)

#define MCA_BTL_SM_FRAG_ALLOC_MAX(frag, endpoint) \
    (frag) = mca_btl_sm_frag_alloc(&mca_btl_sm_component.sm_frags_max_send, endpoint)

#define MCA_BTL_SM_FRAG_ALLOC_USER(frag, endpoint) \
    (frag) = mca_btl_sm_frag_alloc(&mca_btl_sm_component.sm_frags_user, endpoint)

#define MCA_BTL_SM_FRAG_RETURN(frag) mca_btl_sm_frag_return(frag)

static inline void mca_btl_sm_frag_complete(mca_btl_sm_frag_t *frag)
{
    /* save the descriptor flags since the callback is allowed to free the frag */
    int des_flags = frag->base.des_flags;

    if (OPAL_UNLIKELY(MCA_BTL_DES_SEND_ALWAYS_CALLBACK & des_flags)) {
        /* completion callback */
        frag->base.des_cbfunc(&mca_btl_sm.super, frag->endpoint, &frag->base, OPAL_SUCCESS);
    }

    if (OPAL_LIKELY(des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        MCA_BTL_SM_FRAG_RETURN(frag);
    }
}

int mca_btl_sm_frag_init(opal_free_list_item_t *item, void *ctx);

static inline void mca_btl_sm_rdma_frag_advance(mca_btl_base_module_t *btl,
                                                mca_btl_base_endpoint_t *endpoint,
                                                mca_btl_sm_frag_t *frag, int status)
{
    mca_btl_sm_sc_emu_hdr_t *hdr = (mca_btl_sm_sc_emu_hdr_t *) frag->segments[0].seg_addr.pval;
    mca_btl_base_rdma_completion_fn_t cbfunc = frag->rdma.cbfunc;
    size_t hdr_size = sizeof(*hdr);
    size_t len = frag->rdma.sent ? frag->segments[0].seg_len - hdr_size : 0;
    void *context = frag->rdma.context;
    void *cbdata = frag->rdma.cbdata;
    void *data = (void *) (hdr + 1);

    if (frag->rdma.sent) {
        if (MCA_BTL_SM_OP_GET == hdr->type) {
            memcpy(frag->rdma.local_address, data, len);
        } else if ((MCA_BTL_SM_OP_ATOMIC == hdr->type || MCA_BTL_SM_OP_CSWAP == hdr->type)
                   && frag->rdma.local_address) {
            if (8 == len) {
                *((int64_t *) frag->rdma.local_address) = hdr->operand[0];
            } else {
                *((int32_t *) frag->rdma.local_address) = (int32_t) hdr->operand[0];
            }
        }
    }

    if (frag->rdma.remaining) {
        size_t packet_size = (frag->rdma.remaining + hdr_size) <= mca_btl_sm.super.btl_max_send_size
                                 ? frag->rdma.remaining
                                 : mca_btl_sm.super.btl_max_send_size - hdr_size;

        /* advance the local and remote pointers */
        frag->rdma.local_address = (void *) ((uintptr_t) frag->rdma.local_address + len);
        frag->rdma.remote_address += len;

        if (MCA_BTL_SM_OP_PUT == hdr->type) {
            /* copy the next block into the fragment buffer */
            memcpy((void *) (hdr + 1), frag->rdma.local_address, packet_size);
        }

        hdr->addr = frag->rdma.remote_address;
        /* clear out the complete flag before sending the fragment again */
        frag->hdr->flags &= ~MCA_BTL_SM_FLAG_COMPLETE;
        frag->segments[0].seg_len = packet_size + sizeof(*hdr);
        frag->rdma.sent += packet_size;
        frag->rdma.remaining -= packet_size;

        /* send is always successful */
        (void) mca_btl_sm_send(btl, endpoint, &frag->base, MCA_BTL_TAG_SM);
        return;
    }

    /* return the fragment before calling the callback */
    MCA_BTL_SM_FRAG_RETURN(frag);
    cbfunc(btl, endpoint, (void *) ((uintptr_t) frag->rdma.local_address - frag->rdma.sent), NULL,
           context, cbdata, status);
}

static inline int
mca_btl_sm_rdma_frag_start(mca_btl_base_module_t *btl, mca_btl_base_endpoint_t *endpoint, int type,
                           uint64_t operand1, uint64_t operand2, mca_btl_base_atomic_op_t op,
                           int order, int flags, size_t size, void *local_address,
                           int64_t remote_address, mca_btl_base_rdma_completion_fn_t cbfunc,
                           void *cbcontext, void *cbdata)
{
    mca_btl_sm_sc_emu_hdr_t *hdr;
    size_t hdr_size = sizeof(*hdr);
    size_t packet_size = (size + hdr_size) <= mca_btl_sm.super.btl_max_send_size
                             ? size
                             : mca_btl_sm.super.btl_max_send_size - hdr_size;
    mca_btl_sm_frag_t *frag;

    frag = (mca_btl_sm_frag_t *) mca_btl_sm_alloc(btl, endpoint, order, packet_size + hdr_size,
                                                  MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    frag->base.des_cbfunc = (mca_btl_base_completion_fn_t) mca_btl_sm_rdma_frag_advance;
    frag->rdma.local_address = local_address;
    frag->rdma.remote_address = remote_address;
    frag->rdma.cbfunc = cbfunc;
    frag->rdma.context = cbcontext;
    frag->rdma.cbdata = cbdata;
    frag->rdma.remaining = size;
    frag->rdma.sent = 0;

    hdr = (mca_btl_sm_sc_emu_hdr_t *) frag->segments[0].seg_addr.pval;

    hdr->type = type;
    hdr->addr = remote_address;
    hdr->op = op;
    hdr->flags = flags;
    hdr->operand[0] = operand1;
    hdr->operand[1] = operand2;

    mca_btl_sm_rdma_frag_advance(btl, endpoint, frag, OPAL_SUCCESS);
    return OPAL_SUCCESS;
}

#endif /* MCA_BTL_SM_SEND_FRAG_H */
