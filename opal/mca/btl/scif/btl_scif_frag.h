/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_SCIF_FRAG_H)
#define MCA_BTL_SCIF_FRAG_H

#include "btl_scif.h"
#include "btl_scif_endpoint.h"

typedef struct mca_btl_scif_frag_hdr_t {
#if defined(SCIF_USE_SEQ)
    uint32_t seq;
#endif
    uint8_t  tag;
    uint8_t  flags;
    uint16_t size;
} mca_btl_scif_frag_hdr_t;

struct mca_btl_scif_base_frag_t;

typedef void (*frag_cb_t) (struct mca_btl_scif_base_frag_t *, int);

typedef struct mca_btl_scif_base_frag_t {
    mca_btl_base_descriptor_t    base;
    mca_btl_scif_frag_hdr_t      hdr;
    mca_btl_base_segment_t       segments[2];
    mca_btl_base_endpoint_t     *endpoint;
    mca_btl_scif_reg_t          *registration;
    opal_free_list_t            *my_list;
} mca_btl_scif_base_frag_t;

typedef mca_btl_scif_base_frag_t mca_btl_scif_dma_frag_t;
typedef mca_btl_scif_base_frag_t mca_btl_scif_eager_frag_t;

OBJ_CLASS_DECLARATION(mca_btl_scif_dma_frag_t);
OBJ_CLASS_DECLARATION(mca_btl_scif_eager_frag_t);

static inline int mca_btl_scif_frag_alloc (mca_btl_base_endpoint_t *ep,
                                           opal_free_list_t *list,
                                           mca_btl_scif_base_frag_t **frag)
{
    *frag = (mca_btl_scif_base_frag_t *) opal_free_list_get (list);
    if (OPAL_LIKELY(NULL != *frag)) {
        (*frag)->my_list  = list;
        (*frag)->endpoint = ep;
        return OPAL_SUCCESS;
    }

    return OPAL_ERR_OUT_OF_RESOURCE;
}

static inline int mca_btl_scif_frag_return (mca_btl_scif_base_frag_t *frag)
{
    if (frag->registration) {
        frag->endpoint->btl->super.btl_mpool->mpool_deregister(frag->endpoint->btl->super.btl_mpool,
                                                               &frag->registration->base);
        frag->registration = NULL;
    }

    frag->segments[0].seg_addr.pval = frag->base.super.ptr;
    frag->segments[0].seg_len = 0;
    frag->segments[1].seg_len = 0;

    opal_free_list_return (frag->my_list, (opal_free_list_item_t *) frag);

    return OPAL_SUCCESS;
}

static inline void mca_btl_scif_frag_complete (mca_btl_scif_base_frag_t *frag, int rc) {
    BTL_VERBOSE(("frag complete. flags = %d", frag->base.des_flags));

    /* call callback if specified */
    if (frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        frag->base.des_cbfunc(&frag->endpoint->btl->super, frag->endpoint, &frag->base, rc);
    }

    if (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP) {
        mca_btl_scif_frag_return (frag);
    }
}

#define MCA_BTL_SCIF_FRAG_ALLOC_EAGER(ep, frag) \
    mca_btl_scif_frag_alloc((ep), &(ep)->btl->eager_frags, &(frag))
#define MCA_BTL_SCIF_FRAG_ALLOC_DMA(ep, frag) \
    mca_btl_scif_frag_alloc((ep), &(ep)->btl->dma_frags, &(frag))

#endif /* MCA_BTL_SCIF_FRAG_H */
