/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(MCA_BTL_UCT_FRAG_H)
#define MCA_BTL_UCT_FRAG_H

#include "btl_uct.h"

static inline mca_btl_uct_base_frag_t *mca_btl_uct_frag_alloc (mca_btl_uct_module_t *uct_btl, opal_free_list_t *fl,
                                                               mca_btl_base_endpoint_t *endpoint)
{
    mca_btl_uct_base_frag_t *frag = (mca_btl_uct_base_frag_t *) opal_free_list_get (fl);
    if (OPAL_LIKELY(NULL != frag)) {
        frag->free_list = fl;
        frag->endpoint = endpoint;
        frag->btl = uct_btl;
    }

    return frag;
}

static inline void mca_btl_uct_frag_return (mca_btl_uct_base_frag_t *frag)
{
    opal_free_list_return (frag->free_list, &frag->base.super);
}

static inline void mca_btl_uct_frag_complete (mca_btl_uct_base_frag_t *frag, int rc) {
    mca_btl_uct_module_t *uct_btl = frag->btl;

    /* call callback if specified */
    if (frag->base.des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        frag->base.des_cbfunc(&uct_btl->super, frag->endpoint, &frag->base, rc);
    }

    if (OPAL_LIKELY(frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP)) {
        mca_btl_uct_frag_return (frag);
    }
}

static inline mca_btl_uct_base_frag_t *mca_btl_uct_frag_alloc_short (mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint)
{
    return mca_btl_uct_frag_alloc (uct_btl, &uct_btl->short_frags, endpoint);
}

static inline mca_btl_uct_base_frag_t *mca_btl_uct_frag_alloc_eager (mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint)
{
    return mca_btl_uct_frag_alloc (uct_btl, &uct_btl->eager_frags, endpoint);
}

static inline mca_btl_uct_base_frag_t *mca_btl_uct_frag_alloc_max (mca_btl_uct_module_t *uct_btl, mca_btl_base_endpoint_t *endpoint)
{
    return mca_btl_uct_frag_alloc (uct_btl, &uct_btl->max_frags, endpoint);
}

#endif /* !defined(MCA_BTL_UCT_FRAG_H) */
