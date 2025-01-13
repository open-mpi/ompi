/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2025      Google, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_uct_frag.h"

static void mca_btl_uct_base_frag_constructor(mca_btl_uct_base_frag_t *frag)
{
    mca_btl_uct_reg_t *reg = (mca_btl_uct_reg_t *) frag->base.super.registration;

    /* zero everything out */
    memset((char *) frag + sizeof(frag->base), 0, sizeof(*frag) - sizeof(frag->base));

    OBJ_CONSTRUCT(&frag->comp, mca_btl_uct_uct_completion_t);
    frag->comp.frag = frag;

    frag->base.des_segments = frag->segments;
    frag->base.des_segment_count = 1;

    frag->segments[0].seg_addr.pval = frag->base.super.ptr;
    frag->uct_iov.buffer = frag->base.super.ptr;
    frag->uct_iov.stride = 0;
    frag->uct_iov.count = 1;
    if (reg) {
        frag->uct_iov.memh = reg->uct_memh;
    }
}

static void mca_btl_uct_base_frag_destructor(mca_btl_uct_base_frag_t *frag)
{
    OBJ_DESTRUCT(&frag->comp);
}

OBJ_CLASS_INSTANCE(mca_btl_uct_base_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_uct_base_frag_constructor, mca_btl_uct_base_frag_destructor);
