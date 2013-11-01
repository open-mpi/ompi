/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_scif.h"
#include "btl_scif_frag.h"

static inline void mca_btl_scif_base_frag_constructor (mca_btl_scif_base_frag_t *frag)
{
    memset ((char *) frag + sizeof (frag->base), 0, sizeof (*frag) - sizeof (frag->base));
    frag->segments[0].base.seg_addr.pval = frag->base.super.ptr;
}

static inline void mca_btl_scif_eager_frag_constructor (mca_btl_scif_base_frag_t *frag)
{
    memset ((char *) frag + sizeof (frag->base), 0, sizeof (*frag) - sizeof (frag->base));
    frag->segments[0].base.seg_addr.pval = frag->base.super.ptr;
}

OBJ_CLASS_INSTANCE(mca_btl_scif_eager_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_scif_base_frag_constructor, NULL);

OBJ_CLASS_INSTANCE(mca_btl_scif_dma_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_scif_base_frag_constructor, NULL);
