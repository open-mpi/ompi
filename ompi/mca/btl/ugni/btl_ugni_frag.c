/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "btl_ugni.h"
#include "btl_ugni_frag.h"

static inline void mca_btl_ugni_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    /* send memory does not need to be registered so we do not need a mpool */
    frag->hdr = (mca_btl_ugni_frag_hdr_t *) calloc (1, sizeof (mca_btl_ugni_frag_hdr_t) + mca_btl_ugni_component.eager_limit);
    frag->segments[0].seg_addr.pval = (void *) (frag->hdr + 1);
}

static inline void mca_btl_ugni_frag_destructor (mca_btl_ugni_base_frag_t *frag)
{
    if (NULL != frag->hdr) {
        free (frag->hdr);
    }
}

static inline void mca_btl_ugni_rdma_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    /* we don't need any buffer memory for rdma frags */
    frag->hdr = NULL;
    frag->segments[0].seg_addr.pval = NULL;
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_base_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_frag_constructor, mca_btl_ugni_frag_destructor);

OBJ_CLASS_INSTANCE(mca_btl_ugni_rdma_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_rdma_frag_constructor, NULL);
