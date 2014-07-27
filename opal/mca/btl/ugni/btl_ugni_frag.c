/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
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

static inline void mca_btl_ugni_base_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    memset ((char *) frag + sizeof (frag->base), 0, sizeof (*frag) - sizeof (frag->base));
    frag->segments[0].base.seg_addr.pval = frag->base.super.ptr;
}

static inline void mca_btl_ugni_eager_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    struct mca_btl_ugni_reg_t *reg =
        (struct mca_btl_ugni_reg_t *) frag->base.super.registration;

    mca_btl_ugni_base_frag_constructor (frag);

    frag->segments[0].memory_handle = reg->memory_hdl;
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_smsg_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_base_frag_constructor, NULL);

OBJ_CLASS_INSTANCE(mca_btl_ugni_rdma_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_base_frag_constructor, NULL);

OBJ_CLASS_INSTANCE(mca_btl_ugni_eager_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_eager_frag_constructor, NULL);

void mca_btl_ugni_frag_init (mca_btl_ugni_base_frag_t *frag, mca_btl_ugni_module_t *ugni_module)
{
    frag->msg_id = opal_pointer_array_add (&ugni_module->pending_smsg_frags_bb, (void *) frag);
}
