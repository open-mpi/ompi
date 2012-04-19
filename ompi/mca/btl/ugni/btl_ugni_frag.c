/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
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

static inline void mca_btl_ugni_smsg_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    /* send memory does not need to be registered so we do not need a mpool */
    memset ((char *) frag + sizeof (frag->base), 0, sizeof (*frag) - sizeof (frag->base));
    frag->segments[0].seg_addr.pval = frag->base.super.ptr;
}

static inline void mca_btl_ugni_frag_destructor (mca_btl_ugni_base_frag_t *frag)
{
}

static inline void mca_btl_ugni_rdma_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    /* we don't need any buffer memory for rdma frags */
    memset ((char *) frag + sizeof (frag->base), 0, sizeof (*frag) - sizeof (frag->base));
}

static inline void mca_btl_ugni_eager_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    struct mca_btl_ugni_reg_t *reg =
        (struct mca_btl_ugni_reg_t *) frag->base.super.registration;

    memset ((char *) frag + sizeof (frag->base), 0, sizeof (*frag) - sizeof (frag->base));
    frag->segments[0].seg_addr.pval = frag->base.super.ptr;
    memmove (frag->segments[0].seg_key.key64, &reg->memory_hdl, sizeof (reg->memory_hdl));
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_smsg_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_smsg_frag_constructor, mca_btl_ugni_frag_destructor);

OBJ_CLASS_INSTANCE(mca_btl_ugni_rdma_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_rdma_frag_constructor, mca_btl_ugni_frag_destructor);

OBJ_CLASS_INSTANCE(mca_btl_ugni_eager_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_eager_frag_constructor, mca_btl_ugni_frag_destructor);

void mca_btl_ugni_frag_init (mca_btl_ugni_base_frag_t *frag, mca_btl_ugni_module_t *ugni_module)
{
    frag->msg_id = opal_atomic_add_32 (&ugni_module->next_frag_id, 1);

    opal_pointer_array_set_item (&ugni_module->pending_smsg_frags_bb, frag->msg_id, (void *) frag);
}
