/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC. All rights
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
    frag->segments[0].seg_addr.pval = frag->base.super.ptr;
}

static inline void mca_btl_ugni_eager_frag_constructor (mca_btl_ugni_base_frag_t *frag)
{
    struct mca_btl_ugni_reg_t *reg =
        (struct mca_btl_ugni_reg_t *) frag->base.super.registration;

    mca_btl_ugni_base_frag_constructor (frag);

    frag->memory_handle = reg->handle;
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_smsg_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_base_frag_constructor, NULL);

OBJ_CLASS_INSTANCE(mca_btl_ugni_rdma_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_base_frag_constructor, NULL);

OBJ_CLASS_INSTANCE(mca_btl_ugni_eager_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_ugni_eager_frag_constructor, NULL);

static void mca_btl_ugni_post_descriptor_constructor (mca_btl_ugni_post_descriptor_t *desc)
{
    desc->cq = NULL;
    desc->ep_handle = NULL;
}

OBJ_CLASS_INSTANCE(mca_btl_ugni_post_descriptor_t, opal_free_list_item_t,
                   mca_btl_ugni_post_descriptor_constructor, NULL);

int mca_btl_ugni_frag_init (mca_btl_ugni_base_frag_t *frag, void *id)
{
    /* NTH: the id is a combination of the module id and the free list id. for now there
     * is only ever one module so the module id is ignored. if this changes the code
     * here and btl_ugni_add_procs.c (opal_free_list_init calls) needs to be updated */
    intptr_t free_list_id = (intptr_t) id & 0xff;
    mca_btl_ugni_module_t *ugni_module = mca_btl_ugni_component.modules;

    frag->msg_id = opal_pointer_array_add (&ugni_module->pending_smsg_frags_bb, (void *) frag);
    frag->my_list = ugni_module->frags_lists + free_list_id;

    return OPAL_SUCCESS;
}
