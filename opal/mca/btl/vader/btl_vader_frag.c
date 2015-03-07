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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"

static inline void mca_btl_vader_frag_constructor (mca_btl_vader_frag_t *frag)
{
    frag->hdr = (mca_btl_vader_hdr_t*)frag->base.super.ptr;
    if(frag->hdr != NULL) {
        frag->hdr->frag = frag;
        frag->hdr->flags = 0;
        frag->segments[0].seg_addr.pval = (char *)(frag->hdr + 1);
    }

    frag->base.des_segments      = frag->segments;
    frag->base.des_segment_count = 1;
    frag->fbox = NULL;
}

int mca_btl_vader_frag_init (opal_free_list_item_t *item, void *ctx)
{
    mca_btl_vader_frag_t *frag = (mca_btl_vader_frag_t *) item;
    unsigned int data_size = (unsigned int)(uintptr_t) ctx;
    unsigned int frag_size = data_size + sizeof (mca_btl_vader_hdr_t);

    /* ensure next fragment is aligned on a cache line */
    frag_size = (frag_size + 63) & ~63;

    OPAL_THREAD_LOCK(&mca_btl_vader_component.lock);

    if (data_size && mca_btl_vader_component.segment_size < mca_btl_vader_component.segment_offset + frag_size) {
        OPAL_THREAD_UNLOCK(&mca_btl_vader_component.lock);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* Set the list element here so we don't have to set it on the critical path. This only
     * works if each free list has its own unique fragment size and ALL free lists are initialized
     * with opal_free_list_init. */
    if (mca_btl_vader_component.max_inline_send == data_size) {
        frag->my_list = &mca_btl_vader_component.vader_frags_user;
    } else if (mca_btl_vader.super.btl_eager_limit == data_size) {
        frag->my_list = &mca_btl_vader_component.vader_frags_eager;
    } else if (mca_btl_vader.super.btl_max_send_size == data_size) {
        frag->my_list = &mca_btl_vader_component.vader_frags_max_send;
    }

    if (data_size) {
        item->ptr = mca_btl_vader_component.my_segment + mca_btl_vader_component.segment_offset;
        mca_btl_vader_component.segment_offset += frag_size;
    }

    OPAL_THREAD_UNLOCK(&mca_btl_vader_component.lock);

    mca_btl_vader_frag_constructor ((mca_btl_vader_frag_t *) item);

    return OPAL_SUCCESS;
}

OBJ_CLASS_INSTANCE(mca_btl_vader_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_vader_frag_constructor, NULL);
