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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "btl_vader.h"
#include "btl_vader_frag.h"

static inline void mca_btl_vader_frag_constructor (mca_btl_vader_frag_t *frag)
{
    frag->hdr = (mca_btl_vader_hdr_t*)frag->base.super.ptr;
    if(frag->hdr != NULL) {
        frag->hdr->my_smp_rank = mca_btl_vader_component.my_smp_rank;
    }

    frag->base.des_src     = frag->segments;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst     = frag->segments;
    frag->base.des_dst_cnt = 1;
}

void mca_btl_vader_frag_init (ompi_free_list_item_t *item, void *ctx)
{
    unsigned int frag_size = (unsigned int)(uintptr_t) ctx;

    if (mca_btl_vader_component.segment_size < mca_btl_vader_component.segment_offset + frag_size) {
        item->ptr = NULL;
    }

    item->ptr = mca_btl_vader_component.my_segment + mca_btl_vader_component.segment_offset;
    mca_btl_vader_component.segment_offset += frag_size;

    mca_btl_vader_frag_constructor ((mca_btl_vader_frag_t *) item);
}

void mca_btl_vader_frag_return (mca_btl_vader_frag_t *frag)
{
    frag->base.des_src     = frag->segments;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst     = frag->segments;
    frag->base.des_dst_cnt = 1;

    OMPI_FREE_LIST_RETURN(frag->my_list, (ompi_free_list_item_t *)frag);
}

OBJ_CLASS_INSTANCE(mca_btl_vader_frag_t, mca_btl_base_descriptor_t,
                   mca_btl_vader_frag_constructor, NULL);
