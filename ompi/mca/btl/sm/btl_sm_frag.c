/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include "btl_sm_frag.h"


static inline void mca_btl_sm_frag_constructor(mca_btl_sm_frag_t* frag)
{
    frag->segment.seg_addr.pval = frag+1;
    frag->segment.seg_len = frag->size;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = 0;
}

static void mca_btl_sm_frag1_constructor(mca_btl_sm_frag_t* frag)
{
    frag->size = mca_btl_sm_component.eager_limit;
    frag->my_list = &mca_btl_sm_component.sm_frags1;
    mca_btl_sm_frag_constructor(frag);
}

static void mca_btl_sm_frag2_constructor(mca_btl_sm_frag_t* frag)
{
    frag->size = mca_btl_sm_component.max_frag_size;
    frag->my_list = &mca_btl_sm_component.sm_frags2;
    mca_btl_sm_frag_constructor(frag);
}

static void mca_btl_sm_frag_destructor(mca_btl_sm_frag_t* frag)
{
}


OBJ_CLASS_INSTANCE(
    mca_btl_sm_frag_t,
    mca_btl_base_descriptor_t,
    mca_btl_sm_frag_constructor,
    mca_btl_sm_frag_destructor);

OBJ_CLASS_INSTANCE(
    mca_btl_sm_frag1_t,
    mca_btl_base_descriptor_t,
    mca_btl_sm_frag1_constructor,
    mca_btl_sm_frag_destructor);

OBJ_CLASS_INSTANCE(
    mca_btl_sm_frag2_t,
    mca_btl_base_descriptor_t,
    mca_btl_sm_frag2_constructor,
    mca_btl_sm_frag_destructor);

