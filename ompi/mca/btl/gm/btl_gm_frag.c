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

#include "btl_gm_frag.h" 

#define MCA_BTL_GM_FRAG_COMMON_CONSTRUCTOR(frag) \
do {                                             \
    frag->base.des_src = NULL;                   \
    frag->base.des_src_cnt = 0;                  \
    frag->base.des_dst = NULL;                   \
    frag->base.des_dst_cnt = 0;                  \
    frag->registration = NULL;                   \
} while(0)

static void mca_btl_gm_frag_eager_constructor(mca_btl_gm_frag_t* frag) 
{ 
    frag->hdr = (mca_btl_base_header_t*)(frag + 1);
    frag->segment.seg_addr.pval = (unsigned char*)(frag->hdr + 1); 
    frag->segment.seg_len = mca_btl_gm_module.super.btl_eager_limit - sizeof(mca_btl_base_header_t);
    frag->size = mca_btl_gm_component.gm_eager_frag_size;
    MCA_BTL_GM_FRAG_COMMON_CONSTRUCTOR(frag);
}

static void mca_btl_gm_frag_max_constructor(mca_btl_gm_frag_t* frag) 
{ 
    frag->hdr = (mca_btl_base_header_t*)(frag + 1);
    frag->segment.seg_addr.pval = (unsigned char*)(frag->hdr + 1); 
    frag->segment.seg_len = mca_btl_gm_module.super.btl_max_send_size - sizeof(mca_btl_base_header_t);
    frag->size = mca_btl_gm_component.gm_max_frag_size;
    MCA_BTL_GM_FRAG_COMMON_CONSTRUCTOR(frag);
}

static void mca_btl_gm_frag_user_constructor(mca_btl_gm_frag_t* frag) 
{ 
    frag->hdr = NULL;
    frag->size = 0;
    frag->registration = NULL;
    MCA_BTL_GM_FRAG_COMMON_CONSTRUCTOR(frag);
}


OBJ_CLASS_INSTANCE(
    mca_btl_gm_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_gm_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_gm_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_gm_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_gm_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_gm_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_gm_frag_user_constructor, 
    NULL); 

