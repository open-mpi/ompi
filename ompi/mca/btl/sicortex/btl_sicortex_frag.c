/*
 * Copyright (c) 2008-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "btl_sicortex_frag.h" 



static void mca_btl_sicortex_frag_common_constructor(mca_btl_sicortex_frag_t* frag) 
{ 
    frag->base.des_src     = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst     = &(frag->segment);
    frag->base.des_dst_cnt = 1;
#if MCA_BTL_HAS_MPOOL
    frag->registration = NULL;
#endif
}

static void mca_btl_sicortex_frag_eager_constructor(mca_btl_sicortex_frag_t* frag) 
{ 
    frag->size = mca_btl_sicortex_module.super.btl_eager_limit;  
    frag->my_list = &mca_btl_sicortex_component.sicortex_frag_eager;
    mca_btl_sicortex_frag_common_constructor(frag); 
}

static void mca_btl_sicortex_frag_max_constructor(mca_btl_sicortex_frag_t* frag) 
{ 
    frag->size = mca_btl_sicortex_module.super.btl_max_send_size; 
    frag->my_list = &mca_btl_sicortex_component.sicortex_frag_max;
    mca_btl_sicortex_frag_common_constructor(frag); 
}

static void mca_btl_sicortex_frag_user_constructor(mca_btl_sicortex_frag_t* frag) 
{ 
    frag->size = 0; 
    frag->my_list = &mca_btl_sicortex_component.sicortex_frag_user;
    mca_btl_sicortex_frag_common_constructor(frag); 
}


OBJ_CLASS_INSTANCE(
    mca_btl_sicortex_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_sicortex_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_sicortex_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_sicortex_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_sicortex_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_sicortex_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_sicortex_frag_user_constructor, 
    NULL); 

