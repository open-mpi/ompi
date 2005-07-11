/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "btl_portals.h" 
#include "btl_portals_frag.h" 


static void
mca_btl_portals_frag_common_send_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->base.des_dst = 0;
    frag->base.des_dst_cnt = 0;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;

    frag->segment.seg_addr.pval = frag + sizeof(mca_btl_portals_frag_t);
    frag->segment.seg_len = frag->size;
    frag->segment.seg_key.key64 = 0;

    frag->type = MCA_BTL_PORTALS_FRAG_SEND;
}


static void
mca_btl_portals_frag_eager_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->size = mca_btl_portals_module.super.btl_eager_limit;  
    mca_btl_portals_frag_common_send_constructor(frag); 
}


static void
mca_btl_portals_frag_max_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->size = mca_btl_portals_module.super.btl_max_send_size; 
    mca_btl_portals_frag_common_send_constructor(frag); 
}


static void
mca_btl_portals_frag_user_constructor(mca_btl_portals_frag_t* frag) 
{ 
    frag->base.des_flags = 0;
    frag->base.des_dst = 0;
    frag->base.des_dst_cnt = 0;
    frag->base.des_src = 0;
    frag->base.des_src_cnt = 0;
    frag->size = 0; 
}


OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_t, 
    mca_btl_base_descriptor_t, 
    NULL, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_eager_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_portals_frag_eager_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_max_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_portals_frag_max_constructor, 
    NULL); 

OBJ_CLASS_INSTANCE(
    mca_btl_portals_frag_user_t, 
    mca_btl_base_descriptor_t, 
    mca_btl_portals_frag_user_constructor, 
    NULL); 

