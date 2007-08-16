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
 * Copyright (c) 2007      Cisco, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "btl_mvapi_frag.h" 

static void mca_btl_mvapi_frag_common_constructor( mca_btl_mvapi_frag_t* frag) 
{
    mca_btl_mvapi_reg_t* mem_hndl =
        (mca_btl_mvapi_reg_t*)frag->base.super.user_data;
    frag->hdr = (mca_btl_mvapi_header_t*) (frag+1);  /* initialize btl header to start at end of frag */ 
    frag->segment.seg_addr.pval = ((unsigned char* )frag->hdr) + sizeof(mca_btl_mvapi_header_t);  
    /* init the segment address to start after the btl header */ 
    
    frag->segment.seg_len = frag->size;
    frag->sg_entry.lkey = mem_hndl->l_key;
    frag->segment.seg_key.key32[0] = frag->sg_entry.lkey;
    frag->sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t) frag->hdr; 
    frag->base.des_flags = 0; 
}

static void mca_btl_mvapi_send_frag_common_constructor(mca_btl_mvapi_frag_t* frag) 
{ 
    
    mca_btl_mvapi_frag_common_constructor(frag); 
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    
    frag->desc.sr_desc.comp_type = VAPI_SIGNALED; 
    frag->desc.sr_desc.opcode = VAPI_SEND; 
    frag->desc.sr_desc.remote_qkey = 0; 
    frag->desc.sr_desc.sg_lst_len = 1; 
    frag->desc.sr_desc.sg_lst_p = &frag->sg_entry; 
    frag->desc.sr_desc.id = (VAPI_virt_addr_t) (MT_virt_addr_t) frag; 
    
}

static void mca_btl_mvapi_recv_frag_common_constructor(mca_btl_mvapi_frag_t* frag) 
{ 
    
    mca_btl_mvapi_frag_common_constructor(frag); 
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    
    frag->desc.rr_desc.comp_type = VAPI_SIGNALED; 
    frag->desc.rr_desc.opcode = VAPI_RECEIVE; 
    frag->desc.rr_desc.sg_lst_len = 1; 
    frag->desc.rr_desc.sg_lst_p = &frag->sg_entry; 
    frag->desc.rr_desc.id = (VAPI_virt_addr_t) (MT_virt_addr_t) frag; 
    
   
}



static void mca_btl_mvapi_send_frag_eager_constructor(mca_btl_mvapi_frag_t* frag) 
{ 
    
    frag->size = mca_btl_mvapi_component.eager_limit;
    frag->type = MCA_BTL_MVAPI_FRAG_EAGER;  
    mca_btl_mvapi_send_frag_common_constructor(frag); 
}


static void mca_btl_mvapi_send_frag_max_constructor(mca_btl_mvapi_frag_t* frag) 
{ 
    
    frag->size = mca_btl_mvapi_component.max_send_size;
    frag->type = MCA_BTL_MVAPI_FRAG_MAX; 
    mca_btl_mvapi_send_frag_common_constructor(frag); 
}

static void mca_btl_mvapi_recv_frag_max_constructor(mca_btl_mvapi_frag_t* frag) 
{
    frag->size = mca_btl_mvapi_component.max_send_size;
    frag->type = MCA_BTL_MVAPI_FRAG_MAX; 
    mca_btl_mvapi_recv_frag_common_constructor(frag); 
    
}


static void mca_btl_mvapi_recv_frag_eager_constructor(mca_btl_mvapi_frag_t* frag) 
{
    frag->size = mca_btl_mvapi_component.eager_limit; 
    frag->type = MCA_BTL_MVAPI_FRAG_EAGER;
    mca_btl_mvapi_recv_frag_common_constructor(frag); 
    frag->ftr = (mca_btl_mvapi_footer_t*)((char*)frag->segment.seg_addr.pval
            + frag->size);
    MCA_BTL_MVAPI_RDMA_MAKE_REMOTE(frag->ftr);
}

static void mca_btl_mvapi_send_frag_frag_constructor(mca_btl_mvapi_frag_t* frag) 
{ 
    
    frag->size = 0; 
    frag->type = MCA_BTL_MVAPI_FRAG_FRAG;
    frag->registration = NULL;
    mca_btl_mvapi_send_frag_common_constructor(frag); 
}


OBJ_CLASS_INSTANCE(
                   mca_btl_mvapi_frag_t, 
                   mca_btl_base_descriptor_t, 
                   NULL, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_mvapi_send_frag_eager_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_mvapi_send_frag_eager_constructor, 
                   NULL); 


OBJ_CLASS_INSTANCE(
                   mca_btl_mvapi_send_frag_max_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_mvapi_send_frag_max_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_mvapi_send_frag_frag_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_mvapi_send_frag_frag_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_mvapi_recv_frag_eager_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_mvapi_recv_frag_eager_constructor, 
                   NULL); 


OBJ_CLASS_INSTANCE(
                   mca_btl_mvapi_recv_frag_max_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_mvapi_recv_frag_max_constructor, 
                   NULL); 


