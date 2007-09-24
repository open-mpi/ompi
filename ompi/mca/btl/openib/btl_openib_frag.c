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
 * Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "btl_openib_frag.h" 
#include "btl_openib_eager_rdma.h"

void mca_btl_openib_frag_init(ompi_free_list_item_t* item, void* ctx) { 
    
    mca_btl_openib_frag_init_data_t* init_data =
        (mca_btl_openib_frag_init_data_t*) ctx;
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*) item;
    mca_btl_openib_reg_t* registration =
        (mca_btl_openib_reg_t*)frag->base.super.registration;
    
    frag->size = init_data->length;
    assert(init_data->order != 255);
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->type = init_data->type;
    frag->list = init_data->list;
    frag->qp_idx = init_data->order;
    
    frag->hdr = (mca_btl_openib_header_t*)frag->base.super.ptr;
    frag->segment.seg_addr.pval = ((unsigned char* )frag->hdr) + sizeof(mca_btl_openib_header_t);  
    
    if(registration) {    
        frag->registration = registration;
        frag->sg_entry.lkey = registration->mr->lkey;
        frag->segment.seg_key.key32[0] = frag->sg_entry.lkey;
    }
    
    /* init the segment address to start after the btl header */ 
    frag->segment.seg_len = frag->size;
    frag->sg_entry.addr = (unsigned long) frag->hdr; 
    frag->sg_entry.length = frag->size + sizeof(mca_btl_openib_header_t);
    frag->base.des_flags = 0; 

    return;
}


 
static void mca_btl_openib_send_frag_common_constructor(mca_btl_openib_frag_t* frag) 
{ 
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    

    frag->wr_desc.sr_desc.wr_id = (unsigned long) frag; 
    frag->wr_desc.sr_desc.sg_list = &frag->sg_entry; 
    frag->wr_desc.sr_desc.num_sge = 1; 
    frag->wr_desc.sr_desc.opcode = IBV_WR_SEND; 
    frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
    frag->wr_desc.sr_desc.next = NULL; 
}

static void mca_btl_openib_recv_frag_common_constructor(mca_btl_openib_frag_t* frag) 
{ 
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
   
    frag->wr_desc.rd_desc.wr_id = (unsigned long) frag; 
    frag->wr_desc.rd_desc.sg_list = &frag->sg_entry; 
    frag->wr_desc.rd_desc.num_sge = 1; 
    frag->wr_desc.rd_desc.next = NULL; 
}


static void mca_btl_openib_recv_user_frag_constructor(mca_btl_openib_frag_t* frag) 
{ 
    frag->registration = NULL;
    frag->hdr = (mca_btl_openib_header_t*)frag->base.super.ptr;
    frag->segment.seg_addr.pval = ((unsigned char* )frag->hdr) + sizeof(mca_btl_openib_header_t);  
    
    /* init the segment address to start after the btl header */ 
    frag->segment.seg_len = frag->size;
    frag->sg_entry.addr = (unsigned long) frag->hdr; 
    frag->sg_entry.length = frag->size + sizeof(mca_btl_openib_header_t);
    frag->base.des_flags = 0; 

    mca_btl_openib_recv_frag_common_constructor(frag); 
}


static void mca_btl_openib_send_user_frag_constructor(mca_btl_openib_frag_t* frag) 
{ 
    frag->registration = NULL;
    frag->hdr = (mca_btl_openib_header_t*)frag->base.super.ptr;
    frag->segment.seg_addr.pval = ((unsigned char* )frag->hdr) + sizeof(mca_btl_openib_header_t);  
    
    /* init the segment address to start after the btl header */ 
    frag->segment.seg_len = frag->size;
    frag->sg_entry.addr = (unsigned long) frag->hdr; 
    frag->sg_entry.length = frag->size + sizeof(mca_btl_openib_header_t);
    frag->base.des_flags = 0; 

    mca_btl_openib_send_frag_common_constructor(frag); 
}

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_frag_t, 
                   mca_btl_base_descriptor_t, 
                   NULL, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_send_frag_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_send_frag_common_constructor, 
                   NULL); 


OBJ_CLASS_INSTANCE(
                   mca_btl_openib_send_frag_control_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_send_frag_common_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_send_user_frag_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_send_user_frag_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_recv_user_frag_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_recv_user_frag_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_recv_frag_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_recv_frag_common_constructor, 
                   NULL); 

