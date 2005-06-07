#include "bmi_ib_frag.h" 
#include "mca/common/vapi/vapi_mem_reg.h"


static void mca_bmi_ib_send_frag_constructor(mca_bmi_ib_frag_t* frag) 
{ 
    mca_common_vapi_memhandle_t* mem_hndl = frag->base.super.user_data; 
    frag->size = MCA_BMI_IB_FIRST_FRAG_SIZE;  
    frag->hdr = (mca_bmi_ib_header_t*) (frag+1);    /* initialize the bmi header to point to start at end of frag */ 
    frag->segment.seg_addr.pval = frag->hdr+1;  /* init the segment address to start after the bmi header */ 
    frag->segment.seg_len = frag->size;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
    frag->base.des_src->seg_key.key64 = (uint64_t) mem_hndl->l_key;  
    /* TODO  - initialize the firstfrag size */ 
    frag->sr_desc.comp_type = VAPI_SIGNALED; 
    frag->sr_desc.opcode = VAPI_SEND; 
    frag->sr_desc.remote_qkey = 0; 
    frag->sr_desc.sg_lst_len = 1; 
    frag->sr_desc.sg_lst_p = &frag->sg_entry; 
    frag->sg_entry.lkey = mem_hndl->l_key;
    
    frag->sr_desc.id = (VAPI_virt_addr_t) (MT_virt_addr_t) frag; 
    frag->sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t) frag->hdr; 
    
}

static void mca_bmi_ib_recv_frag_constructor(mca_bmi_ib_frag_t* frag) 
{ 
    
    mca_common_vapi_memhandle_t* mem_hndl = frag->base.super.user_data; 
    frag->size = MCA_BMI_IB_FIRST_FRAG_SIZE; 
    frag->hdr = (mca_bmi_ib_header_t*) (frag+1);    /* initialize the bmi header to point to start at end of frag */ 
    frag->segment.seg_addr.pval = frag->hdr+1;  /* init the segment address to start after the bmi header */ 
    frag->segment.seg_len = frag->size;
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = 0;
    
    frag->base.des_src->seg_key.key64 = (uint64_t) mem_hndl->l_key;  
    /* TODO - initialize the first frag size */ 
    frag->rr_desc.comp_type = VAPI_SIGNALED; 
    frag->rr_desc.opcode = VAPI_RECEIVE; 
    frag->rr_desc.sg_lst_len = 1; 
    frag->rr_desc.sg_lst_p = &frag->sg_entry; 
    frag->sg_entry.lkey = mem_hndl->l_key; 
   
    frag->rr_desc.id = (VAPI_virt_addr_t) (MT_virt_addr_t) frag; 
    frag->sg_entry.addr = (VAPI_virt_addr_t) (MT_virt_addr_t) frag->hdr; 
   
}



OBJ_CLASS_INSTANCE(
                   mca_bmi_ib_frag_t, 
                   mca_bmi_base_descriptor_t, 
                   NULL, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_bmi_ib_send_frag_t, 
                   mca_bmi_base_descriptor_t, 
                   mca_bmi_ib_send_frag_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_bmi_ib_recv_frag_t, 
                   mca_bmi_base_descriptor_t, 
                   mca_bmi_ib_recv_frag_constructor, 
                   NULL); 


