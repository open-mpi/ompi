#include "btl_openib_frag.h" 
#include "mca/mpool/openib/mpool_openib.h" 



static void mca_btl_openib_frag_common_constructor( mca_btl_openib_frag_t* frag) 
{
    mca_mpool_openib_registration_t* registration = 
        (mca_mpool_openib_registration_t*) frag->base.super.user_data;
    
    frag->hdr = (mca_btl_openib_header_t*) (frag+1);    /* initialize the btl header to point to start at end of frag */ 
#if 0   
    mod = (unsigned long) frag->hdr % MCA_BTL_IB_FRAG_ALIGN; 
    
    if(mod != 0) {
        frag->hdr = (mca_btl_openib_header_t*) ((unsigned char*) frag->hdr + (MCA_BTL_IB_FRAG_ALIGN - mod));
    }
#endif 
    
    frag->segment.seg_addr.pval = ((unsigned char* )frag->hdr) + sizeof(mca_btl_openib_header_t);  /* init the segment address to start after the btl header */ 
    
#if 0 
    mod = (frag->segment.seg_addr.lval) % MCA_BTL_IB_FRAG_ALIGN; 
    if(mod != 0) {
        frag->segment.seg_addr.lval += (MCA_BTL_IB_FRAG_ALIGN - mod);
    }
#endif 
    
    frag->mr = registration->mr; 
    frag->segment.seg_len = frag->size;
    frag->segment.seg_key.key32[0] = (uint32_t) frag->mr->lkey; 
    frag->sg_entry.addr = (uintprt_t) frag->hdr; 
    frag->sg_entry.length = frag->size; 
    frag->sg_entry.lkey = frag->mr->lkey; 
    frag->base.des_flags = 0; 
}

 
static void mca_btl_openib_send_frag_common_constructor(mca_btl_openib_frag_t* frag) 
{ 
    
    mca_btl_openib_frag_common_constructor(frag); 
    frag->base.des_src = &frag->segment;
    frag->base.des_src_cnt = 1;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    
    frag->sr_desc.wr_id = frag; 
    frag->sr_desc.sg_list = &frag->sg_entry; 
    frag->sr_desc.num_sge = 1; 
    frag->sr_desc.opcode = IBV_WR_SEND; 
    frag->sr_desc.send_flags = IBV_SEND_SIGNALED; 
}

static void mca_btl_openib_recv_frag_common_constructor(mca_btl_openib_frag_t* frag) 
{ 
    
    mca_btl_openib_frag_common_constructor(frag); 
    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    
    frag->rr_desc.wr_id = frag; 
    frag->rr_desc.sg_list = &frag->sg_entry; 
    frag->rr_desc.num_sge = 1; 
}

static void mca_btl_openib_send_frag_eager_constructor(mca_btl_openib_frag_t* frag) 
{ 
    
    frag->size = mca_btl_openib_component.eager_limit;  
    mca_btl_openib_send_frag_common_constructor(frag); 
}


static void mca_btl_openib_send_frag_max_constructor(mca_btl_openib_frag_t* frag) 
{ 
    
    frag->size = mca_btl_openib_component.max_send_size; 
    mca_btl_openib_send_frag_common_constructor(frag); 
}

static void mca_btl_openib_recv_frag_max_constructor(mca_btl_openib_frag_t* frag) 
{
    frag->size = mca_btl_openib_component.max_send_size; 
    mca_btl_openib_recv_frag_common_constructor(frag); 
    
}


static void mca_btl_openib_recv_frag_eager_constructor(mca_btl_openib_frag_t* frag) 
{
    frag->size = mca_btl_openib_component.eager_limit; 
    mca_btl_openib_recv_frag_common_constructor(frag); 
    
}

static void mca_btl_openib_send_frag_frag_constructor(mca_btl_openib_frag_t* frag) 
{ 
    
    frag->size = 0; 
    mca_btl_openib_send_frag_common_constructor(frag); 
}


OBJ_CLASS_INSTANCE(
                   mca_btl_openib_frag_t, 
                   mca_btl_base_descriptor_t, 
                   NULL, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_send_frag_eager_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_send_frag_eager_constructor, 
                   NULL); 


OBJ_CLASS_INSTANCE(
                   mca_btl_openib_send_frag_max_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_send_frag_max_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_send_frag_frag_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_send_frag_frag_constructor, 
                   NULL); 

OBJ_CLASS_INSTANCE(
                   mca_btl_openib_recv_frag_eager_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_recv_frag_eager_constructor, 
                   NULL); 


OBJ_CLASS_INSTANCE(
                   mca_btl_openib_recv_frag_max_t, 
                   mca_btl_base_descriptor_t, 
                   mca_btl_openib_recv_frag_max_constructor, 
                   NULL); 


