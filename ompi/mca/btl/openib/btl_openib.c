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
#include <string.h>
#include <inttypes.h>
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "btl_openib.h"
#include "btl_openib_frag.h" 
#include "btl_openib_proc.h"
#include "btl_openib_endpoint.h"
#include "ompi/datatype/convertor.h" 
#include "ompi/datatype/datatype.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/mpool/openib/mpool_openib.h" 
#include <errno.h> 
#include <string.h> 
#include <math.h>
mca_btl_openib_module_t mca_btl_openib_module = {
    {
        &mca_btl_openib_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0,  /* TODO this should be PUT btl flags */
        mca_btl_openib_add_procs,
        mca_btl_openib_del_procs,
        mca_btl_openib_register, 
        mca_btl_openib_finalize,
        /* we need alloc free, pack */ 
        mca_btl_openib_alloc, 
        mca_btl_openib_free, 
        mca_btl_openib_prepare_src,
        mca_btl_openib_prepare_dst,
        mca_btl_openib_send,
        mca_btl_openib_put,
        mca_btl_openib_get,
        mca_btl_base_dump
    }
};



/* 
 *  add a proc to this btl module 
 *    creates an endpoint that is setup on the
 *    first send to the endpoint
 */ 
int mca_btl_openib_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*)btl;
    int i, rc;

    for(i = 0; i < (int) nprocs; i++) {
        
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_openib_proc_t* ib_proc;
        mca_btl_base_endpoint_t* ib_peer;

        if(NULL == (ib_proc = mca_btl_openib_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OPAL_THREAD_LOCK(&ib_proc->proc_lock);

        /* The btl_proc datastructure is shared by all IB PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the btl_proc.
         */
        ib_peer = OBJ_NEW(mca_btl_openib_endpoint_t);
        if(NULL == ib_peer) {
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ib_peer->endpoint_btl = openib_btl;
        ib_peer->subnet = openib_btl->port_info.subnet; 
        rc = mca_btl_openib_proc_insert(ib_proc, ib_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ib_peer);
            OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OPAL_THREAD_UNLOCK(&ib_proc->proc_lock);
        peers[i] = ib_peer;
    }

    do {
        int min_cq_size;
        int first_time = openib_btl->num_peers  == 0;
        int rc;
        openib_btl->num_peers += nprocs; 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
        if(mca_btl_openib_component.use_srq) { 
            openib_btl->rd_num = mca_btl_openib_component.rd_num + log2(nprocs) * mca_btl_openib_component.srq_rd_per_peer; 
            if(openib_btl->rd_num > mca_btl_openib_component.srq_rd_max)
                openib_btl->rd_num = mca_btl_openib_component.srq_rd_max;
            openib_btl->rd_low = openib_btl->rd_num - 1;
            min_cq_size = openib_btl->rd_num * 2 * openib_btl->num_peers;
            if(!first_time) { 
                struct ibv_srq_attr srq_attr;
                srq_attr.max_wr = openib_btl->rd_num;
                rc = ibv_modify_srq( openib_btl->srq_hp, &srq_attr, IBV_SRQ_MAX_WR); 
                if(rc) { 
                    BTL_ERROR(("cannot resize high priority shared receive queue, error: %d", rc));
                    return OMPI_ERROR;
                }
                rc = ibv_modify_srq(openib_btl->srq_lp, &srq_attr, IBV_SRQ_MAX_WR); 
                if(rc) { 
                    BTL_ERROR(("cannot resize low priority shared receive queue, error: %d", rc));
                    return OMPI_ERROR;
                }
                
            }
            
        } else 
#endif
        {
            min_cq_size = ( mca_btl_openib_component.rd_num >  (int32_t) mca_btl_openib_component.eager_rdma_num  ? 
                            mca_btl_openib_component.rd_num : (int32_t) mca_btl_openib_component.eager_rdma_num ) * 
                2 * openib_btl->num_peers;
            
        }

#ifdef OMPI_MCA_BTL_OPENIB_HAVE_RESIZE_CQ

        if(min_cq_size > (int32_t) mca_btl_openib_component.ib_cq_size) { 
            mca_btl_openib_component.ib_cq_size = min_cq_size;
            if(!first_time) { 
                rc = ibv_resize_cq(openib_btl->ib_cq_lp, min_cq_size);
                if(rc) {
                    BTL_ERROR(("cannot resize low priority completion queue, error: %d", rc));
                    return OMPI_ERROR;
                }
                rc = ibv_resize_cq(openib_btl->ib_cq_hp, min_cq_size); 
                if(rc) {
                    BTL_ERROR(("cannot resize high priority completion queue, error: %d", rc));
                    return OMPI_ERROR;
                }
            }
        }
#endif
        if(first_time) { 
            /* never been here before, setup cq and srq */
            mca_btl_openib_create_cq_srq(openib_btl); 
        }
    } while(0);
    
    return OMPI_SUCCESS;
        
}


/* 
 * delete the proc as reachable from this btl module 
 */
int mca_btl_openib_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    BTL_DEBUG(("TODO\n"));
    return OMPI_SUCCESS;
}

/* 
 *Register callback function to support send/recv semantics 
 */ 
int mca_btl_openib_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl; 
    

    OPAL_THREAD_LOCK(&openib_btl->ib_lock); 
    openib_btl->ib_reg[tag].cbfunc = cbfunc; 
    openib_btl->ib_reg[tag].cbdata = cbdata; 
    OPAL_THREAD_UNLOCK(&openib_btl->ib_lock); 
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 * 
 * When allocating a segment we pull a pre-alllocated segment 
 * from one of two free lists, an eager list and a max list
 */
mca_btl_base_descriptor_t* mca_btl_openib_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_openib_frag_t* frag = NULL;
    mca_btl_openib_module_t* openib_btl; 
    int rc;
    openib_btl = (mca_btl_openib_module_t*) btl; 
    
    if(size <= mca_btl_openib_component.eager_limit){ 
        MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc);
    } else if(size <= mca_btl_openib_component.max_send_size) { 
        MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc); 
    }
    
    if(NULL == frag)
        return NULL;

    frag->segment.seg_len = size <= openib_btl->super.btl_eager_limit ? size : openib_btl->super.btl_eager_limit;  
    frag->base.des_flags = 0; 
    
    return (mca_btl_base_descriptor_t*)frag;
}

/** 
 * Return a segment 
 * 
 * Return the segment to the appropriate 
 *  preallocated segment list 
 */ 
int mca_btl_openib_free(
                    struct mca_btl_base_module_t* btl, 
                    mca_btl_base_descriptor_t* des) 
{
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*)des; 

    if(frag->size == 0) {
        btl->btl_mpool->mpool_release(btl->btl_mpool, 
                                      (mca_mpool_base_registration_t*) 
                                      frag->openib_reg); 
    }
    MCA_BTL_IB_FRAG_RETURN(((mca_btl_openib_module_t*) btl), frag);
        
    return OMPI_SUCCESS; 
}


/**
 * register user buffer or pack 
 * data into pre-registered buffer and return a 
 * descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 *  
 * prepare source's behavior depends on the following: 
 * Has a valid memory registration been passed to prepare_src? 
 *    if so we attempt to use the pre-registred user-buffer, if the memory registration 
 *    is to small (only a portion of the user buffer) then we must reregister the user buffer 
 * Has the user requested the memory to be left pinned? 
 *    if so we insert the memory registration into a memory tree for later lookup, we 
 *    may also remove a previous registration if a MRU (most recently used) list of 
 *    registions is full, this prevents resources from being exhausted.
 * Is the requested size larger than the btl's max send size? 
 *    if so and we aren't asked to leave the registration pinned than we register the memory if 
 *    the users buffer is contiguous 
 * Otherwise we choose from two free lists of pre-registered memory in which to pack the data into. 
 * 
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration, 
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_btl_openib_module_t* openib_btl; 
    mca_btl_openib_frag_t* frag; 
    mca_mpool_openib_registration_t * openib_reg; 
    struct iovec iov; 
    uint32_t iov_count = 1; 
    size_t max_data = *size; 
    int32_t free_after; 
    int rc; 
    
    openib_btl = (mca_btl_openib_module_t*) btl; 
    openib_reg = (mca_mpool_openib_registration_t*) registration; 

    
    if(NULL != openib_reg &&  0 == ompi_convertor_need_buffers(convertor)){ 
        size_t reg_len; 

       /* the memory is already pinned and we have contiguous user data */ 

        MCA_BTL_IB_FRAG_ALLOC_FRAG(btl, frag, rc); 
        if(NULL == frag){
            return NULL; 
        } 
        
        iov.iov_len = max_data; 
        iov.iov_base = NULL; 
        
        ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after); 
                
        frag->segment.seg_len = max_data; 
        frag->segment.seg_addr.pval = iov.iov_base; 
        
        
        reg_len = (unsigned char*)openib_reg->base_reg.bound - (unsigned char*)iov.iov_base + 1; 
               
        frag->mr = openib_reg->mr; 
        frag->sg_entry.length = max_data; 
        frag->sg_entry.lkey = frag->mr->lkey; 
        
        frag->sg_entry.addr = (unsigned long) iov.iov_base;
        
        frag->segment.seg_key.key32[0] = (uint32_t) frag->sg_entry.lkey; 
        
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags = 0; 
        frag->openib_reg= openib_reg; 
        btl->btl_mpool->mpool_retain(btl->btl_mpool, (mca_mpool_base_registration_t*) openib_reg); 
        return &frag->base;
        
    } else if( max_data > btl->btl_max_send_size && 
               ompi_convertor_need_buffers(convertor) == 0 && 
               reserve == 0) {
        /* The user buffer is contigous and we are asked to send more than the max send size.  */            
        
        MCA_BTL_IB_FRAG_ALLOC_FRAG(openib_btl, frag, rc); 
        if(NULL == frag){
            return NULL; 
        } 
       
        iov.iov_len = max_data; 
        iov.iov_base = NULL; 
        
        ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after); 
        
        frag->segment.seg_len = max_data; 
        frag->segment.seg_addr.pval = iov.iov_base; 
        frag->base.des_flags = 0; 

        
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                                            iov.iov_base, 
                                            max_data, 
                                            0,
                                            (mca_mpool_base_registration_t**) &openib_reg); 
        if(OMPI_SUCCESS != rc || NULL == openib_reg) {
            MCA_BTL_IB_FRAG_RETURN(openib_btl, frag); 
            return NULL;
        }
        
        
        frag->mr = openib_reg->mr; 
        frag->sg_entry.length = max_data; 
        frag->sg_entry.lkey = openib_reg->mr->lkey;
        
        frag->sg_entry.addr = (unsigned long) iov.iov_base;
        
        frag->segment.seg_key.key32[0] = (uint32_t) frag->mr->rkey; 
            
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->openib_reg = openib_reg; 
        BTL_VERBOSE(("frag->sg_entry.lkey = %lu .addr = %llu", frag->sg_entry.lkey, frag->sg_entry.addr)); 

        return &frag->base;

    } else if (max_data+reserve <=  btl->btl_eager_limit) { 
        /* the data is small enough to fit in the eager frag and 
           either we received no prepinned memory or leave pinned is 
           not set
        */    
        MCA_BTL_IB_FRAG_ALLOC_EAGER(btl, frag, rc); 
        if(NULL == frag) { 
            return NULL; 
        } 
        
        iov.iov_len = max_data; 
        iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve; 
        
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after); 
        *size  = max_data; 
        if( rc < 0 ) { 
            MCA_BTL_IB_FRAG_RETURN(openib_btl, frag); 
            return NULL; 
        } 
        
        frag->segment.seg_len = max_data + reserve; 
        frag->segment.seg_key.key32[0] = (uint32_t) frag->sg_entry.lkey; 
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags = 0; 
        
        return &frag->base; 
        
    } else {
           
        MCA_BTL_IB_FRAG_ALLOC_MAX(btl, frag, rc); 
        if(NULL == frag) { 
            return NULL; 
        } 
        if(max_data + reserve > btl->btl_max_send_size){ 
            max_data = btl->btl_max_send_size - reserve; 
        }
        iov.iov_len = max_data; 
        iov.iov_base = (unsigned char*)frag->segment.seg_addr.pval + reserve; 
        
        rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, &free_after); 
        *size  = max_data; 

        if( rc < 0 ) { 
            MCA_BTL_IB_FRAG_RETURN(openib_btl, frag); 
            return NULL; 
        } 
        
        frag->segment.seg_len = max_data + reserve; 
        frag->segment.seg_key.key32[0] = (uint32_t) frag->sg_entry.lkey; 
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags=0; 
        
        return &frag->base; 
    }
    return NULL; 
}

/**
 * Prepare the dst buffer
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 * prepare dest's behavior depends on the following: 
 * Has a valid memory registration been passed to prepare_src? 
 *    if so we attempt to use the pre-registred user-buffer, if the memory registration 
 *    is to small (only a portion of the user buffer) then we must reregister the user buffer 
 * Has the user requested the memory to be left pinned? 
 *    if so we insert the memory registration into a memory tree for later lookup, we 
 *    may also remove a previous registration if a MRU (most recently used) list of 
 *    registions is full, this prevents resources from being exhausted.
 */
mca_btl_base_descriptor_t* mca_btl_openib_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration, 
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_openib_module_t* openib_btl; 
    mca_btl_openib_frag_t* frag; 
    mca_mpool_openib_registration_t * openib_reg; 
    int rc; 
    long lb;
    size_t reg_len; 

    openib_btl = (mca_btl_openib_module_t*) btl; 
    openib_reg = (mca_mpool_openib_registration_t*) registration; 
    
    MCA_BTL_IB_FRAG_ALLOC_FRAG(btl, frag, rc); 
    if(NULL == frag){
        return NULL; 
    }
    
    ompi_ddt_type_lb(convertor->pDesc, &lb);
    frag->segment.seg_len = *size; 
    frag->segment.seg_addr.pval = convertor->pBaseBuf + lb + convertor->bConverted; 
    frag->base.des_flags = 0; 

    if(NULL!= openib_reg){ 
        /* the memory is already pinned try to use it if the pinned region is large enough*/ 
        reg_len = (unsigned char*)openib_reg->base_reg.bound - (unsigned char*)frag->segment.seg_addr.pval + 1; 
        btl->btl_mpool->mpool_retain(btl->btl_mpool, 
                                     (mca_mpool_base_registration_t*) openib_reg); 
    }  else { 
        /* we didn't get a memory registration passed in, so we have to register the region
         * ourselves 
         */ 
        
        rc = btl->btl_mpool->mpool_register(btl->btl_mpool,
                                       frag->segment.seg_addr.pval,
                                       *size, 
                                       0,
                                       (mca_mpool_base_registration_t**) &openib_reg);
        if(OMPI_SUCCESS != rc || NULL == openib_reg) {
            MCA_BTL_IB_FRAG_RETURN(openib_btl, frag);
            return NULL;
        }
    }

    
    frag->mr = openib_reg->mr; 
    frag->sg_entry.length = *size; 
    frag->sg_entry.lkey = openib_reg->mr->lkey; 
    frag->sg_entry.addr = (unsigned long) frag->segment.seg_addr.pval; 
    
    frag->segment.seg_key.key32[0] = frag->mr->rkey; 
    
    frag->base.des_dst = &frag->segment; 
    frag->base.des_dst_cnt = 1; 
    frag->base.des_src = NULL; 
    frag->base.des_src_cnt = 0; 
    frag->openib_reg = openib_reg; 
    BTL_VERBOSE(("frag->sg_entry.lkey = %lu .addr = %llu frag->segment.seg_key.key32[0] = %lu" , frag->sg_entry.lkey, frag->sg_entry.addr, frag->segment.seg_key.key32[0])); 

    return &frag->base; 
    
}

int mca_btl_openib_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_openib_module_t* openib_btl; 
    openib_btl = (mca_btl_openib_module_t*) btl; 

#if 0 
    if(openib_btl->send_free_eager.fl_num_allocated != 
       openib_btl->send_free_eager.super.opal_list_length){ 
        opal_output(0, "btl ib send_free_eager frags: %d allocated %d returned \n", 
                    openib_btl->send_free_eager.fl_num_allocated, 
                    openib_btl->send_free_eager.super.opal_list_length); 
    }
    if(openib_btl->send_free_max.fl_num_allocated != 
      openib_btl->send_free_max.super.opal_list_length){ 
        opal_output(0, "btl ib send_free_max frags: %d allocated %d returned \n", 
                    openib_btl->send_free_max.fl_num_allocated, 
                    openib_btl->send_free_max.super.opal_list_length); 
    }
    if(openib_btl->send_free_frag.fl_num_allocated != 
       openib_btl->send_free_frag.super.opal_list_length){ 
        opal_output(0, "btl ib send_free_frag frags: %d allocated %d returned \n", 
                    openib_btl->send_free_frag.fl_num_allocated, 
                    openib_btl->send_free_frag.super.opal_list_length); 
    }
    
    if(openib_btl->recv_free_eager.fl_num_allocated != 
       openib_btl->recv_free_eager.super.opal_list_length){ 
        opal_output(0, "btl ib recv_free_eager frags: %d allocated %d returned \n", 
                    openib_btl->recv_free_eager.fl_num_allocated, 
                    openib_btl->recv_free_eager.super.opal_list_length); 
    }

    if(openib_btl->recv_free_max.fl_num_allocated != 
       openib_btl->recv_free_max.super.opal_list_length){ 
        opal_output(0, "btl ib recv_free_max frags: %d allocated %d returned \n", 
                    openib_btl->recv_free_max.fl_num_allocated, 
                    openib_btl->recv_free_max.super.opal_list_length); 
    }
#endif 

    return OMPI_SUCCESS;
}

/*
 *  Initiate a send. 
 */

int mca_btl_openib_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag)
   
{
    
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*)descriptor; 
    frag->endpoint = endpoint; 
    frag->hdr->tag = tag;
    frag->wr_desc.sr_desc.opcode = IBV_WR_SEND;
    return mca_btl_openib_endpoint_send(endpoint, frag);
}

/*
 * RDMA WRITE local buffer to remote buffer address.
 */

int mca_btl_openib_put( mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* endpoint,
                    mca_btl_base_descriptor_t* descriptor)
{
    int rc;
    struct ibv_send_wr* bad_wr; 
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*) descriptor; 
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;

    /* setup for queued requests */
    frag->endpoint = endpoint;
    frag->wr_desc.sr_desc.opcode = IBV_WR_RDMA_WRITE; 

    /* check for a send wqe */
    if (OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,-1) < 0) {

        OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->pending_frags_lp, (opal_list_item_t *)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return OMPI_SUCCESS;

    /* post descriptor */
    } else {
        
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
        frag->wr_desc.sr_desc.wr.rdma.remote_addr = (unsigned long) frag->base.des_dst->seg_addr.pval; 
        frag->wr_desc.sr_desc.wr.rdma.rkey = frag->base.des_dst->seg_key.key32[0]; 
        frag->sg_entry.addr = (unsigned long) frag->base.des_src->seg_addr.pval; 
        frag->sg_entry.length  = frag->base.des_src->seg_len; 
        
        if(ibv_post_send(endpoint->lcl_qp_lp, 
                         &frag->wr_desc.sr_desc, 
                         &bad_wr)){ 
            rc = OMPI_ERROR;
        } else {
            rc = OMPI_SUCCESS;
        }
    
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ 
        if(mca_btl_openib_component.use_srq) { 
            MCA_BTL_OPENIB_POST_SRR_HIGH(openib_btl, 1); 
            MCA_BTL_OPENIB_POST_SRR_LOW(openib_btl, 1);         
        } else { 
#endif 
            MCA_BTL_OPENIB_ENDPOINT_POST_RR_HIGH(endpoint, 1); 
            MCA_BTL_OPENIB_ENDPOINT_POST_RR_LOW(endpoint, 1); 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
        }
#endif 
    }
    return rc; 
}


/*
 * RDMA READ remote buffer to local buffer address.
 */

int mca_btl_openib_get( mca_btl_base_module_t* btl,
                    mca_btl_base_endpoint_t* endpoint,
                    mca_btl_base_descriptor_t* descriptor)
{
    int rc;
    struct ibv_send_wr* bad_wr; 
    mca_btl_openib_frag_t* frag = (mca_btl_openib_frag_t*) descriptor; 
    mca_btl_openib_module_t* openib_btl = (mca_btl_openib_module_t*) btl;
    frag->endpoint = endpoint;
    frag->wr_desc.sr_desc.opcode = IBV_WR_RDMA_READ; 

    /* check for a send wqe */
    if (OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,-1) < 0) {

        OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->pending_frags_lp, (opal_list_item_t *)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return OMPI_SUCCESS;

    /* check for a get token */
    } else if(OPAL_THREAD_ADD32(&endpoint->get_tokens,-1) < 0) {

        OPAL_THREAD_ADD32(&endpoint->sd_wqe_lp,1);
        OPAL_THREAD_ADD32(&endpoint->get_tokens,1);
        OPAL_THREAD_LOCK(&endpoint->endpoint_lock);
        opal_list_append(&endpoint->pending_frags_lp, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&endpoint->endpoint_lock);
        return OMPI_SUCCESS;

    } else { 
    
        frag->wr_desc.sr_desc.send_flags = IBV_SEND_SIGNALED; 
        frag->wr_desc.sr_desc.wr.rdma.remote_addr = (unsigned long) frag->base.des_src->seg_addr.pval; 
        frag->wr_desc.sr_desc.wr.rdma.rkey = frag->base.des_src->seg_key.key32[0]; 
        frag->sg_entry.addr = (unsigned long) frag->base.des_dst->seg_addr.pval; 
        frag->sg_entry.length  = frag->base.des_dst->seg_len; 
        
        if(ibv_post_send(endpoint->lcl_qp_lp, 
                         &frag->wr_desc.sr_desc, 
                         &bad_wr)){ 
            BTL_ERROR(("error posting send request errno (%d) says %s", errno, strerror(errno))); 
            rc = ORTE_ERROR;
        }  else {
            rc = ORTE_SUCCESS;
        }
        
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ 
        if(mca_btl_openib_component.use_srq) { 
            MCA_BTL_OPENIB_POST_SRR_HIGH(openib_btl, 1); 
            MCA_BTL_OPENIB_POST_SRR_LOW(openib_btl, 1);         
        } else { 
#endif 
            MCA_BTL_OPENIB_ENDPOINT_POST_RR_HIGH(endpoint, 1); 
            MCA_BTL_OPENIB_ENDPOINT_POST_RR_LOW(endpoint, 1); 
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
        }
#endif 
    }
    return rc; 
}

/* 
 * create both the high and low priority completion queues 
 * and the shared receive queue (if requested)
 */ 
int mca_btl_openib_create_cq_srq(mca_btl_openib_module_t *openib_btl)
{
    /* Allocate Protection Domain */ 
    openib_btl->poll_cq = false; 
    
#ifdef OMPI_MCA_BTL_OPENIB_HAVE_SRQ
    if(mca_btl_openib_component.use_srq) { 
        
        struct ibv_srq_init_attr attr; 
        attr.attr.max_wr = mca_btl_openib_component.srq_rd_max;
        attr.attr.max_sge = mca_btl_openib_component.ib_sg_list_size;

        openib_btl->srd_posted_hp = 0; 
        openib_btl->srd_posted_lp = 0; 
        
        openib_btl->srq_hp = ibv_create_srq(openib_btl->hca->ib_pd, &attr); 
        if(NULL == openib_btl->srq_hp) { 
            BTL_ERROR(("error in ibv_create_srq\n")); 
            return OMPI_ERROR; 
        }
        
        openib_btl->srq_lp = ibv_create_srq(openib_btl->hca->ib_pd, &attr); 
        if(NULL == openib_btl->srq_hp) { 
            BTL_ERROR(("error in ibv_create_srq\n")); 
            return OMPI_ERROR; 
        }
        
    } else { 
        openib_btl->srq_hp = NULL; 
        openib_btl->srq_lp = NULL;
    } 
#endif
    
    /* Create the low and high priority queue pairs */ 
#if OMPI_MCA_BTL_OPENIB_IBV_CREATE_CQ_ARGS == 3
    openib_btl->ib_cq_lp = 
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL); 
#else
    openib_btl->ib_cq_lp = 
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL, NULL, 0); 
#endif
    
    if(NULL == openib_btl->ib_cq_lp) {
        BTL_ERROR(("error creating low priority cq for %s errno says %s\n",
                  ibv_get_device_name(openib_btl->hca->ib_dev), 
                  strerror(errno))); 
        return OMPI_ERROR;
    }

#if OMPI_MCA_BTL_OPENIB_IBV_CREATE_CQ_ARGS == 3
    openib_btl->ib_cq_hp = 
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL); 
#else
    openib_btl->ib_cq_hp = 
        ibv_create_cq(openib_btl->hca->ib_dev_context,
                mca_btl_openib_component.ib_cq_size, NULL, NULL, 0); 
#endif    

    if(NULL == openib_btl->ib_cq_hp) {
        BTL_ERROR(("error creating high priority cq for %s errno says %s\n", 
                  ibv_get_device_name(openib_btl->hca->ib_dev), 
                  strerror(errno))); 
        return OMPI_ERROR;
    }
        
    
    return OMPI_SUCCESS;
}
