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
#include <string.h>
#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"

#include "bmi_ib.h"
#include "bmi_ib_frag.h" 
#include "bmi_ib_proc.h"
#include "bmi_ib_endpoint.h"
#include "datatype/convertor.h" 
#include "mca/common/vapi/vapi_mem_reg.h" 

mca_bmi_ib_module_t mca_bmi_ib_module = {
    {
        &mca_bmi_ib_component.super,
        0, /* max size of first fragment */
        0, /* min send fragment size */
        0, /* max send fragment size */
        0, /* min rdma fragment size */
        0, /* max rdma fragment size */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0,  /* TODO this should be PUT bmi flags */
        mca_bmi_ib_add_procs,
        mca_bmi_ib_del_procs,
        mca_bmi_ib_register, 
        mca_bmi_ib_finalize,
        /* we need alloc free, pack */ 
        mca_bmi_ib_alloc, 
        mca_bmi_ib_free, 
        mca_bmi_ib_prepare_src,
        mca_bmi_ib_prepare_dst,
        mca_bmi_ib_send,
        mca_bmi_ib_put,
        NULL /* get */ 
    }
};

int mca_bmi_ib_add_procs(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_bmi_base_endpoint_t** peers, 
    ompi_bitmap_t* reachable)
{
    mca_bmi_ib_module_t* ib_bmi = (mca_bmi_ib_module_t*)bmi;
    int i, rc;

    for(i = 0; i < nprocs; i++) {

        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_bmi_ib_proc_t* ib_proc;
        mca_bmi_base_endpoint_t* ib_peer;

        if(NULL == (ib_proc = mca_bmi_ib_proc_create(ompi_proc))) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        /*
         * Check to make sure that the peer has at least as many interface 
         * addresses exported as we are trying to use. If not, then 
         * don't bind this PTL instance to the proc.
         */

        OMPI_THREAD_LOCK(&ib_proc->proc_lock);

        /* The bmi_proc datastructure is shared by all IB PTL
         * instances that are trying to reach this destination. 
         * Cache the peer instance on the bmi_proc.
         */
        ib_peer = OBJ_NEW(mca_bmi_ib_endpoint_t);
        if(NULL == ib_peer) {
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        ib_peer->endpoint_bmi = ib_bmi;
        rc = mca_bmi_ib_proc_insert(ib_proc, ib_peer);
        if(rc != OMPI_SUCCESS) {
            OBJ_RELEASE(ib_peer);
            OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
            continue;
        }

        ompi_bitmap_set_bit(reachable, i);
        OMPI_THREAD_UNLOCK(&module_proc->proc_lock);
        peers[i] = ib_peer;
    }

    return OMPI_SUCCESS;
}

int mca_bmi_ib_del_procs(struct mca_bmi_base_module_t* bmi, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_bmi_base_endpoint_t ** peers)
{
    /* Stub */
    DEBUG_OUT("Stub\n");
    return OMPI_SUCCESS;
}

int mca_bmi_ib_register(
                        struct mca_bmi_base_module_t* bmi, 
                        mca_bmi_base_tag_t tag, 
                        mca_bmi_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    /* TODO add register stuff here... */ 
    mca_bmi_ib_module_t* ib_bmi = (mca_bmi_ib_module_t*) bmi; 
    ib_bmi->ib_reg[tag].cbfunc = cbfunc; 
    ib_bmi->ib_reg[tag].cbdata = cbdata; 
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */
mca_bmi_base_descriptor_t* mca_bmi_ib_alloc(
    struct mca_bmi_base_module_t* bmi,
    size_t size)
{
    mca_bmi_ib_frag_t* frag;
    mca_bmi_ib_module_t* ib_bmi; 
    int rc;
    void * user_out; 
    ib_bmi = (mca_bmi_ib_module_t*) bmi; 
    
   /*  if(size <= ib_bmi->super.bmi_eager_limit){ */
    
   
    if(size <= mca_bmi_ib_component.eager_limit){ 
        MCA_BMI_IB_FRAG_ALLOC_EAGER(bmi, frag, rc); 
        frag->segment.seg_len = 
            size <= mca_bmi_ib_component.eager_limit ? 
            size: mca_bmi_ib_component.eager_limit ; 
    } else { 
        MCA_BMI_IB_FRAG_ALLOC_MAX(bmi, frag, rc); 
        frag->segment.seg_len = 
            size <= mca_bmi_ib_component.max_send_size ? 
            size: mca_bmi_ib_component.max_send_size ; 
    }
    
    
    /* } else {  */
        
        /*   frag = (mca_bmi_ib_frag_t*) ib_bmi->ib_pool->mpool_alloc(ib_bmi->ib_pool,  sizeof(frag) + sizeof(mca_bmi_ib_header_t) + size ,0, &user_out);  */
        /*         frag->base.super.user_data = user_out;  */
        /*         OBJ_CONSTRUCT(frag, mca_bmi_ib_frag_t);  */
    
    /*  }  */
    
    frag->segment.seg_len = size <= ib_bmi->super.bmi_eager_limit ? size : ib_bmi->super.bmi_eager_limit;  
    return (mca_bmi_base_descriptor_t*)frag;
}

int mca_bmi_ib_free(
                           struct mca_bmi_base_module_t* bmi, 
                           mca_bmi_base_descriptor_t* des) 
{
    mca_bmi_ib_frag_t* frag = (mca_bmi_ib_frag_t*)des; 
    
    if(frag->size == 0) {
        MCA_BMI_IB_FRAG_RETURN_FRAG(bmi, frag); 
    } else if(frag->size == mca_bmi_ib_component.max_send_size){ 
        MCA_BMI_IB_FRAG_RETURN_MAX(bmi, frag); 
    } else if(frag->size == mca_bmi_ib_component.eager_limit){ 
        MCA_BMI_IB_FRAG_RETURN_EAGER(bmi, frag); 
    } 
    return frag->rc; 
}

/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
mca_bmi_base_descriptor_t* mca_bmi_ib_prepare_src(
    struct mca_bmi_base_module_t* bmi,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size
)
{
    mca_bmi_ib_module_t* ib_bmi; 
    mca_bmi_ib_frag_t* frag; 
    struct iovec iov; 
    int32_t out_size = 0; 
    size_t max_data = *size; 
    int32_t free_after; 
    int rc; 
    void* user_out; 

    ib_bmi = (mca_bmi_ib_module_t*) bmi; 
    
    if(  max_data+reserve <=  bmi->bmi_eager_limit) { 
        MCA_BMI_IB_FRAG_ALLOC_EAGER(bmi, frag, rc); 
        if(NULL == frag) { 
            return NULL; 
        } 
        if(max_data + reserve > frag->size){ 
            max_data = frag->size - reserve; 
        } 
        iov.iov_len = max_data; 
        iov.iov_base = frag->segment.seg_addr.pval + reserve; 
        
        rc = ompi_convertor_pack(convertor, &iov, &out_size, &max_data, &free_after); 
        if( rc < 0 ) { 
            MCA_BMI_IB_FRAG_RETURN_EAGER(bmi, frag); 
            return NULL; 
        } 
        
        frag->segment.seg_len = max_data + reserve; 
        *size  = max_data; 
        return &frag->base; 
        
    }else if( max_data + reserve <= ib_bmi->ib_pin_min || 1 == ompi_convertor_need_buffers( convertor) ){ 
        MCA_BMI_IB_FRAG_ALLOC_MAX(bmi, frag, rc); 
        if(NULL == frag) { 
            return NULL; 
        } 
        if(max_data + reserve > frag->size){ 
            max_data = frag->size - reserve; 
        }
        iov.iov_len = max_data; 
        iov.iov_base = frag->segment.seg_addr.pval + reserve; 
        
        rc = ompi_convertor_pack(convertor, &iov, &out_size, &max_data, &free_after); 
        if( rc < 0 ) { 
            MCA_BMI_IB_FRAG_RETURN_MAX(bmi, frag); 
            return NULL; 
        } 
        
        frag->segment.seg_len = max_data + reserve; 
        *size  = max_data; 
        return &frag->base; 
    } else { 
          VAPI_mrw_t mr_in, mr_out;
          VAPI_ret_t ret;   
          mca_common_vapi_memhandle_t mem_hndl; 
          
          memset(&mr_in, 0, sizeof(VAPI_mrw_t)); 
          memset(&mr_out, 0, sizeof(VAPI_mrw_t)); 
          memset(&mem_hndl, 0, sizeof(mca_common_vapi_memhandle_t)); 

          mem_hndl.hndl = VAPI_INVAL_HNDL; 
          

          mr_in.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE;
          mr_in.l_key = 0;
          mr_in.r_key = 0;
          mr_in.pd_hndl = ib_bmi->ptag; 
          mr_in.type = VAPI_MR;
          
          
          frag = (mca_bmi_ib_send_frag_frag_t*) ib_bmi->ib_pool->mpool_alloc(ib_bmi->ib_pool,  sizeof(frag) + sizeof(mca_bmi_ib_header_t) + *size ,0, &user_out);
          frag->base.super.user_data = user_out;
          OBJ_CONSTRUCT(frag, mca_bmi_ib_send_frag_frag_t);
          iov.iov_len = max_data; 
          iov.iov_base = NULL; 
          
          ompi_convertor_pack(convertor, &iov, &out_size, &max_data, &free_after); 
          frag->segment.seg_len = max_data; 
          frag->segment.seg_addr.pval = iov.iov_base; 
  
          mr_in.size = max_data;
          mr_in.start = (VAPI_virt_addr_t) (MT_virt_addr_t) iov.iov_base;
          
          ret = VAPI_register_mr(
                                 ib_bmi->nic, 
                                 &mr_in, 
                                 &mem_hndl.hndl, 
                                 &mr_out
                                 ); 
          
          if(VAPI_OK != ret){ 
              ompi_output(0, "error pinning vapi memory\n"); 
              return NULL; 
          }
          
          mem_hndl.l_key = mr_out.l_key; 
          mem_hndl.r_key = mr_out.r_key; 
  
          return &frag->base; 

    }
    
    
    return NULL; 
}

/**
 * Pack data
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
mca_bmi_base_descriptor_t* mca_bmi_ib_prepare_dst(
    struct mca_bmi_base_module_t* bmi,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    return NULL;
}

int mca_bmi_ib_finalize(struct mca_bmi_base_module_t* bmi)
{
    return OMPI_SUCCESS;
}

/*
 *  Initiate a send. If this is the first fragment, use the fragment
 *  descriptor allocated with the send requests, otherwise obtain
 *  one from the free list. Initialize the fragment and foward
 *  on to the peer.
 */

int mca_bmi_ib_send( 
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor, 
    mca_bmi_base_tag_t tag)
   
{
    
    mca_bmi_ib_frag_t* frag = (mca_bmi_ib_frag_t*)descriptor; 
    frag->endpoint = endpoint; 
        
    frag->hdr->tag = tag; 
    frag->type = MCA_BMI_IB_FRAG_SEND; 
    frag->rc = mca_bmi_ib_endpoint_send(endpoint, frag);
           
    return frag->rc;
}

/*
 * RDMA local buffer to remote buffer address.
 */

int mca_bmi_ib_put( mca_bmi_base_module_t* bmi,
                    mca_bmi_base_endpoint_t* bmi_peer,
                    mca_bmi_base_descriptor_t* descriptor)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}



/*
 * Asynchronous event handler to detect unforseen
 * events. Usually, such events are catastrophic.
 * Should have a robust mechanism to handle these
 * events and abort the OMPI application if necessary.
 *
 */
static void async_event_handler(VAPI_hca_hndl_t hca_hndl,
        VAPI_event_record_t * event_p,
        void *priv_data)
{
    switch (event_p->type) {
        case VAPI_QP_PATH_MIGRATED:
        case VAPI_EEC_PATH_MIGRATED:
        case VAPI_QP_COMM_ESTABLISHED:
        case VAPI_EEC_COMM_ESTABLISHED:
        case VAPI_SEND_QUEUE_DRAINED:
        case VAPI_PORT_ACTIVE:
            {
                DEBUG_OUT("Got an asynchronous event: %s\n",
                        VAPI_event_record_sym(event_p->type));
                break;
            }
        case VAPI_CQ_ERROR:
        case VAPI_LOCAL_WQ_INV_REQUEST_ERROR:
        case VAPI_LOCAL_WQ_ACCESS_VIOL_ERROR:
        case VAPI_LOCAL_WQ_CATASTROPHIC_ERROR:
        case VAPI_PATH_MIG_REQ_ERROR:
        case VAPI_LOCAL_EEC_CATASTROPHIC_ERROR:
        case VAPI_LOCAL_CATASTROPHIC_ERROR:
        case VAPI_PORT_ERROR:
            {
                ompi_output(0, "Got an asynchronous event: %s (%s)",
                        VAPI_event_record_sym(event_p->type),
                        VAPI_event_syndrome_sym(event_p->
                            syndrome));
                break;
            }
        default:
            ompi_output(0, "Warning!! Got an undefined "
                    "asynchronous event\n");
    }

}




int mca_bmi_ib_module_init(mca_bmi_ib_module_t *ib_bmi)
{

    /* Allocate Protection Domain */ 
    VAPI_ret_t ret;
    uint32_t cqe_cnt = 0;
      
    ret = VAPI_alloc_pd(ib_bmi->nic, &ib_bmi->ptag);

    if(ret != VAPI_OK) {
        MCA_BMI_IB_VAPI_ERROR(ret, "VAPI_alloc_pd");
        return OMPI_ERROR;
    }
    
    ret = VAPI_create_cq(ib_bmi->nic, ib_bmi->ib_cq_size,
            &ib_bmi->cq_hndl, &cqe_cnt);

    if( VAPI_OK != ret) {  
        MCA_BMI_IB_VAPI_ERROR(ret, "VAPI_create_cq");
        return OMPI_ERROR;
    }
    if(cqe_cnt <= 0) { 
        ompi_output(0, "%s: error creating completion queue ", __func__); 
        return OMPI_ERROR; 
    } 

    ret = EVAPI_set_async_event_handler(ib_bmi->nic,
            async_event_handler, 0, &ib_bmi->async_handler);

    if(VAPI_OK != ret) {
        MCA_BMI_IB_VAPI_ERROR(ret, "EVAPI_set_async_event_handler");
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
}
