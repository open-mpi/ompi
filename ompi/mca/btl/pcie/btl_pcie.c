/*
 * Copyright (c) 2009      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <sched.h>

#include "opal/types.h"
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "opal/sys/atomic.h"
#include "opal/mca/paffinity/paffinity.h"

#include "opal/datatype/opal_convertor.h" 
#include "ompi/datatype/ompi_datatype.h" 
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/mca/mpool/mpool.h" 
#include "ompi/mca/mpool/base/base.h" 
#include "ompi/mca/pml/pml.h"

#include "btl_pcie.h"
#include "btl_pcie_frag.h" 
#include "btl_pcie_proc.h"
#include "btl_pcie_endpoint.h"
#include "btl_pcie_ddriver.h"

mca_btl_pcie_module_t mca_btl_pcie_module = {
    {
        &mca_btl_pcie_component.super,
        "unknown",
        0, /* max size of first fragment */
        0, /* Threshold below which BTL should not fragment */
        0, /* max send fragment size */
        0, /* pipeline protocol length */
        0, /* max rdma fragment size */
        0, /* min packet size for pipeline protocol */
        0, /* exclusivity */
        0, /* latency */
        0, /* bandwidth */
        0, /* flags */
        mca_btl_pcie_add_procs,
        mca_btl_pcie_del_procs,
        mca_btl_pcie_register,
        mca_btl_pcie_finalize,
        mca_btl_pcie_alloc, 
        mca_btl_pcie_free, 
        mca_btl_pcie_prepare_src,
        mca_btl_pcie_prepare_dst,
        mca_btl_pcie_send,
        NULL, /* send immediate */
        mca_btl_pcie_put, /* put */ 
        NULL, /* get */ 
        mca_btl_base_dump, /*dump */
        NULL, /* mpool */
        NULL, /* register error cb */
        NULL  /* ft event */
    }
};

                               
/**
 *
 */

int mca_btl_pcie_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **ompi_procs, 
    struct mca_btl_base_endpoint_t** peers, 
    opal_bitmap_t* reachable)
{
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*)btl;
    int i;

    for(i = 0; i < (int) nprocs; i++) {
        struct ompi_proc_t* ompi_proc = ompi_procs[i];
        mca_btl_pcie_proc_t* pcie_proc;
        int rc;

        /* Don't connect to anyone on our local node, including
           ourselves.  The PCIe doesn't work that way, and the mapper
           sometimes gets confused by that fact. */
        if (OPAL_PROC_ON_LOCAL_NODE(ompi_proc->proc_flags)) continue;

        rc = mca_btl_pcie_proc_create(ompi_proc, pcie_btl, &pcie_proc);
        if(OMPI_SUCCESS != rc) { 
            return rc;
        } else if (pcie_proc) { 
            opal_bitmap_set_bit(reachable, i);
            peers[i] = pcie_proc->endpoint_proc;
        }
    }
    
    return OMPI_SUCCESS;
}

int mca_btl_pcie_del_procs(struct mca_btl_base_module_t* btl, 
        size_t nprocs, 
        struct ompi_proc_t **procs, 
        struct mca_btl_base_endpoint_t ** peers)
{
    /* TODO */
    return OMPI_SUCCESS;
}


/**
 * Register callback function to support send/recv semantics
 */

int mca_btl_pcie_register(
                        struct mca_btl_base_module_t* btl, 
                        mca_btl_base_tag_t tag, 
                        mca_btl_base_module_recv_cb_fn_t cbfunc, 
                        void* cbdata)
{
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl; 
    pcie_btl->pcie_reg[tag].cbfunc = cbfunc; 
    pcie_btl->pcie_reg[tag].cbdata = cbdata; 
    return OMPI_SUCCESS;
}


/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */

mca_btl_base_descriptor_t* mca_btl_pcie_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl; 
    mca_btl_pcie_frag_t* frag = NULL;
    int rc;

    if (size <= btl->btl_eager_limit) { 
        MCA_BTL_PCIE_FRAG_ALLOC_EAGER(pcie_btl, frag, rc);
        if (frag) { 
            frag->segment.seg_len = size;
            frag->base.des_flags = 0; 
            frag->hdr->length = size;
        }
    }
    if (NULL == frag && size <= btl->btl_max_send_size) {
        MCA_BTL_PCIE_FRAG_ALLOC_MAX(pcie_btl, frag, rc); 
        if (frag) { 
            frag->segment.seg_len = size;
            frag->base.des_flags = 0;
            frag->hdr->length = size;
        }
    }
    BTL_VERBOSE(("btl_pcie_alloc called for %lu bytes, returning 0x%lx", (unsigned long)size, (long)frag));
    
    return (mca_btl_base_descriptor_t*) frag;
}


/**
 * Return a segment
 */

int mca_btl_pcie_free(
    struct mca_btl_base_module_t* btl, 
    mca_btl_base_descriptor_t* des) 
{
    mca_btl_pcie_frag_t* frag = (mca_btl_pcie_frag_t*)des; 
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl;
    int ret;

    BTL_VERBOSE(("btl_pcie_free returning 0x%lx", (long)frag));
    
    if (frag->registration != NULL) { 
        pcie_btl->rdma_mpool->mpool_deregister(pcie_btl->rdma_mpool, 
					       (mca_mpool_base_registration_t*) 
					       frag->registration);
        frag->registration = NULL;
    }

    MCA_BTL_PCIE_FRAG_RETURN(pcie_btl, frag, ret);
    return ret;
}


/**
 * Pack data and return a descriptor that can be
 * used for send/put.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
mca_btl_base_descriptor_t* mca_btl_pcie_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags
)
{
    mca_btl_pcie_frag_t* frag = NULL;
    mca_btl_pcie_reg_t* pcie_reg;
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    BTL_VERBOSE(("btl_pcie_prepare_src called with reserve %lu", (unsigned long)reserve));

    /* check and see if the data is contiguous */ 
    if(opal_convertor_need_buffers(convertor) == false && 0 == reserve) { 
       MCA_BTL_PCIE_FRAG_ALLOC_DMA(btl, frag, rc);
        if(NULL == frag) {
            return NULL;
        }
        
        iov.iov_len = max_data;
        iov.iov_base = NULL;
        
        /* get the user buffer's address */
        opal_convertor_pack(convertor, &iov, &iov_count, &max_data);
        *size = max_data;
        
        if(NULL == registration) { 
            rc = pcie_btl->rdma_mpool->mpool_register(pcie_btl->rdma_mpool,
						      iov.iov_base, max_data, 0, &registration);
            if(OMPI_SUCCESS != rc || NULL == registration){ 
                MCA_BTL_PCIE_FRAG_RETURN(pcie_btl, frag, rc);
                return NULL;
            }
            frag->registration = (mca_btl_pcie_reg_t*) registration;
        }
        
        pcie_reg = (mca_btl_pcie_reg_t*) registration;
        frag->base.des_flags = 0;
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags = 0;
        
        frag->segment.seg_len = max_data;
        frag->segment.seg_addr.pval = iov.iov_base;
        frag->segment.seg_key.key64 = (uint64_t)pcie_reg->handle;

        BTL_VERBOSE(("prepare_src: frag->segment.seg_len = %lu .seg_addr.pval= %lu "
		   "frag->segment.seg_key.key64 = %lu",
		   (unsigned long)frag->segment.seg_len, (unsigned long)frag->segment.seg_addr.pval,
		   (unsigned long)frag->segment.seg_key.key64));

        return &frag->base;
        
    } else { 
        /*
         * if we aren't pinning the data and the requested size is less
         * than the eager limit pack into a fragment from the eager pool
         */
        if (max_data+reserve <= btl->btl_eager_limit) {
            
            MCA_BTL_PCIE_FRAG_ALLOC_EAGER(btl, frag, rc);
            if(NULL == frag) {
                return NULL;
            }
            
            iov.iov_len = max_data;
            iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
            
            rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
            *size  = max_data;
            if( rc < 0 ) {
                MCA_BTL_PCIE_FRAG_RETURN(btl, frag, rc);
                return NULL;
            }
            frag->segment.seg_len = max_data + reserve;
        }
        
        /* 
         * otherwise pack as much data as we can into a fragment
         * that is the max send size.
         */
        else {
            
            MCA_BTL_PCIE_FRAG_ALLOC_MAX(btl, frag, rc);
            if(NULL == frag) {
                return NULL;
            }
            if(max_data + reserve > frag->size){
                max_data = frag->size - reserve;
            }
            iov.iov_len = max_data;
            iov.iov_base = (unsigned char*) frag->segment.seg_addr.pval + reserve;
            
            rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
            *size  = max_data;
            
            if( rc < 0 ) {
                MCA_BTL_PCIE_FRAG_RETURN(btl, frag, rc);
            return NULL;
            }
        frag->segment.seg_len = max_data + reserve;
        
        }
        frag->hdr->length = *size + reserve;
        frag->base.des_src = &frag->segment;
        frag->base.des_src_cnt = 1;
        frag->base.des_dst = NULL;
        frag->base.des_dst_cnt = 0;
        frag->base.des_flags = 0;
        return &frag->base;
    }

}

/**
 * Prepare a descriptor for send/rdma using the supplied
 * convertor. If the convertor references data that is contigous,
 * the descriptor may simply point to the user buffer. Otherwise,
 * this routine is responsible for allocating buffer space and
 * packing if required.
 *
 * @param btl (IN)          BTL module
 * @param endpoint (IN)     BTL peer addressing
 * @param convertor (IN)    Data type convertor
 * @param reserve (IN)      Additional bytes requested by upper layer to precede user data
 * @param size (IN/OUT)     Number of bytes to prepare (IN), number of bytes actually prepared (OUT)
 */

mca_btl_base_descriptor_t* mca_btl_pcie_prepare_dst(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_pcie_frag_t* frag;
    mca_btl_pcie_reg_t* pcie_reg;
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl;
    int rc;
    ptrdiff_t lb;

    MCA_BTL_PCIE_FRAG_ALLOC_DMA(pcie_btl, frag, rc);
    if(NULL == frag) { 
        return NULL;
    }
    ompi_datatype_type_lb((ompi_datatype_t*)convertor->pDesc, &lb);
    frag->segment.seg_addr.pval = convertor->pBaseBuf + lb +
        convertor->bConverted;
    if(NULL == registration) { 
        rc = pcie_btl->rdma_mpool->mpool_register(pcie_btl->rdma_mpool,
						  frag->segment.seg_addr.pval, *size, 0, 
						  &registration);
        if(OMPI_SUCCESS != rc || NULL == registration) {
            MCA_BTL_PCIE_FRAG_RETURN(pcie_btl, frag, rc);
            return NULL;
        }
        frag->registration = (mca_btl_pcie_reg_t*) registration;
    }
    pcie_reg = (mca_btl_pcie_reg_t*)registration;

    frag->segment.seg_len = *size;
    frag->segment.seg_key.key64 = (uint64_t) pcie_reg->handle;

    frag->base.des_dst = &frag->segment;
    frag->base.des_dst_cnt = 1;
    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_flags = 0;

    BTL_VERBOSE(("prepare_dst: frag->segment.seg_len = %lu .seg_addr.pval= %lu "
	       "frag->segment.seg_key.key64 = %lu",
	       (unsigned long)frag->segment.seg_len, (unsigned long)frag->segment.seg_addr.pval,
	       (unsigned long)frag->segment.seg_key.key64));

    return &frag->base;
}


/**
 * Initiate an asynchronous send.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transfered
 * @param tag (IN)         The tag value used to notify the peer.
 */

int mca_btl_pcie_send( 
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor, 
    mca_btl_base_tag_t tag)
   
{
    /* mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl; */
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl;
    mca_btl_pcie_frag_t* frag = (mca_btl_pcie_frag_t*)descriptor; 
    mca_btl_pcie_sma_buf_t *buf = NULL;
    int rc;
    btl_pcie_fifo_entry_t idx;

    /* setup these fields so they get pulled over in the memcpy */
    frag->hdr->tag = tag;
    frag->hdr->length = frag->segment.seg_len;

    if (frag->type == MCA_BTL_PCIE_TYPE_EAGER) {
        MCA_BTL_PCIE_SMA_BUF_ALLOC_EAGER(pcie_btl, buf, rc);
    } else {
        MCA_BTL_PCIE_SMA_BUF_ALLOC_MAX(pcie_btl, buf, rc);
    }
    if (NULL == frag) { 
        BTL_ERROR(("can't alloc buf for frag of type %d", frag->type));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    frag->endpoint = endpoint; 
    frag->sma_buf = buf;
    /* Put fragment into network byte order before copy to save work
       done in sma region */
    OMPI_BTL_PCIE_HEADER_HTON(*frag->hdr);
    /* BWB - FIX ME - both pointers are 16 byte aligned and the
       buffers behind them are a multiple of 16 in length (but
       frag->segment.seg_len might not be).  There might be a more
       optimized memcpy option given that behavior. */
    memcpy(buf->pcie_data.pval, frag->hdr,
           sizeof(mca_btl_pcie_header_t) + 
           frag->segment.seg_len);

    /* send the fragment pointer to the receiver, 
       who will later ACK it back so that we can return it */
    idx = ((char*) buf->pcie_data.pval) - ((char*) endpoint->rem_frag_base);
    idx |= BTL_PCIE_FIFO_TYPE_SEND;

    /* make sure the top bit is zero */ 
    assert((idx & BTL_PCIE_FIFO_TYPE_MASK) == BTL_PCIE_FIFO_TYPE_SEND);

    /* need to barrier prior to writing remote completion */
    opal_atomic_wmb();
    
    BTL_VERBOSE(("sent frag 0x%lx (offset %lx), tag %d, length %d, rc = %d", 
                 (long)frag, idx, frag->hdr->tag, frag->segment.seg_len, rc));

    idx = opal_swap_bytes8(idx);
    rc = ompi_btl_pcie_fifo_set_msg(&endpoint->send_fifo, idx);
    if(OMPI_SUCCESS != rc) { 
        if(OMPI_ERR_RESOURCE_BUSY == rc) {
            /* BWB - FIX ME - queue for later */
            abort();
        } else { 
            return rc;
        }
    }

    return OMPI_SUCCESS;
}


static int
dd_dma_request(DD_adapter_handle *a_handle,
               struct AXON_dma_request *dma_req,
               int dma_requests_available,
               int *dma_requests_started)
{
    int rc;
#if 0
    struct AXON_dma_command_list dma_op;
    
    memset (&dma_op, 0x00, sizeof(dma_op));
    dma_op.dma_req = dma_req;
    dma_op.dma_requests_available = dma_requests_available;
    rc = ioctl (a_handle->fd, AXONIO_ISSUE_DMA ,&dma_op);
    
    if (0 == rc) {
        *dma_requests_started = dma_op.dma_requests_started;
    }
#else
    struct AXON_dma_command_list_fast *command = a_handle->cmd_block;
    
    command->dma_req_offset = sizeof (struct AXON_dma_command_list_fast);
    command->dma_requests_available = dma_requests_available;
    command->dma_requests_started = 0;
    dma_req->flags = AXON_DMAFLAG_WRITE_REMOTE_STATUS;
    memcpy ((char *) command + command->dma_req_offset,
            dma_req, 
            sizeof (struct AXON_dma_request));
    
    rc = ioctl (a_handle->fd, AXONIO_ISSUE_DMA_FAST, 0);
    if (0 == rc)
        *dma_requests_started = command->dma_requests_started;
    
#endif
    return rc;
}

/**
 * Initiate an asynchronous put.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 */

int mca_btl_pcie_put( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    
    mca_btl_pcie_frag_t* frag = (mca_btl_pcie_frag_t*) descriptor;
    struct AXON_dma_request dma_req;
    int dma_reqs_started;
    int rc;
    volatile uint64_t *dma_status_addr;
    uint64_t dma_status;

    frag->endpoint = endpoint;

    memset(&dma_req,0x00,sizeof(dma_req));
    dma_req.dma_type = AXON_DMATYPE_PUT;
    
    dma_req.local_descriptor[0].src_address = frag->base.des_src->seg_addr.lval;
    dma_req.local_descriptor[0].src_memory_region_handle = frag->base.des_src->seg_key.key64;

    dma_req.remote_descriptor[0].src_address = 
	opal_swap_bytes8(frag->base.des_dst->seg_addr.lval);
    dma_req.remote_descriptor[0].src_memory_region_handle = 
	opal_swap_bytes8(frag->base.des_dst->seg_key.key64);

    dma_req.transfer_size =
	dma_req.remote_descriptor[0].transfer_size = 
	dma_req.local_descriptor[0].transfer_size = frag->base.des_src->seg_len;

    dma_req.localDmaStatusOffset = endpoint->lcl_dma_status - (char*) endpoint->lcl_sma_ptr;
    dma_req.remoteDmaStatusOffset = 0;
    
    dma_req.local_descriptor_count = 1;
    dma_req.remote_descriptor_count = 1;
    
    dma_status_addr = (uint64_t*) endpoint->lcl_dma_status;
    *dma_status_addr = 0;
    
    rc = dd_dma_request(&endpoint->pcie_adapter,
                        &dma_req,
                        1,
                        &dma_reqs_started);
    
    if (0 != rc) abort();

    /* wait for completion, for now anyway */
    while (0 == (dma_status = *dma_status_addr)) {
      /* sched_yield(); */
    }

    frag->base.des_cbfunc(btl, endpoint, &(frag->base), OMPI_SUCCESS);
    
    return OMPI_SUCCESS; 
}


/**
 * Initiate an asynchronous get.
 *
 * @param btl (IN)         BTL module
 * @param endpoint (IN)    BTL addressing information
 * @param descriptor (IN)  Description of the data to be transferred
 *
 */

int mca_btl_pcie_get( 
    mca_btl_base_module_t* btl,
    mca_btl_base_endpoint_t* endpoint,
    mca_btl_base_descriptor_t* descriptor)
{
    return OMPI_ERR_NOT_IMPLEMENTED; 
}



/*
 * Cleanup/release module resources.
 */

int mca_btl_pcie_finalize(struct mca_btl_base_module_t* btl)
{
    mca_btl_pcie_module_t* pcie_btl = (mca_btl_pcie_module_t*) btl; 
    OBJ_DESTRUCT(&pcie_btl->pcie_lock);
    OBJ_DESTRUCT(&pcie_btl->pcie_sma_buf_eager);
    OBJ_DESTRUCT(&pcie_btl->pcie_sma_buf_max);
    OBJ_DESTRUCT(&pcie_btl->pcie_frag_eager);
    OBJ_DESTRUCT(&pcie_btl->pcie_frag_max);
    OBJ_DESTRUCT(&pcie_btl->pcie_frag_dma);
    OBJ_DESTRUCT(&pcie_btl->pcie_recv_frag);
    return OMPI_SUCCESS;
}
