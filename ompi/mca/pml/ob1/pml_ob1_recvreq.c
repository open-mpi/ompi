/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2012-2013 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2012      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/bml/bml.h" 
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_ob1_comm.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_rdmafrag.h"
#include "ompi/mca/bml/base/base.h" 
#include "opal/util/arch.h"
#include "ompi/memchecker.h"
#if OPAL_CUDA_SUPPORT
#include "opal/datatype/opal_datatype_cuda.h"
#include "ompi/mca/common/cuda/common_cuda.h"
#endif /* OPAL_CUDA_SUPPORT */

#if OPAL_CUDA_SUPPORT
int mca_pml_ob1_cuda_need_buffers(mca_pml_ob1_recv_request_t* recvreq,
                                  mca_btl_base_module_t* btl);
#endif /* OPAL_CUDA_SUPPORT */

void mca_pml_ob1_recv_request_process_pending(void)
{
    mca_pml_ob1_recv_request_t* recvreq;
    int rc, i, s = (int)opal_list_get_size(&mca_pml_ob1.recv_pending);

    for(i = 0; i < s; i++) {
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        recvreq = (mca_pml_ob1_recv_request_t*)
            opal_list_remove_first(&mca_pml_ob1.recv_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        if( OPAL_UNLIKELY(NULL == recvreq) )
            break;
        recvreq->req_pending = false;
        rc = mca_pml_ob1_recv_request_schedule_exclusive(recvreq, NULL);
        if(OMPI_ERR_OUT_OF_RESOURCE == rc)
            break;
    }
}

static int mca_pml_ob1_recv_request_free(struct ompi_request_t** request)
{
    mca_pml_ob1_recv_request_t* recvreq = *(mca_pml_ob1_recv_request_t**)request; 

    assert( false == recvreq->req_recv.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_recv.req_base.req_free_called = true;

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_NOTIFY,
                             &(recvreq->req_recv.req_base), PERUSE_RECV );

    if( true == recvreq->req_recv.req_base.req_pml_complete ) {
        /* make buffer defined when the request is compeleted,
           and before releasing the objects. */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            recvreq->req_recv.req_base.req_addr,
                            recvreq->req_recv.req_base.req_count,
                            recvreq->req_recv.req_base.req_datatype);
        );

        MCA_PML_OB1_RECV_REQUEST_RETURN( recvreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
} 

static int mca_pml_ob1_recv_request_cancel(struct ompi_request_t* ompi_request, int complete)
{
    mca_pml_ob1_recv_request_t* request = (mca_pml_ob1_recv_request_t*)ompi_request;
    mca_pml_ob1_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;

    if( true == request->req_match_received ) { /* way to late to cancel this one */
        assert( OMPI_ANY_TAG != ompi_request->req_status.MPI_TAG ); /* not matched isn't it */
        return OMPI_SUCCESS;
    }

    /* The rest should be protected behind the match logic lock */
    OPAL_THREAD_LOCK(&comm->matching_lock);
    if( request->req_recv.req_base.req_peer == OMPI_ANY_SOURCE ) {
        opal_list_remove_item( &comm->wild_receives, (opal_list_item_t*)request );
    } else {
        mca_pml_ob1_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
        opal_list_remove_item(&proc->specific_receives, (opal_list_item_t*)request);
    }
    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q,
                             &(request->req_recv.req_base), PERUSE_RECV );
    /**
     * As now the PML is done with this request we have to force the pml_complete
     * to true. Otherwise, the request will never be freed.
     */
    request->req_recv.req_base.req_pml_complete = true;
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    
    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request->req_status._cancelled = true;
    /* This macro will set the req_complete to true so the MPI Test/Wait* functions
     * on this request will be able to complete. As the status is marked as
     * cancelled the cancel state will be detected.
     */
    MCA_PML_OB1_RECV_REQUEST_MPI_COMPLETE(request);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    /*
     * Receive request cancelled, make user buffer accessible.
     */
    MEMCHECKER(
        memchecker_call(&opal_memchecker_base_mem_defined,
                        request->req_recv.req_base.req_addr,
                        request->req_recv.req_base.req_count,
                        request->req_recv.req_base.req_datatype);
    );
    return OMPI_SUCCESS;
}

static void mca_pml_ob1_recv_request_construct(mca_pml_ob1_recv_request_t* request)
{
    /* the request type is set by the superclass */
    request->req_recv.req_base.req_ompi.req_free = mca_pml_ob1_recv_request_free;
    request->req_recv.req_base.req_ompi.req_cancel = mca_pml_ob1_recv_request_cancel;
    request->req_rdma_cnt = 0;
    OBJ_CONSTRUCT(&request->lock, opal_mutex_t);
}

static void mca_pml_ob1_recv_request_destruct(mca_pml_ob1_recv_request_t* request)
{
    OBJ_DESTRUCT(&request->lock);
}

OBJ_CLASS_INSTANCE(
    mca_pml_ob1_recv_request_t,
    mca_pml_base_recv_request_t,
    mca_pml_ob1_recv_request_construct,
    mca_pml_ob1_recv_request_destruct);


/*
 * Release resources.
 */

static void mca_pml_ob1_recv_ctl_completion( mca_btl_base_module_t* btl,
                                             struct mca_btl_base_endpoint_t* ep,
                                             struct mca_btl_base_descriptor_t* des,
                                             int status )
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;

    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/*
 * Put operation has completed remotely - update request status
 */

static void mca_pml_ob1_put_completion( mca_btl_base_module_t* btl,
                                        struct mca_btl_base_endpoint_t* ep,
                                        struct mca_btl_base_descriptor_t* des,
                                        int status )
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)des->des_cbdata;
    size_t bytes_received = 0;

    if( OPAL_LIKELY(status == OMPI_SUCCESS) ) {
        bytes_received = mca_pml_ob1_compute_segment_length (btl->btl_seg_size,
                                                             (void *) des->des_dst,
                                                             des->des_dst_cnt, 0);
    }
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_pipeline_depth,-1);

    mca_bml_base_free(bml_btl, des);

    /* check completion status */
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received);
    if(recv_request_pml_complete_check(recvreq) == false &&
            recvreq->req_rdma_offset < recvreq->req_send_offset) {
        /* schedule additional rdma operations */
        mca_pml_ob1_recv_request_schedule(recvreq, bml_btl);
    }
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/*
 *
 */

int mca_pml_ob1_recv_request_ack_send_btl(
        ompi_proc_t* proc, mca_bml_base_btl_t* bml_btl,
        uint64_t hdr_src_req, void *hdr_dst_req, uint64_t hdr_send_offset,
        bool nordma)
{
    mca_btl_base_descriptor_t* des;
    mca_pml_ob1_ack_hdr_t* ack;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_ob1_ack_hdr_t),
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE; 
    }

    /* fill out header */
    ack = (mca_pml_ob1_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_ACK;
    ack->hdr_common.hdr_flags = nordma ? MCA_PML_OB1_HDR_FLAGS_NORDMA : 0;
    ack->hdr_src_req.lval = hdr_src_req;
    ack->hdr_dst_req.pval = hdr_dst_req;
    ack->hdr_send_offset = hdr_send_offset;

    ob1_hdr_hton(ack, MCA_PML_OB1_HDR_TYPE_ACK, proc);

    /* initialize descriptor */
    des->des_cbfunc = mca_pml_ob1_recv_ctl_completion;

    rc = mca_bml_base_send(bml_btl, des, MCA_PML_OB1_HDR_TYPE_ACK);
    if( OPAL_LIKELY( rc >= 0 ) ) {
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des);
    return OMPI_ERR_OUT_OF_RESOURCE; 
}

static int mca_pml_ob1_recv_request_ack(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_pml_ob1_rendezvous_hdr_t* hdr, 
    size_t bytes_received)
{
    ompi_proc_t* proc = (ompi_proc_t*)recvreq->req_recv.req_base.req_proc;
    mca_bml_base_endpoint_t* bml_endpoint = NULL;

    bml_endpoint = (mca_bml_base_endpoint_t*) proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML]; 

    /* by default copy everything */
    recvreq->req_send_offset = bytes_received;
    if(hdr->hdr_msg_length > bytes_received) {
        size_t rdma_num = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_rdma);
        /*
         * lookup request buffer to determine if memory is already
         * registered. 
         */

        if(opal_convertor_need_buffers(&recvreq->req_recv.req_base.req_convertor) == 0 &&
           hdr->hdr_match.hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_CONTIG &&
           rdma_num != 0) {
            unsigned char *base;
            opal_convertor_get_current_pointer( &recvreq->req_recv.req_base.req_convertor, (void**)&(base) );
           
            if(hdr->hdr_match.hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_PIN)
                recvreq->req_rdma_cnt = mca_pml_ob1_rdma_btls(bml_endpoint,
                        base, recvreq->req_recv.req_bytes_packed,
                        recvreq->req_rdma );
            else
                recvreq->req_rdma_cnt = 0;

            /* memory is already registered on both sides */
            if (recvreq->req_rdma_cnt != 0) {
                recvreq->req_send_offset = hdr->hdr_msg_length;
                /* are rdma devices available for long rdma protocol */
            } else if(bml_endpoint->btl_send_limit < hdr->hdr_msg_length) {
                /* use convertor to figure out the rdma offset for this request */
                recvreq->req_send_offset = hdr->hdr_msg_length - 
                    bml_endpoint->btl_pipeline_send_length;

                if(recvreq->req_send_offset < bytes_received)
                    recvreq->req_send_offset = bytes_received;

                /* use converter to figure out the rdma offset for this
                 * request */
                opal_convertor_set_position(&recvreq->req_recv.req_base.req_convertor,
                        &recvreq->req_send_offset);

                recvreq->req_rdma_cnt =
                    mca_pml_ob1_rdma_pipeline_btls(bml_endpoint,
                            recvreq->req_send_offset - bytes_received,
                            recvreq->req_rdma);
            }
        }
        /* nothing to send by copy in/out - no need to ack */
        if(recvreq->req_send_offset == hdr->hdr_msg_length)
            return OMPI_SUCCESS;
    }
    /* let know to shedule function there is no need to put ACK flag */
    recvreq->req_ack_sent = true;
    return mca_pml_ob1_recv_request_ack_send(proc, hdr->hdr_src_req.lval,
                                             recvreq, recvreq->req_send_offset,
                                             recvreq->req_send_offset == bytes_received);
}

/**
 * Return resources used by the RDMA
 */

static void mca_pml_ob1_rget_completion( mca_btl_base_module_t* btl,
                                         struct mca_btl_base_endpoint_t* ep,
                                         struct mca_btl_base_descriptor_t* des,
                                         int status )
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)frag->rdma_req;

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
        /* TSW - FIX */
        OMPI_ERROR_LOG(status);
        ompi_rte_abort(-1, NULL);
    }

    /* is receive request complete */
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, frag->rdma_length);
    if (recvreq->req_recv.req_bytes_packed <= recvreq->req_bytes_received) {
        mca_pml_ob1_send_fin(recvreq->req_recv.req_base.req_proc,
                             bml_btl,
                             frag->rdma_hdr.hdr_rget.hdr_des,
                             des->order, 0);
    }

    recv_request_pml_complete_check(recvreq);

    MCA_PML_OB1_RDMA_FRAG_RETURN(frag);

    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}


static int mca_pml_ob1_init_get_fallback (mca_pml_ob1_rdma_frag_t *frag,
                                          mca_btl_base_descriptor_t *dst) {
    mca_pml_ob1_recv_request_t *recvreq = (mca_pml_ob1_recv_request_t *) frag->rdma_req;
    mca_bml_base_btl_t *bml_btl = frag->rdma_bml;
    mca_btl_base_descriptor_t *ctl;
    mca_pml_ob1_rdma_hdr_t *hdr;
    size_t seg_size;
    int rc;

    seg_size = bml_btl->btl->btl_seg_size * dst->des_dst_cnt;

    /* prepare a descriptor for rdma control message */
    mca_bml_base_alloc (bml_btl, &ctl, MCA_BTL_NO_ORDER, sizeof (mca_pml_ob1_rdma_hdr_t) + seg_size,
                        MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP |
                        MCA_BTL_DES_SEND_ALWAYS_CALLBACK);
    if (OPAL_UNLIKELY(NULL == ctl)) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    ctl->des_cbfunc = mca_pml_ob1_recv_ctl_completion;
        
    /* fill in rdma header */
    hdr = (mca_pml_ob1_rdma_hdr_t *) ctl->des_src->seg_addr.pval;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_PUT;
    hdr->hdr_common.hdr_flags =
        (!recvreq->req_ack_sent) ? MCA_PML_OB1_HDR_TYPE_ACK : 0;

    hdr->hdr_req = frag->rdma_hdr.hdr_rget.hdr_rndv.hdr_src_req;
    hdr->hdr_rdma_offset = recvreq->req_rdma_offset;
    hdr->hdr_des.pval = dst;
    hdr->hdr_recv_req.pval = recvreq;

    hdr->hdr_seg_cnt = dst->des_dst_cnt;

    /* copy segments */
    memcpy (hdr + 1, dst->des_dst, seg_size);

    dst->des_cbfunc = mca_pml_ob1_put_completion;
    dst->des_cbdata = recvreq;

    if (!recvreq->req_ack_sent)
        recvreq->req_ack_sent = true;

    /* send rdma request to peer */
    rc = mca_bml_base_send (bml_btl, ctl, MCA_PML_OB1_HDR_TYPE_PUT);
    if (OPAL_UNLIKELY(rc < 0)) {
        mca_bml_base_free (bml_btl, ctl);
        return rc;
    }

    return OMPI_SUCCESS;
}

/*
 *
 */
int mca_pml_ob1_recv_request_get_frag( mca_pml_ob1_rdma_frag_t* frag )
{
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)frag->rdma_req;
    mca_bml_base_btl_t* bml_btl = frag->rdma_bml;
    mca_btl_base_descriptor_t* descriptor;
    size_t save_size = frag->rdma_length;
    int rc;

    /* prepare descriptor */
    mca_bml_base_prepare_dst( bml_btl, 
                              NULL,
                              &recvreq->req_recv.req_base.req_convertor,
                              MCA_BTL_NO_ORDER,
                              0,
                              &frag->rdma_length,
                              MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK |
                              MCA_BTL_DES_FLAGS_GET,
                              &descriptor );
    if( OPAL_UNLIKELY(NULL == descriptor) ) {
        if (frag->retries < mca_pml_ob1.rdma_retries_limit) {
            frag->rdma_length = save_size;
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } else {
            ompi_proc_t *proc = (ompi_proc_t *) recvreq->req_recv.req_base.req_proc;

            /* tell peer to fall back on send */
            recvreq->req_send_offset = 0;
            rc = mca_pml_ob1_recv_request_ack_send(proc, frag->rdma_hdr.hdr_rget.hdr_rndv.hdr_src_req.lval,
                                                   recvreq, recvreq->req_send_offset, true);
            MCA_PML_OB1_RDMA_FRAG_RETURN(frag);
            return rc;
        }
    }

    descriptor->des_src = (mca_btl_base_segment_t *) frag->rdma_segs;
    descriptor->des_src_cnt = frag->rdma_hdr.hdr_rdma.hdr_seg_cnt;
    descriptor->des_cbfunc = mca_pml_ob1_rget_completion;
    descriptor->des_cbdata = frag;

    PERUSE_TRACE_COMM_OMPI_EVENT(PERUSE_COMM_REQ_XFER_CONTINUE,
                                 &(recvreq->req_recv.req_base),
                                 frag->rdma_length, PERUSE_RECV);

    /* queue up get request */
    rc = mca_bml_base_get(bml_btl,descriptor);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
        if (OPAL_UNLIKELY(OMPI_ERR_NOT_AVAILABLE == rc)) {
            /* get isn't supported for this transfer. tell peer to fallback on put */
            rc = mca_pml_ob1_init_get_fallback (frag, descriptor);
         }

        if(OMPI_ERR_OUT_OF_RESOURCE == rc) {
            mca_bml_base_free(bml_btl, descriptor);
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending,
                    (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } else if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            OMPI_ERROR_LOG(rc);
            ompi_rte_abort(-1, NULL);
        }
    }

    return OMPI_SUCCESS;
}




/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_ob1_recv_request_progress_frag( mca_pml_ob1_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    size_t bytes_received, data_offset = 0;
    size_t bytes_delivered __opal_attribute_unused__; /* is being set to zero in MCA_PML_OB1_RECV_REQUEST_UNPACK */
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    bytes_received = mca_pml_ob1_compute_segment_length_base (segments, num_segments,
                                                              sizeof(mca_pml_ob1_frag_hdr_t));
    data_offset     = hdr->hdr_frag.hdr_frag_offset;
    /*
     *  Make user buffer accessible(defined) before unpacking.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_defined,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    MCA_PML_OB1_RECV_REQUEST_UNPACK( recvreq,
                                     segments,
                                     num_segments,
                                     sizeof(mca_pml_ob1_frag_hdr_t),
                                     data_offset,
                                     bytes_received,
                                     bytes_delivered );
    /*
     *  Unpacking finished, make the user buffer unaccessable again.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_noaccess,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received);
    /* check completion status */
    if(recv_request_pml_complete_check(recvreq) == false &&
            recvreq->req_rdma_offset < recvreq->req_send_offset) {
        /* schedule additional rdma operations */
        mca_pml_ob1_recv_request_schedule(recvreq, NULL);
    }
}

#if OPAL_CUDA_SUPPORT /* CUDA_ASYNC_RECV */
/**
 * This function is basically the first half of the code in the
 * mca_pml_ob1_recv_request_progress_frag function.  This fires off
 * the asynchronous copy and returns.  Unused fields in the descriptor
 * are used to pass extra information for when the asynchronous copy
 * completes.  No memchecker support in this function as copies are 
 * happening asynchronously.
 */
void mca_pml_ob1_recv_request_frag_copy_start( mca_pml_ob1_recv_request_t* recvreq,
                                               mca_btl_base_module_t* btl,
                                               mca_btl_base_segment_t* segments,
                                               size_t num_segments,
                                               mca_btl_base_descriptor_t* des)
{
    int result;
    size_t bytes_received = 0, data_offset = 0;
    size_t bytes_delivered __opal_attribute_unused__; /* is being set to zero in MCA_PML_OB1_RECV_REQUEST_UNPACK */
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    OPAL_OUTPUT((-1, "start_frag_copy frag=%p", (void *)des));

    bytes_received = mca_pml_ob1_compute_segment_length_base (segments, num_segments,
                                                              sizeof(mca_pml_ob1_frag_hdr_t));
    data_offset     = hdr->hdr_frag.hdr_frag_offset;

    MCA_PML_OB1_RECV_REQUEST_UNPACK( recvreq,
                                     segments,
                                     num_segments,
                                     sizeof(mca_pml_ob1_frag_hdr_t),
                                     data_offset,
                                     bytes_received,
                                     bytes_delivered );
    /* Store the receive request in unused context pointer. */
    des->des_context = (void *)recvreq;
    /* Store the amount of bytes in unused src count value */
    des->des_src_cnt = bytes_delivered;
    /* Then record an event that will get triggered by a PML progress call which
     * checks the stream events.  If we get an error, abort.  Should get message
     * from CUDA code about what went wrong. */
    result = mca_common_cuda_record_htod_event("pml", des);
    if (OMPI_SUCCESS != result) {
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        ompi_rte_abort(-1, NULL);
    }
}

/**
 * This function is basically the second half of the code in the
 * mca_pml_ob1_recv_request_progress_frag function.  The number of
 * bytes delivered is updated.  Then a call is made into the BTL so it
 * can free the fragment that held that data.  This is currently
 * called directly by the common CUDA code. No memchecker support
 * in this function as copies are happening asynchronously.
 */
void mca_pml_ob1_recv_request_frag_copy_finished( mca_btl_base_module_t* btl,
                                                  struct mca_btl_base_endpoint_t* ep,
                                                  struct mca_btl_base_descriptor_t* des,
                                                  int status )
{
    mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)des->des_context;
    size_t bytes_received = des->des_src_cnt;

    OPAL_OUTPUT((-1, "frag_copy_finished (delivered=%d), frag=%p", (int)bytes_received, (void *)des));
    /* Call into the BTL so it can free the descriptor.  At this point, it is 
     * known that the data has been copied out of the descriptor. */
    des->des_cbfunc(NULL, (struct mca_btl_base_endpoint_t *)des->des_cbdata, des, 0);

    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received);

    /* check completion status */
    if(recv_request_pml_complete_check(recvreq) == false &&
            recvreq->req_rdma_offset < recvreq->req_send_offset) {
        /* schedule additional rdma operations */
        mca_pml_ob1_recv_request_schedule(recvreq, NULL);
    }
}
#endif /* OPAL_CUDA_SUPPORT */

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_ob1_recv_request_progress_rget( mca_pml_ob1_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    mca_pml_ob1_rget_hdr_t* hdr = (mca_pml_ob1_rget_hdr_t*)segments->seg_addr.pval;
    mca_bml_base_endpoint_t* bml_endpoint = NULL;
    size_t bytes_remaining, prev_sent, offset;
    mca_btl_base_segment_t *r_segments;
    mca_pml_ob1_rdma_frag_t *frag;
    mca_bml_base_btl_t *rdma_bml;
    int rc;

    prev_sent = offset = 0;
    bytes_remaining = hdr->hdr_rndv.hdr_msg_length;
    recvreq->req_recv.req_bytes_packed = hdr->hdr_rndv.hdr_msg_length;

    MCA_PML_OB1_RECV_REQUEST_MATCHED(recvreq, &hdr->hdr_rndv.hdr_match);
    
    /* if receive buffer is not contiguous we can't just RDMA read into it, so
     * fall back to copy in/out protocol. It is a pity because buffer on the
     * sender side is already registered. We need to be smarter here, perhaps
     * do couple of RDMA reads */
    if (opal_convertor_need_buffers(&recvreq->req_recv.req_base.req_convertor) == true) {
#if OPAL_CUDA_SUPPORT
        if (mca_pml_ob1_cuda_need_buffers(recvreq, btl))
#endif /* OPAL_CUDA_SUPPORT */
        {
            mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv, 0);
            return;
        }
    }
    
    /* lookup bml datastructures */
    bml_endpoint = (mca_bml_base_endpoint_t*)recvreq->req_recv.req_base.req_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
    rdma_bml = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);

#if OPAL_CUDA_SUPPORT
    if (OPAL_UNLIKELY(NULL == rdma_bml)) {
        if (recvreq->req_recv.req_base.req_convertor.flags & CONVERTOR_CUDA) {
            mca_bml_base_btl_t *bml_btl;
            bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_send, btl);
            /* Check to see if this is a CUDA get */
            if (bml_btl->btl_flags & MCA_BTL_FLAGS_CUDA_GET) {
                rdma_bml = bml_btl;
            }
        } else {
            /* Just default back to send and receive.  Must be mix of GPU and HOST memory. */
            mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv, 0);
            return;
        }
    }
#endif /* OPAL_CUDA_SUPPORT */

    if (OPAL_UNLIKELY(NULL == rdma_bml)) {
        opal_output(0, "[%s:%d] invalid bml for rdma get", __FILE__, __LINE__);
        ompi_rte_abort(-1, NULL);
    }

    bytes_remaining = mca_pml_ob1_compute_segment_length_remote (btl->btl_seg_size, (void *)(hdr + 1),
                                                                 hdr->hdr_seg_cnt, recvreq->req_recv.req_base.req_proc);

    /* The while loop adds a fragmentation mechanism. The variable bytes_remaining holds the num
     * of bytes left to be send. In each iteration we send the max possible bytes supported
     * by the HCA. The field  frag->rdma_length holds the actual num of  bytes that were
     * sent in each iteration. We subtract this number from bytes_remaining and continue to
     * the next iteration with the updated size.
     * Also - In each iteration we update the location in the buffer to be used for writing
     * the message ,and the location to read from. This is done using the offset variable that
     * accumulates the number of bytes that were sent so far. */
    while (bytes_remaining > 0) {
        /* allocate/initialize a fragment */
        MCA_PML_OB1_RDMA_FRAG_ALLOC(frag);
        if (OPAL_UNLIKELY(NULL == frag)) {
            /* GLB - FIX */
             OMPI_ERROR_LOG(OMPI_ERR_OUT_OF_RESOURCE);
             ompi_rte_abort(-1, NULL);
        }

        assert (btl->btl_seg_size * hdr->hdr_seg_cnt <= sizeof (frag->rdma_segs));

        memcpy (frag->rdma_segs, hdr + 1, btl->btl_seg_size * hdr->hdr_seg_cnt);

        /* update the read location -- NTH: note this will only work if there is exactly one
           segment. TODO -- make this work with multiple segments */
        r_segments = (mca_btl_base_segment_t *) frag->rdma_segs;
        r_segments->seg_addr.lval += offset;

        /* updating the write location */
        OPAL_THREAD_LOCK(&recvreq->lock);
        opal_convertor_set_position( &recvreq->req_recv.req_base.req_convertor, &offset);
        OPAL_THREAD_UNLOCK(&recvreq->lock);

        frag->rdma_bml = rdma_bml;

        frag->rdma_hdr.hdr_rget = *hdr;
        frag->retries     = 0;
        frag->rdma_req    = recvreq;
        frag->rdma_ep     = bml_endpoint;
        frag->rdma_state  = MCA_PML_OB1_RDMA_GET;
        frag->reg         = NULL;
        frag->rdma_length = bytes_remaining;

        /* NTH: TODO -- handle error conditions gracefully */
        rc = mca_pml_ob1_recv_request_get_frag(frag);
        if (OMPI_SUCCESS != rc) {
            break;
        }

        prev_sent = frag->rdma_length;
        bytes_remaining -= prev_sent;
        offset += prev_sent;
    }
}

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_ob1_recv_request_progress_rndv( mca_pml_ob1_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    size_t bytes_received = 0;
    size_t bytes_delivered __opal_attribute_unused__; /* is being set to zero in MCA_PML_OB1_RECV_REQUEST_UNPACK */
    size_t data_offset = 0;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    bytes_received = mca_pml_ob1_compute_segment_length_base (segments, num_segments,
                                                              sizeof(mca_pml_ob1_rendezvous_hdr_t));

    recvreq->req_recv.req_bytes_packed = hdr->hdr_rndv.hdr_msg_length;
    recvreq->remote_req_send = hdr->hdr_rndv.hdr_src_req;
    recvreq->req_rdma_offset = bytes_received;
    MCA_PML_OB1_RECV_REQUEST_MATCHED(recvreq, &hdr->hdr_match);
    mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv, bytes_received);
    /**
     * The PUT protocol do not attach any data to the original request.
     * Therefore, we might want to avoid unpacking if there is nothing to
     * unpack.
     */
    if( 0 < bytes_received ) {
        MEMCHECKER(
                   memchecker_call(&opal_memchecker_base_mem_defined,
                                   recvreq->req_recv.req_base.req_addr,
                                   recvreq->req_recv.req_base.req_count,
                                   recvreq->req_recv.req_base.req_datatype);
                   );
        MCA_PML_OB1_RECV_REQUEST_UNPACK( recvreq,
                                         segments,
                                         num_segments,
                                         sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                         data_offset,
                                         bytes_received,
                                         bytes_delivered );
        MEMCHECKER(
                   memchecker_call(&opal_memchecker_base_mem_noaccess,
                                   recvreq->req_recv.req_base.req_addr,
                                   recvreq->req_recv.req_base.req_count,
                                   recvreq->req_recv.req_base.req_datatype);
                   );
    }
    OPAL_THREAD_ADD_SIZE_T(&recvreq->req_bytes_received, bytes_received);
    /* check completion status */
    if(recv_request_pml_complete_check(recvreq) == false &&
       recvreq->req_rdma_offset < recvreq->req_send_offset) {
        /* schedule additional rdma operations */
        mca_pml_ob1_recv_request_schedule(recvreq, NULL);
    }

#if OPAL_CUDA_SUPPORT /* CUDA_ASYNC_RECV */
    /* If BTL supports it and this is a CUDA buffer being received into,
     * have all subsequent FRAGS copied in asynchronously. */
    if ((recvreq->req_recv.req_base.req_convertor.flags & CONVERTOR_CUDA) &&
        (btl->btl_flags & MCA_BTL_FLAGS_CUDA_COPY_ASYNC_RECV)) {
        void *strm = mca_common_cuda_get_htod_stream();
        opal_cuda_set_copy_function_async(&recvreq->req_recv.req_base.req_convertor, strm);
    }
#endif
    
}

/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */
void mca_pml_ob1_recv_request_progress_match( mca_pml_ob1_recv_request_t* recvreq,
                                              mca_btl_base_module_t* btl,
                                              mca_btl_base_segment_t* segments,
                                              size_t num_segments )
{
    size_t bytes_received, data_offset = 0;
    size_t bytes_delivered __opal_attribute_unused__; /* is being set to zero in MCA_PML_OB1_RECV_REQUEST_UNPACK */
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    bytes_received = mca_pml_ob1_compute_segment_length_base (segments, num_segments,
                                                              OMPI_PML_OB1_MATCH_HDR_LEN);

    recvreq->req_recv.req_bytes_packed = bytes_received;
    
    MCA_PML_OB1_RECV_REQUEST_MATCHED(recvreq, &hdr->hdr_match);
    /*
     *  Make user buffer accessable(defined) before unpacking.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_defined,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    MCA_PML_OB1_RECV_REQUEST_UNPACK( recvreq,
                                     segments,
                                     num_segments,
                                     OMPI_PML_OB1_MATCH_HDR_LEN,
                                     data_offset,
                                     bytes_received,
                                     bytes_delivered);
    /*
     *  Unpacking finished, make the user buffer unaccessable again.
     */
    MEMCHECKER(
               memchecker_call(&opal_memchecker_base_mem_noaccess,
                               recvreq->req_recv.req_base.req_addr,
                               recvreq->req_recv.req_base.req_count,
                               recvreq->req_recv.req_base.req_datatype);
               );
    
    /*
     * No need for atomic here, as we know there is only one fragment
     * for this request.
     */
    recvreq->req_bytes_received += bytes_received;
    recv_request_pml_complete(recvreq);
}


/**
 * Handle completion of a probe request
 */

void mca_pml_ob1_recv_request_matched_probe( mca_pml_ob1_recv_request_t* recvreq,
                                             mca_btl_base_module_t* btl,
                                             mca_btl_base_segment_t* segments,
                                             size_t num_segments )
{
    size_t bytes_packed = 0;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:
            bytes_packed = mca_pml_ob1_compute_segment_length_base (segments, num_segments,
                                                                    OMPI_PML_OB1_MATCH_HDR_LEN);
            break;
        case MCA_PML_OB1_HDR_TYPE_RNDV:
        case MCA_PML_OB1_HDR_TYPE_RGET:

            bytes_packed = hdr->hdr_rndv.hdr_msg_length;
            break;
    }

    /* set completion status */
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_TAG = hdr->hdr_match.hdr_tag;
    recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE = hdr->hdr_match.hdr_src;
    recvreq->req_bytes_received = bytes_packed;
    recvreq->req_bytes_expected = bytes_packed;

    recv_request_pml_complete(recvreq);
}


/*
 * Schedule RDMA protocol.
 *
*/

int mca_pml_ob1_recv_request_schedule_once( mca_pml_ob1_recv_request_t* recvreq,
                                            mca_bml_base_btl_t *start_bml_btl )
{
    mca_bml_base_btl_t* bml_btl; 
    int num_tries = recvreq->req_rdma_cnt, num_fail = 0;
    size_t i, prev_bytes_remaining = 0;
    size_t bytes_remaining = recvreq->req_send_offset -
        recvreq->req_rdma_offset;

    /* if starting bml_btl is provided schedule next fragment on it first */
    if(start_bml_btl != NULL) {
        for(i = 0; i < recvreq->req_rdma_cnt; i++) {
            if(recvreq->req_rdma[i].bml_btl != start_bml_btl)
                continue;
            /* something left to be send? */
            if( OPAL_LIKELY(recvreq->req_rdma[i].length) )
                recvreq->req_rdma_idx = i;
            break;
        }
    }

    while(bytes_remaining > 0 &&
           recvreq->req_pipeline_depth < mca_pml_ob1.recv_pipeline_depth) {
        size_t size, seg_size;
        mca_pml_ob1_rdma_hdr_t* hdr;
        mca_btl_base_descriptor_t* dst;
        mca_btl_base_descriptor_t* ctl;
        mca_mpool_base_registration_t * reg = NULL;
        mca_btl_base_module_t* btl;
        int rc, rdma_idx;

        if(prev_bytes_remaining == bytes_remaining) {
            if(++num_fail == num_tries) {
                OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                if(false == recvreq->req_pending) {
                    opal_list_append(&mca_pml_ob1.recv_pending,
                            (opal_list_item_t*)recvreq);
                    recvreq->req_pending = true;
                }
                OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        } else {
            num_fail = 0;
            prev_bytes_remaining = bytes_remaining;
        }

        do {
            rdma_idx = recvreq->req_rdma_idx;
            bml_btl = recvreq->req_rdma[rdma_idx].bml_btl;
            reg = recvreq->req_rdma[rdma_idx].btl_reg;
            size = recvreq->req_rdma[rdma_idx].length;
            if(++recvreq->req_rdma_idx >= recvreq->req_rdma_cnt)
                recvreq->req_rdma_idx = 0;
        } while(!size);
        btl = bml_btl->btl;

        /* makes sure that we don't exceed BTL max rdma size
         * if memory is not pinned already */
        if( (NULL == reg) && (btl->btl_rdma_pipeline_frag_size != 0) &&
                             (size > btl->btl_rdma_pipeline_frag_size)) {
            size = btl->btl_rdma_pipeline_frag_size;
        }

        /* take lock to protect converter against concurrent access
         * from unpack */
        OPAL_THREAD_LOCK(&recvreq->lock);
        opal_convertor_set_position( &recvreq->req_recv.req_base.req_convertor,
                                     &recvreq->req_rdma_offset );

        /* prepare a descriptor for RDMA */
        mca_bml_base_prepare_dst(bml_btl, reg, 
                                 &recvreq->req_recv.req_base.req_convertor,
                                 MCA_BTL_NO_ORDER, 0, &size, MCA_BTL_DES_FLAGS_BTL_OWNERSHIP |
                                 MCA_BTL_DES_FLAGS_PUT, &dst);
        OPAL_THREAD_UNLOCK(&recvreq->lock);

        if(OPAL_UNLIKELY(dst == NULL)) {
            continue;
        }

        dst->des_cbfunc = mca_pml_ob1_put_completion;
        dst->des_cbdata = recvreq;

        seg_size = btl->btl_seg_size * dst->des_dst_cnt;

        /* prepare a descriptor for rdma control message */
        mca_bml_base_alloc(bml_btl, &ctl, MCA_BTL_NO_ORDER, sizeof(mca_pml_ob1_rdma_hdr_t) + seg_size,
                           MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK);

        if( OPAL_UNLIKELY(NULL == ctl) ) {
            mca_bml_base_free(bml_btl,dst);
            continue;
        }
        ctl->des_cbfunc = mca_pml_ob1_recv_ctl_completion;
        
        /* fill in rdma header */
        hdr = (mca_pml_ob1_rdma_hdr_t*)ctl->des_src->seg_addr.pval;
        hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_PUT;
        hdr->hdr_common.hdr_flags =
            (!recvreq->req_ack_sent) ? MCA_PML_OB1_HDR_TYPE_ACK : 0;
        hdr->hdr_req = recvreq->remote_req_send;
        hdr->hdr_des.pval = dst;
        hdr->hdr_recv_req.pval = recvreq;
        hdr->hdr_rdma_offset = recvreq->req_rdma_offset;
        hdr->hdr_seg_cnt = dst->des_dst_cnt;

        /* copy segments */
        memmove (hdr + 1, dst->des_dst, seg_size);

        if(!recvreq->req_ack_sent)
            recvreq->req_ack_sent = true;
        ob1_hdr_hton(hdr, MCA_PML_OB1_HDR_TYPE_PUT, recvreq->req_recv.req_base.req_proc);

        PERUSE_TRACE_COMM_OMPI_EVENT( PERUSE_COMM_REQ_XFER_CONTINUE,
                                      &(recvreq->req_recv.req_base), size,
                                      PERUSE_RECV);

        /* send rdma request to peer */
        rc = mca_bml_base_send(bml_btl, ctl, MCA_PML_OB1_HDR_TYPE_PUT);
        if( OPAL_LIKELY( rc >= 0 ) ) {
            /* update request state */
            recvreq->req_rdma_offset += size;
            OPAL_THREAD_ADD_SIZE_T(&recvreq->req_pipeline_depth, 1);
            recvreq->req_rdma[rdma_idx].length -= size;
            bytes_remaining -= size;
        } else {
            mca_bml_base_free(bml_btl,ctl);
            mca_bml_base_free(bml_btl,dst);
        }
    }

    return OMPI_SUCCESS;
}

#define IS_PROB_REQ(R) \
    ((MCA_PML_REQUEST_IPROBE == (R)->req_recv.req_base.req_type) || \
     (MCA_PML_REQUEST_PROBE == (R)->req_recv.req_base.req_type) || \
     (MCA_PML_REQUEST_IMPROBE == (R)->req_recv.req_base.req_type) || \
     (MCA_PML_REQUEST_MPROBE == (R)->req_recv.req_base.req_type))
#define IS_MPROB_REQ(R) \
    ((MCA_PML_REQUEST_IMPROBE == (R)->req_recv.req_base.req_type) || \
     (MCA_PML_REQUEST_MPROBE == (R)->req_recv.req_base.req_type))

static inline void append_recv_req_to_queue(opal_list_t *queue,
        mca_pml_ob1_recv_request_t *req)
{
    if(OPAL_UNLIKELY(req->req_recv.req_base.req_type == MCA_PML_REQUEST_IPROBE ||
                     req->req_recv.req_base.req_type == MCA_PML_REQUEST_IMPROBE))
        return;

    opal_list_append(queue, (opal_list_item_t*)req);

    /**
     * We don't want to generate this kind of event for MPI_Probe. Hopefully,
     * the compiler will optimize out the empty if loop in the case where PERUSE
     * support is not required by the user.
     */
    if(req->req_recv.req_base.req_type != MCA_PML_REQUEST_PROBE ||
       req->req_recv.req_base.req_type != MCA_PML_REQUEST_MPROBE) {
        PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_REQ_INSERT_IN_POSTED_Q,
                                &(req->req_recv.req_base), PERUSE_RECV);
    }
}

/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. This
 *  function has to be called with the communicator matching lock held.
*/
static mca_pml_ob1_recv_frag_t*
recv_req_match_specific_proc( const mca_pml_ob1_recv_request_t *req,
                              mca_pml_ob1_comm_proc_t *proc )
{
    opal_list_t* unexpected_frags = &proc->unexpected_frags;
    opal_list_item_t *i;
    mca_pml_ob1_recv_frag_t* frag;
    int tag = req->req_recv.req_base.req_tag;

    if(opal_list_get_size(unexpected_frags) == 0)
        return NULL;

    if( OMPI_ANY_TAG == tag ) {
        for (i =  opal_list_get_first(unexpected_frags);
             i != opal_list_get_end(unexpected_frags);
             i =  opal_list_get_next(i)) {
            frag = (mca_pml_ob1_recv_frag_t*)i;
            
            if( frag->hdr.hdr_match.hdr_tag >= 0 )
                return frag;
        }
    } else {
        for (i =  opal_list_get_first(unexpected_frags);
             i != opal_list_get_end(unexpected_frags);
             i =  opal_list_get_next(i)) {
            frag = (mca_pml_ob1_recv_frag_t*)i;
            
            if( frag->hdr.hdr_match.hdr_tag == tag )
                return frag;
        }
    }
    return NULL;
}

/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/
static mca_pml_ob1_recv_frag_t*
recv_req_match_wild( mca_pml_ob1_recv_request_t* req,
                     mca_pml_ob1_comm_proc_t **p)
{
    mca_pml_ob1_comm_t* comm = req->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_ob1_comm_proc_t* proc = comm->procs;
    size_t i;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
     *
     * In order to avoid starvation do this in a round-robin fashion.
     */
    for (i = comm->last_probed + 1; i < comm->num_procs; i++) {
        mca_pml_ob1_recv_frag_t* frag;

        /* loop over messages from the current proc */
        if((frag = recv_req_match_specific_proc(req, &proc[i]))) {
            *p = &proc[i];
            comm->last_probed = i;
            req->req_recv.req_base.req_proc = proc[i].ompi_proc;
            prepare_recv_req_converter(req);
            return frag; /* match found */
        }
    }
    for (i = 0; i <= comm->last_probed; i++) {
        mca_pml_ob1_recv_frag_t* frag;

        /* loop over messages from the current proc */
        if((frag = recv_req_match_specific_proc(req, &proc[i]))) {
            *p = &proc[i];
            comm->last_probed = i;
            req->req_recv.req_base.req_proc = proc[i].ompi_proc;
            prepare_recv_req_converter(req);
            return frag; /* match found */
        }
    }

    *p = NULL;
    return NULL;
}


void mca_pml_ob1_recv_req_start(mca_pml_ob1_recv_request_t *req)
{
    mca_pml_ob1_comm_t* comm = req->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_ob1_comm_proc_t* proc;
    mca_pml_ob1_recv_frag_t* frag;
    opal_list_t *queue;
    mca_pml_ob1_hdr_t* hdr;

    /* init/re-init the request */
    req->req_lock = 0;
    req->req_pipeline_depth = 0;
    req->req_bytes_received = 0;
    req->req_bytes_expected = 0;
    /* What about req_rdma_cnt ? */
    req->req_rdma_idx = 0;
    req->req_pending = false;
    req->req_ack_sent = false;

    MCA_PML_BASE_RECV_START(&req->req_recv.req_base);

    OPAL_THREAD_LOCK(&comm->matching_lock);
    /**
     * The laps of time between the ACTIVATE event and the SEARCH_UNEX one include
     * the cost of the request lock.
     */
    PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_SEARCH_UNEX_Q_BEGIN,
                            &(req->req_recv.req_base), PERUSE_RECV);

    /* assign sequence number */
    req->req_recv.req_base.req_sequence = comm->recv_sequence++;

    /* attempt to match posted recv */
    if(req->req_recv.req_base.req_peer == OMPI_ANY_SOURCE) {
        frag = recv_req_match_wild(req, &proc);
        queue = &comm->wild_receives;
#if !OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        /* As we are in a homogeneous environment we know that all remote
         * architectures are exactly the same as the local one. Therefore,
         * we can safely construct the convertor based on the proc
         * information of rank 0.
         */
        if( NULL == frag ) {
            req->req_recv.req_base.req_proc = ompi_proc_local_proc;
            prepare_recv_req_converter(req);
        }
#endif  /* !OPAL_ENABLE_HETEROGENEOUS_SUPPORT */
    } else {
        proc = &comm->procs[req->req_recv.req_base.req_peer];
        req->req_recv.req_base.req_proc = proc->ompi_proc;
        frag = recv_req_match_specific_proc(req, proc);
        queue = &proc->specific_receives;
        /* wild cardrecv will be prepared on match */ 
        prepare_recv_req_converter(req);
    }

    if(OPAL_UNLIKELY(NULL == frag)) {
        PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_SEARCH_UNEX_Q_END,
                                &(req->req_recv.req_base), PERUSE_RECV);
        /* We didn't find any matches.  Record this irecv so we can match
           it when the message comes in. */
        append_recv_req_to_queue(queue, req);
        req->req_match_received = false;
        OPAL_THREAD_UNLOCK(&comm->matching_lock);
    } else {
        if(OPAL_LIKELY(!IS_PROB_REQ(req))) {
            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_REQ_MATCH_UNEX,
                                    &(req->req_recv.req_base), PERUSE_RECV);

            hdr = (mca_pml_ob1_hdr_t*)frag->segments->seg_addr.pval;
            PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_REMOVE_FROM_UNEX_Q,
                                   req->req_recv.req_base.req_comm,
                                   hdr->hdr_match.hdr_src,
                                   hdr->hdr_match.hdr_tag,
                                   PERUSE_RECV);

            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_SEARCH_UNEX_Q_END,
                                    &(req->req_recv.req_base), PERUSE_RECV);

            opal_list_remove_item(&proc->unexpected_frags,
                                  (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            
            switch(hdr->hdr_common.hdr_type) {
            case MCA_PML_OB1_HDR_TYPE_MATCH:
                mca_pml_ob1_recv_request_progress_match(req, frag->btl, frag->segments,
                                                        frag->num_segments);
                break;
            case MCA_PML_OB1_HDR_TYPE_RNDV:
                mca_pml_ob1_recv_request_progress_rndv(req, frag->btl, frag->segments,
                                                       frag->num_segments);
                break;
            case MCA_PML_OB1_HDR_TYPE_RGET:
                mca_pml_ob1_recv_request_progress_rget(req, frag->btl, frag->segments,
                                                       frag->num_segments);
                break;
            default:
                assert(0);
            }
            
            MCA_PML_OB1_RECV_FRAG_RETURN(frag);

        } else if (OPAL_UNLIKELY(IS_MPROB_REQ(req))) {
            /* Remove the fragment from the match list, as it's now
               matched.  Stash it somewhere in the request (which,
               yes, is a complete hack), where it will be plucked out
               during the end of mprobe.  The request will then be
               "recreated" as a receive request, and the frag will be
               restarted with this request during mrecv */
            opal_list_remove_item(&proc->unexpected_frags,
                                  (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&comm->matching_lock);

            req->req_recv.req_base.req_addr = frag;
            mca_pml_ob1_recv_request_matched_probe(req, frag->btl,
                                                   frag->segments, frag->num_segments);

        } else {
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            mca_pml_ob1_recv_request_matched_probe(req, frag->btl,
                                                   frag->segments, frag->num_segments);
        }
    }
}
