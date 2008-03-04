/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "opal/prefetch.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvreq.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/datatype/dt_arch.h"


void mca_pml_ob1_send_request_process_pending(mca_bml_base_btl_t *bml_btl)
{
    int i, s = opal_list_get_size(&mca_pml_ob1.send_pending);

    /* advance pending requests */
    for(i = 0; i < s; i++) {
        mca_pml_ob1_send_pending_t pending_type;
        mca_pml_ob1_send_request_t* sendreq;
        mca_bml_base_btl_t *send_dst;
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        sendreq = (mca_pml_ob1_send_request_t*)
            opal_list_remove_first(&mca_pml_ob1.send_pending);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        if(NULL == sendreq)
            break;
        pending_type = sendreq->req_pending;
        sendreq->req_pending = MCA_PML_OB1_SEND_PENDING_NONE;
        switch(pending_type) {
        case MCA_PML_OB1_SEND_PENDING_SCHEDULE:
            if(mca_pml_ob1_send_request_schedule_exclusive(sendreq) ==
                    OMPI_ERR_OUT_OF_RESOURCE) {
                return;
            }
            break;
        case MCA_PML_OB1_SEND_PENDING_START:
            send_dst = mca_bml_base_btl_array_find(
                    &sendreq->req_endpoint->btl_eager, bml_btl->btl);
            if(NULL == send_dst ||
                    mca_pml_ob1_send_request_start_btl(sendreq, send_dst) ==
                    OMPI_ERR_OUT_OF_RESOURCE) {
                    /* if dst of this sendreq cannot be reached through the
                     * endpoint or no resources put request back on the list */
                    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                    sendreq->req_pending = MCA_PML_OB1_SEND_PENDING_START;
                    if(NULL == send_dst) {
                        opal_list_append(&mca_pml_ob1.send_pending,
                                (opal_list_item_t*)sendreq);
                    } else {
                        /* prepend to the pending list to minimaze reordering */
                        opal_list_prepend(&mca_pml_ob1.send_pending,
                                (opal_list_item_t*)sendreq);
                    }
                    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    /* if no destination try next request otherwise give up,
                     * no more resources on this btl */
                    if(send_dst != NULL)
                        return;
            }
            break;
        default:
            opal_output(0, "[%s:%d] wrong send request type\n",
                    __FILE__, __LINE__);
            break;
        }
    }
}

/*
 * The free call mark the final stage in a request life-cycle. Starting from this
 * point the request is completed at both PML and user level, and can be used
 * for others p2p communications. Therefore, in the case of the OB1 PML it should
 * be added to the free request list.
 */
static int mca_pml_ob1_send_request_free(struct ompi_request_t** request)
{
    mca_pml_ob1_send_request_t* sendreq = *(mca_pml_ob1_send_request_t**)request;
    
    assert( false == sendreq->req_send.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    sendreq->req_send.req_base.req_free_called = true;

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_NOTIFY,
                             &(sendreq->req_send.req_base), PERUSE_SEND );

    if( true == sendreq->req_send.req_base.req_pml_complete ) {
        MCA_PML_OB1_SEND_REQUEST_RETURN( sendreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_send_request_cancel(struct ompi_request_t* request, int complete)
{
    /* we dont cancel send requests by now */
    return OMPI_SUCCESS;
}

static void mca_pml_ob1_send_request_construct(mca_pml_ob1_send_request_t* req)
{
    req->req_send.req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_send.req_base.req_ompi.req_free = mca_pml_ob1_send_request_free;
    req->req_send.req_base.req_ompi.req_cancel = mca_pml_ob1_send_request_cancel;
    req->req_rdma_cnt = 0;
}

OBJ_CLASS_INSTANCE( mca_pml_ob1_send_request_t,
                    mca_pml_base_send_request_t,
                    mca_pml_ob1_send_request_construct,
                    NULL );

/**
 * Completion of a short message - nothing left to schedule. Note that this
 * function is only called for 0 sized messages.
 */

static void
mca_pml_ob1_match_completion_cache( struct mca_btl_base_module_t* btl,  
                                    struct mca_btl_base_endpoint_t* ep,
                                    struct mca_btl_base_descriptor_t* descriptor,
                                    int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 

    if( sendreq->req_send.req_bytes_packed > 0 ) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                 &(sendreq->req_send.req_base), PERUSE_SEND );
    }

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* signal normal request completion */
    if (OPAL_UNLIKELY(!mca_pml_ob1.use_early_completion)) {
        OPAL_THREAD_LOCK(&ompi_request_lock);
        MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

    /* attempt to cache the descriptor */
    MCA_BML_BASE_BTL_DES_RETURN( bml_btl, descriptor ); 

    /* signal request completion */
    MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/**
 * Completion of a short message - nothing left to schedule.
 */

static void
mca_pml_ob1_match_completion_free( struct mca_btl_base_module_t* btl,  
                                   struct mca_btl_base_endpoint_t* ep,
                                   struct mca_btl_base_descriptor_t* descriptor,
                                   int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 

    if( sendreq->req_send.req_bytes_packed > 0 ) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                 &(sendreq->req_send.req_base), PERUSE_SEND );
    }

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* signal normal request completion */
    if (OPAL_UNLIKELY(!mca_pml_ob1.use_early_completion)) {
        OPAL_THREAD_LOCK(&ompi_request_lock);
        MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

    /* free the descriptor */
    mca_bml_base_free( bml_btl, descriptor ); 

    /* signal request completion */
    MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/*
 *  Completion of the first fragment of a long message that 
 *  requires an acknowledgement
 */
static void
mca_pml_ob1_rndv_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* descriptor,
                             int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)descriptor->des_context;
    size_t req_bytes_delivered = 0;

    if( sendreq->req_send.req_bytes_packed > 0 ) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                 &(sendreq->req_send.req_base), PERUSE_SEND );
    }

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* count bytes of user data actually delivered. As the rndv completion only
     * happens in one thread, the increase of the req_bytes_delivered does not
     * have to be atomic.
     */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( descriptor->des_src,
                                        descriptor->des_src_cnt,
                                        sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                        req_bytes_delivered );

    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered,
                           req_bytes_delivered);
    /* return the descriptor */
    mca_bml_base_free(bml_btl, descriptor); 

    /* advance the request */
    if(OPAL_THREAD_ADD32(&sendreq->req_state, 1) == 2 &&
       sendreq->req_bytes_delivered >= sendreq->req_send.req_bytes_packed) {
        if(!sendreq->req_send.req_base.req_pml_complete){
            /* We must check that completion hasn't already occured */
            /*  for the self BTL we may choose the RDMA PUT protocol */
            /*  on the send side, in  this case we send no eager data */
            /*  if, on the receiver side the data is not contiguous we  */
            /*  may choose to use the copy in/out protocol */ 
            /*  if this occurs, the entire request can be completed in a */
            /*  single call to mca_pml_ob1_recv_request_ack */ 
            /*  as soon as the last fragment of the copy in/out protocol */ 
            /*  gets local completion. This doesn't occur in the general */
            /*  case of the copy in/out protocol because when both sender */
            /*  and receiver agree on the copy in/out protoocol we eagerly */
            /*  send data, we don't update the request with this eagerly sent */
            /*  data until here in this function, so completion could not have */
            /*  yet occurred. */
            MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
        }
    }
    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}


/**
 * Completion of a get request.
 */

static void
mca_pml_ob1_rget_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* des,
                             int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    size_t req_bytes_delivered = 0;

    /* count bytes of user data actually delivered and check for request completion */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( des->des_src, des->des_src_cnt,
                                        0, req_bytes_delivered );
    if( OPAL_THREAD_ADD_SIZE_T( &sendreq->req_bytes_delivered, req_bytes_delivered )
        == sendreq->req_send.req_bytes_packed ) {
        MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
    }

    /* release resources */
    btl->btl_free(btl,des);
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}


/**
 * Completion of a control message - return resources.
 */

static void
mca_pml_ob1_send_ctl_completion( mca_btl_base_module_t* btl,
                                 struct mca_btl_base_endpoint_t* ep,
                                 struct mca_btl_base_descriptor_t* descriptor,
                                 int status )
{
    /* return the descriptor */
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 
    mca_bml_base_free(bml_btl, descriptor);

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

/**
 * Completion of additional fragments of a large message - may need
 * to schedule additional fragments.
 */

static void
mca_pml_ob1_frag_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* descriptor,
                             int status )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context;
    size_t req_bytes_delivered = 0;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* count bytes of user data actually delivered */
    MCA_PML_OB1_COMPUTE_SEGMENT_LENGTH( descriptor->des_src,
                                        descriptor->des_src_cnt,
                                        sizeof(mca_pml_ob1_frag_hdr_t),
                                        req_bytes_delivered );

    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth, -1);

    /* return the descriptor */
    mca_bml_base_free(bml_btl, descriptor);

    if(OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered,
                req_bytes_delivered) == sendreq->req_send.req_bytes_packed) {
        MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
    } else {
        mca_pml_ob1_send_request_schedule(sendreq);
    }

    /* check for pending requests */
    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}



/**
 *  Buffer the entire message and mark as complete.
 */

int mca_pml_ob1_send_request_start_buffered(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, sizeof(mca_pml_ob1_rendezvous_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = descriptor->des_src;

    /* pack the data into the BTL supplied buffer */
    iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval + 
                                    sizeof(mca_pml_ob1_rendezvous_hdr_t));
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if((rc = ompi_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                  &iov,
                                  &iov_count,
                                  &max_data)) < 0) {
        mca_bml_base_free(bml_btl, descriptor);
        return rc;
    }

    /* build rendezvous header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
    /* if we are little endian and the remote side is big endian,
       we're responsible for making sure the data is in network byte
       order */
    if (sendreq->req_send.req_base.req_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
        MCA_PML_OB1_RNDV_HDR_HTON(hdr->hdr_rndv);
    }
#endif
#endif

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t) + max_data;
    sendreq->req_send_offset = max_data;

    descriptor->des_cbfunc = mca_pml_ob1_rndv_completion;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* buffer the remainder of the message */
    rc = mca_pml_base_bsend_request_alloc((ompi_request_t*)sendreq);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor);
        return rc;
    }

    iov.iov_base = (IOVBASE_TYPE*)(((unsigned char*)sendreq->req_send.req_addr) + sendreq->req_send_offset);
    iov.iov_len = max_data = sendreq->req_send.req_bytes_packed - sendreq->req_send_offset;

    if((rc = ompi_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                  &iov,
                                  &iov_count,
                                  &max_data)) < 0) {
        mca_bml_base_free(bml_btl, descriptor);
        return rc;
    }

    /* re-init convertor for packed data */
    ompi_convertor_prepare_for_send( &sendreq->req_send.req_base.req_convertor,
                                     MPI_BYTE,
                                     sendreq->req_send.req_bytes_packed,
                                     sendreq->req_send.req_addr );
    /* request is complete at mpi level */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* send */
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    }
    return rc;
}


/**
 *  BTL requires "specially" allocated memory. Request a segment that
 *  is used for initial hdr and any eager data. This is used only from
 *  the _START macro.
 */

int mca_pml_ob1_send_request_start_copy(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int rc;

    /* allocate descriptor */
    if(0==size)
        MCA_PML_OB1_DES_ALLOC(bml_btl, descriptor,
                sizeof(mca_pml_ob1_match_hdr_t));
    else
        mca_bml_base_alloc(bml_btl, &descriptor,
                sizeof(mca_pml_ob1_match_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = descriptor->des_src;

    max_data = size;
    if(size > 0) { 
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval + sizeof(mca_pml_ob1_match_hdr_t));
        iov.iov_len = size;
        iov_count = 1;
        if((rc = ompi_convertor_pack(
                                     &sendreq->req_send.req_base.req_convertor,
                                     &iov,
                                     &iov_count,
                                     &max_data)) < 0) {
            mca_bml_base_free(bml_btl, descriptor);
            return rc;
        }
    }
    
    /* build match header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
    /* if we are little endian and the remote side is big endian,
       we're responsible for making sure the data is in network byte
       order */
    if (sendreq->req_send.req_base.req_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
        MCA_PML_OB1_MATCH_HDR_HTON(hdr->hdr_match);
    }
#endif
#endif

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t) + max_data;
    sendreq->req_send_offset = max_data;
    sendreq->req_rdma_offset = max_data;

    /* short message */
    descriptor->des_cbfunc = size?mca_pml_ob1_match_completion_free:
        mca_pml_ob1_match_completion_cache;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* signal early request completion */
    if (OPAL_LIKELY(mca_pml_ob1.use_early_completion)) {
        OPAL_THREAD_LOCK(&ompi_request_lock);
        MCA_PML_OB1_SEND_REQUEST_MPI_COMPLETE(sendreq);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }

    /* send */
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        if(0==size)
            MCA_BML_BASE_BTL_DES_RETURN(bml_btl, descriptor);
        else
            mca_bml_base_free(bml_btl, descriptor );
    } 
    return rc;
}

/**
 *  BTL can send directly from user buffer so allow the BTL
 *  to prepare the segment list. Start sending a small message.
 */

int mca_pml_ob1_send_request_start_prepare(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    mca_bml_base_prepare_src(
            bml_btl,
            NULL,
            &sendreq->req_send.req_base.req_convertor,
            sizeof(mca_pml_ob1_match_hdr_t),
            &size,
            &descriptor);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    segment = descriptor->des_src;

    /* build match header */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
    /* if we are little endian and the remote side is big endian,
       we're responsible for making sure the data is in network byte
       order */
    if (sendreq->req_send.req_base.req_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
        MCA_PML_OB1_MATCH_HDR_HTON(hdr->hdr_match);
    }
#endif
#endif

    /* short message */
    descriptor->des_cbfunc = mca_pml_ob1_match_completion_free;

    /* update lengths */
    sendreq->req_send_offset = size;
    sendreq->req_rdma_offset = size;

    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML); 
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    }
    return rc;
}


/**
 *  We have contigous data that is registered - schedule across
 *  available nics.
 */

int mca_pml_ob1_send_request_start_rdma(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    /*
     * When req_rdma array is constructed the firs element of the array always
     * assigned different btl in round robin fashion (if there are more than
     * one RDMA capable BTLs). This way round robin distribution of RDMA
     * operation is achieved.
     */

    mca_mpool_base_registration_t* reg = sendreq->req_rdma[0].btl_reg;
    mca_btl_base_descriptor_t* src;
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    size_t i;
    int rc;


    bml_btl = sendreq->req_rdma[0].bml_btl;
    if(sendreq->req_rdma_cnt == 1 &&
       bml_btl->btl_flags & MCA_BTL_FLAGS_GET) {
        size_t old_position = sendreq->req_send.req_base.req_convertor.bConverted;

        /* prepare source descriptor/segment(s) */
        mca_bml_base_prepare_src( bml_btl, 
                                  reg,
                                  &sendreq->req_send.req_base.req_convertor,
                                  0,
                                  &size,
                                  &src );
        if(NULL == src) {
            ompi_convertor_set_position(&sendreq->req_send.req_base.req_convertor,
                                        &old_position);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } 
        src->des_cbfunc = mca_pml_ob1_rget_completion;
        src->des_cbdata = sendreq;

        /* allocate space for get hdr + segment list */
        mca_bml_base_alloc(bml_btl, &des, 
                           sizeof(mca_pml_ob1_rget_hdr_t) + (sizeof(mca_btl_base_segment_t)*(src->des_src_cnt-1)));
        if(NULL == des) {
            ompi_convertor_set_position(&sendreq->req_send.req_base.req_convertor,
                                        &old_position);
            mca_bml_base_free(bml_btl, src);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        segment = des->des_src;

        /* build match header */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_CONTIG|MCA_PML_OB1_HDR_FLAGS_PIN;
        hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RGET;
        hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
        hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_rndv.hdr_src_req.pval = sendreq;
        hdr->hdr_rget.hdr_des.pval = src;
        hdr->hdr_rget.hdr_seg_cnt = src->des_src_cnt;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
         hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
        /* if we are little endian and the remote side is big endian,
           we're responsible for making sure the data is in network byte
           order */
        /* RDMA is currently disabled by bml if arch doesn't
           match, so this shouldn't be needed.  here to make sure
           we remember if we ever change the bml. */
        assert(0 == (sendreq->req_send.req_base.req_proc->proc_arch & 
                     OMPI_ARCH_ISBIGENDIAN));
#endif
#endif

        for( i = 0; i < src->des_src_cnt; i++ ) {
            hdr->hdr_rget.hdr_segs[i].seg_addr.lval = ompi_ptr_ptol(src->des_src[i].seg_addr.pval);
            hdr->hdr_rget.hdr_segs[i].seg_len       = src->des_src[i].seg_len;
            hdr->hdr_rget.hdr_segs[i].seg_key.key64 = src->des_src[i].seg_key.key64;
        }

        des->des_cbfunc = mca_pml_ob1_send_ctl_completion;

        /**
         * Well, it's a get so we will not know when the peer get the data anyway.
         * If we generate the PERUSE event here, at least we will know when do we
         * sent the GET message ...
         */
        if( sendreq->req_send.req_bytes_packed > 0 ) {
            PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                     &(sendreq->req_send.req_base), PERUSE_SEND );
        }

    } else {

        /* allocate a rendezvous header - dont eager send any data 
         * receiver will schedule rdma put(s) of the entire message
         */

        mca_bml_base_alloc(bml_btl, &des, sizeof(mca_pml_ob1_rendezvous_hdr_t));
        if(NULL == des) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        segment = des->des_src;
            
         /* build hdr */
         hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
         hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_CONTIG|MCA_PML_OB1_HDR_FLAGS_PIN;
         hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
         hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
         hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
         hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
         hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
         hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
         hdr->hdr_rndv.hdr_src_req.pval = sendreq;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
         hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
         /* if we are little endian and the remote side is big endian,
            we're responsible for making sure the data is in network byte
            order */
         if (sendreq->req_send.req_base.req_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
             hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
             MCA_PML_OB1_RNDV_HDR_HTON(hdr->hdr_rndv);
         }
#endif
#endif

         /* update lengths with number of bytes actually packed */
         segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t);
         sendreq->req_rdma_offset = 0;
    
         /* first fragment of a long message */
         des->des_cbfunc = mca_pml_ob1_rndv_completion;
    }

    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, des);
    }
    return rc;
}


/**
 *  Rendezvous is required. Not doing rdma so eager send up to
 *  the btls eager limit.
 */

int mca_pml_ob1_send_request_start_rndv( mca_pml_ob1_send_request_t* sendreq,
                                         mca_bml_base_btl_t* bml_btl,
                                         size_t size,
                                         int flags )
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    if(size == 0) {
        mca_bml_base_alloc( bml_btl, 
                            &des, 
                            sizeof(mca_pml_ob1_rendezvous_hdr_t) ); 
    } else {
        mca_bml_base_prepare_src( bml_btl, 
                                  NULL,
                                  &sendreq->req_send.req_base.req_convertor,
                                  sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                  &size,
                                  &des );
    }

    if(NULL == des) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = des->des_src;

    /* build hdr */
    hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
    /* if we are little endian and the remote side is big endian,
       we're responsible for making sure the data is in network byte
       order */
    if (sendreq->req_send.req_base.req_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
        MCA_PML_OB1_RNDV_HDR_HTON(hdr->hdr_rndv);
    }
#endif
#endif

    /* first fragment of a long message */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbdata = sendreq;
    des->des_cbfunc = mca_pml_ob1_rndv_completion;
    sendreq->req_send_offset = size;
    sendreq->req_rdma_offset = size;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, des );
    }
    return rc;
}


/**
 *  Schedule pipeline of send descriptors for the given request.
 *  Up to the rdma threshold. If this is a send based protocol,
 *  the rdma threshold is the end of the message. Otherwise, schedule
 *  fragments up to the threshold to overlap initial registration/setup
 *  costs of the rdma. Only one thread can be inside this function.
 */

int mca_pml_ob1_send_request_schedule_exclusive(
        mca_pml_ob1_send_request_t* sendreq)
{ 
    mca_bml_base_endpoint_t* bml_endpoint = sendreq->req_endpoint;
    size_t num_btl_avail =
        mca_bml_base_btl_array_get_size(&bml_endpoint->btl_send);

    do {
        /* allocate remaining bytes to BTLs */
        size_t bytes_remaining = sendreq->req_rdma_offset -
            sendreq->req_send_offset;
        size_t prev_bytes_remaining = 0, num_fail = 0;

        if(bytes_remaining == 0) {
            OPAL_THREAD_ADD32(&sendreq->req_lock, -sendreq->req_lock);
            return OMPI_SUCCESS;
        }
        while((int32_t)bytes_remaining > 0 &&
                (sendreq->req_pipeline_depth < mca_pml_ob1.send_pipeline_depth
                 ||
                 sendreq->req_rdma_offset < sendreq->req_send.req_bytes_packed))
        {
            mca_pml_ob1_frag_hdr_t* hdr;
            mca_btl_base_descriptor_t* des;
            int rc;
            size_t size; 
            mca_bml_base_btl_t* bml_btl =
                mca_bml_base_btl_array_get_next(&bml_endpoint->btl_send); 
               
            if(prev_bytes_remaining == bytes_remaining)
                num_fail++;
            else
                num_fail = 0;

            prev_bytes_remaining = bytes_remaining;

            if (num_fail == num_btl_avail) {
                assert(sendreq->req_pending == MCA_PML_OB1_SEND_PENDING_NONE);
                OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                sendreq->req_pending = MCA_PML_OB1_SEND_PENDING_SCHEDULE;
                opal_list_append(&mca_pml_ob1.send_pending,
                        (opal_list_item_t*)sendreq);
                OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            if(num_btl_avail == 1 ||
                    bytes_remaining < bml_btl->btl_min_send_size) {
                size = bytes_remaining;
            } else {
                /* otherwise attempt to give the BTL a percentage of the message
                 * based on a weighting factor. for simplicity calculate this as
                 * a percentage of the overall message length (regardless of
                 * amount previously assigned)
                 */
                    size = (size_t)(bml_btl->btl_weight * bytes_remaining);
            } 

            /* makes sure that we don't exceed BTL max send size */
            if (bml_btl->btl_max_send_size != 0 &&
                    size > (bml_btl->btl_max_send_size -
                        sizeof(mca_pml_ob1_frag_hdr_t))) {
                size = bml_btl->btl_max_send_size -
                    sizeof(mca_pml_ob1_frag_hdr_t);
            }
                
            /* pack into a descriptor */
            ompi_convertor_set_position(&sendreq->req_send.req_base.req_convertor, 
                                        &sendreq->req_send_offset);

            mca_bml_base_prepare_src(bml_btl, NULL,
                    &sendreq->req_send.req_base.req_convertor,
                    sizeof(mca_pml_ob1_frag_hdr_t), &size, &des);
                
            if(des == NULL) {
                continue;
            }
            des->des_cbfunc = mca_pml_ob1_frag_completion;
            des->des_cbdata = sendreq;

            /* setup header */
            hdr = (mca_pml_ob1_frag_hdr_t*)des->des_src->seg_addr.pval;
            hdr->hdr_common.hdr_flags = 0;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FRAG;
            hdr->hdr_frag_offset = sendreq->req_send_offset;
            hdr->hdr_src_req.pval = sendreq;
            hdr->hdr_dst_req = sendreq->req_recv;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
            hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
            /*
             * if we are little endian and the remote side is big endian,
             *  we're responsible for making sure the data is in network byte
             *  order
             */
            if(sendreq->req_send.req_base.req_proc->proc_arch &
                    OMPI_ARCH_ISBIGENDIAN) {
                hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
                MCA_PML_OB1_FRAG_HDR_HTON(*hdr);
            }
#endif
#endif

            /*
             * The if-clause should be optimized away, in case the macro
             * extends to ;
             */
#if OMPI_WANT_PERUSE
            if( 0 != sendreq->req_send_offset ) {
                PERUSE_TRACE_COMM_OMPI_EVENT(PERUSE_COMM_REQ_XFER_CONTINUE,
                                             &(sendreq->req_send.req_base),
                                             size, PERUSE_SEND);
            }
#endif  /* OMPI_WANT_PERUSE */

            /* initiate send - note that this may complete before the call returns */
            rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
                
            if(rc == OMPI_SUCCESS) {
                bytes_remaining -= size;
                /* update state */
                sendreq->req_send_offset += size;
                OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth, 1);
            } else { 
                mca_bml_base_free(bml_btl,des);
                continue;
            }
            mca_bml.bml_progress();
        }
    } while (OPAL_THREAD_ADD32(&sendreq->req_lock,-1) > 0);

    return OMPI_SUCCESS;
} 


/**
 *  An RDMA put operation has completed:
 *  (1) Update request status and if required set completed
 *  (2) Send FIN control message to the destination 
 */

static void mca_pml_ob1_put_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)frag->rdma_req;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(status);
        orte_errmgr.abort();
    }

    mca_pml_ob1_send_fin(sendreq->req_send.req_base.req_proc, 
            frag->rdma_hdr.hdr_rdma.hdr_des.pval, bml_btl);

    /* check for request completion */
    if( OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, frag->rdma_length)
        >= sendreq->req_send.req_bytes_packed) {

        /* if we've got completion on rndv packet */
        if (sendreq->req_state == 2) {
            MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
        }
    }

    MCA_PML_OB1_RDMA_FRAG_RETURN(frag);
    /* return rdma descriptor - do this after queuing the fin message - as 
     * release rdma resources (unpin memory) can take some time.
     */
    des->des_dst = NULL;
    des->des_dst_cnt = 0;
    mca_bml_base_free(bml_btl, des);

    MCA_PML_OB1_PROGRESS_PENDING(bml_btl);
}

int mca_pml_ob1_send_request_put_frag(
        mca_pml_ob1_rdma_frag_t* frag
        )
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)frag->rdma_req;
    mca_mpool_base_registration_t* reg = NULL;
    mca_bml_base_btl_t* bml_btl;
    mca_btl_base_descriptor_t* des;
    size_t offset = (size_t)frag->rdma_hdr.hdr_rdma.hdr_rdma_offset;
    size_t i, save_size = frag->rdma_length;
    int rc;

    bml_btl = mca_bml_base_btl_array_find(&frag->rdma_ep->btl_rdma,
            frag->rdma_btl);  
    
    /* lookup the corresponding registration */
    for(i=0; i<sendreq->req_rdma_cnt; i++) {
       if(sendreq->req_rdma[i].bml_btl == bml_btl) {
           reg = sendreq->req_rdma[i].btl_reg;
           break;
       }
    } 

    /* set convertor at current offset */
    ompi_convertor_set_position(&sendreq->req_send.req_base.req_convertor, &offset);

    /* setup descriptor */
    mca_bml_base_prepare_src(
        bml_btl, 
        reg,
        &sendreq->req_send.req_base.req_convertor, 
        0,
        &frag->rdma_length, 
        &des
        );
    
    if(NULL == des) {
        frag->rdma_length = save_size; 
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    des->des_dst = frag->rdma_segs;
    des->des_dst_cnt = frag->rdma_hdr.hdr_rdma.hdr_seg_cnt;
    des->des_cbfunc = mca_pml_ob1_put_completion;
    des->des_cbdata = frag;

    PERUSE_TRACE_COMM_OMPI_EVENT( PERUSE_COMM_REQ_XFER_CONTINUE,
                                  &(sendreq->req_send.req_base), save_size, PERUSE_SEND );

    if(OMPI_SUCCESS != (rc = mca_bml_base_put(bml_btl, des))) {
        mca_bml_base_free(bml_btl, des);
        frag->rdma_length = save_size;
        if(OMPI_ERR_OUT_OF_RESOURCE == rc) {
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending,
                    (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
    }
    return OMPI_SUCCESS;
}

/**
 *  Receiver has scheduled an RDMA operation:
 *  (1) Allocate an RDMA fragment to maintain the state of the operation
 *  (2) Call BTL prepare_src to pin/prepare source buffers
 *  (3) Queue the RDMA put 
 */

void mca_pml_ob1_send_request_put(
    mca_pml_ob1_send_request_t* sendreq,
    mca_btl_base_module_t* btl, 
    mca_pml_ob1_rdma_hdr_t* hdr)
{
    mca_bml_base_endpoint_t *bml_endpoint = sendreq->req_endpoint;
    mca_pml_ob1_rdma_frag_t* frag;
    int rc;
    size_t i, size = 0;

    if(hdr->hdr_common.hdr_flags & MCA_PML_OB1_HDR_TYPE_ACK) { 
        OPAL_THREAD_ADD32(&sendreq->req_state, 1);
    }

    MCA_PML_OB1_RDMA_FRAG_ALLOC(frag, rc); 

    if(NULL == frag) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(rc);
        orte_errmgr.abort();
    }

    /* setup fragment */
    for( i = 0; i < hdr->hdr_seg_cnt; i++ ) {
        frag->rdma_segs[i].seg_addr.lval = hdr->hdr_segs[i].seg_addr.lval;
        frag->rdma_segs[i].seg_len       = hdr->hdr_segs[i].seg_len;
        frag->rdma_segs[i].seg_key.key64 = hdr->hdr_segs[i].seg_key.key64;
        size += frag->rdma_segs[i].seg_len;
    }

    frag->rdma_hdr.hdr_rdma = *hdr;
    frag->rdma_req = sendreq; 
    frag->rdma_ep = bml_endpoint;
    frag->rdma_btl = btl;
    frag->rdma_length = size;
    frag->rdma_state = MCA_PML_OB1_RDMA_PUT;

    mca_pml_ob1_send_request_put_frag(frag);
}
