/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_rdmafrag.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_endpoint.h"
#include "ompi/mca/bml/base/base.h"
#include "ompi/datatype/dt_arch.h"

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
    if( true == sendreq->req_send.req_base.req_pml_complete ) {
        MCA_PML_OB1_SEND_REQUEST_RETURN( sendreq );
    }

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_NOTIFY,
                             &(sendreq->req_send.req_base), PERUSE_SEND );

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

static void mca_pml_ob1_send_request_destruct(mca_pml_ob1_send_request_t* req)
{
}


OBJ_CLASS_INSTANCE(
    mca_pml_ob1_send_request_t,
    mca_pml_base_send_request_t,
    mca_pml_ob1_send_request_construct,
    mca_pml_ob1_send_request_destruct);

/**
 * Completion of a short message - nothing left to schedule. Note that this
 * function is only called for 0 sized messages.
 */

void mca_pml_ob1_match_completion_cache(
    struct mca_btl_base_module_t* btl,  
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                             &(sendreq->req_send.req_base), PERUSE_SEND );

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* attempt to cache the descriptor */
    MCA_BML_BASE_BTL_DES_RETURN( bml_btl, descriptor ); 

    /* signal request completion */
    MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
}

/**
 * Completion of a short message - nothing left to schedule.
 */

void mca_pml_ob1_match_completion_free(
    struct mca_btl_base_module_t* btl,  
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                             &(sendreq->req_send.req_base), PERUSE_SEND );

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* free the descriptor */
    mca_bml_base_free( bml_btl, descriptor ); 

    /* signal request completion */
    MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
}

/*
 *  Completion of the first fragment of a long message that 
 *  requires an acknowledgement
 */
static void mca_pml_ob1_rndv_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)  descriptor->des_context; 

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                             &(sendreq->req_send.req_base), PERUSE_SEND );

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
                                        sendreq->req_bytes_delivered );

    /* return the descriptor */
    mca_bml_base_free(bml_btl, descriptor); 

    /* advance the request */
    MCA_PML_OB1_SEND_REQUEST_ADVANCE(sendreq);

    /* check for pending requests */
    MCA_PML_OB1_SEND_REQUEST_PROCESS_PENDING();
}


/**
 * Completion of a get request.
 */

static void mca_pml_ob1_rget_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)des->des_cbdata;
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
}


/**
 * Completion of a control message - return resources.
 */

static void mca_pml_ob1_send_ctl_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    /* return the descriptor */
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) descriptor->des_context; 
    mca_bml_base_free(bml_btl, descriptor);
}

/**
 * Completion of additional fragments of a large message - may need
 * to schedule additional fragments.
 */

static void mca_pml_ob1_frag_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
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
    req_bytes_delivered = OPAL_THREAD_ADD_SIZE_T( &sendreq->req_bytes_delivered,
                                                  req_bytes_delivered );
    if (OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1) == 0 &&
        req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
    /*if( OPAL_THREAD_ADD_SIZE_T( &sendreq->req_bytes_delivered, req_bytes_delivered )
        == sendreq->req_send.req_bytes_packed) {*/
        MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
    } else {
        mca_pml_ob1_send_request_schedule(sendreq);
    }

    /* return the descriptor */
    mca_bml_base_free(bml_btl, descriptor);

    /* check for pending requests */
    MCA_PML_OB1_SEND_REQUEST_PROCESS_PENDING();
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
    int32_t free_after;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, sizeof(mca_pml_ob1_rendezvous_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = descriptor->des_src;

    /* pack the data into the BTL supplied buffer */
    iov.iov_base = (void*)((unsigned char*)segment->seg_addr.pval + 
             sizeof(mca_pml_ob1_rendezvous_hdr_t));
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if((rc = ompi_convertor_pack(
        &sendreq->req_send.req_convertor,
        &iov,
        &iov_count,
        &max_data,
        &free_after)) < 0) {
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
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
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

    iov.iov_base = (void*)(((unsigned char*)sendreq->req_send.req_addr) + sendreq->req_send_offset);
    iov.iov_len = max_data = sendreq->req_send.req_bytes_packed - sendreq->req_send_offset;

    if((rc = ompi_convertor_pack(
            &sendreq->req_send.req_convertor,
            &iov,
            &iov_count,
            &max_data,
            &free_after)) < 0) {
            mca_bml_base_free(bml_btl, descriptor);
            return rc;
    }

    /* re-init convertor for packed data */
    ompi_convertor_prepare_for_send(
            &sendreq->req_send.req_convertor,
            sendreq->req_send.req_datatype,
            sendreq->req_send.req_count,
            sendreq->req_send.req_addr);
#if 0
    ompi_convertor_set_position( &sendreq->req_send.req_convertor, 
                                 &sendreq->req_send_offset );
#endif
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
    int32_t free_after;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, sizeof(mca_pml_ob1_match_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = descriptor->des_src;

    max_data = size;
    if(size > 0) { 
        /* pack the data into the supplied buffer */
        iov.iov_base = (void*)((unsigned char*)segment->seg_addr.pval + sizeof(mca_pml_ob1_match_hdr_t));
        iov.iov_len = size;
        iov_count = 1;
        if((rc = ompi_convertor_pack(
                                     &sendreq->req_send.req_convertor,
                                     &iov,
                                     &iov_count,
                                     &max_data,
                                     &free_after)) < 0) {
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
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;

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
    descriptor->des_cbfunc = mca_pml_ob1_match_completion_free;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* signal request completion */
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
            &sendreq->req_send.req_convertor,
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
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;

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
     * FIX - to get the basics working - schedule on the
     * first BTL only
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

         /* prepare source descriptor/segment(s) */
        mca_bml_base_prepare_src(
             bml_btl, 
             reg,
             &sendreq->req_send.req_convertor,
             0,
             &size,
             &src);
         if(NULL == src) {
             return OMPI_ERR_OUT_OF_RESOURCE;
         } 
         src->des_cbfunc = mca_pml_ob1_rget_completion;
         src->des_cbdata = sendreq;

         /* allocate space for get hdr + segment list */
         mca_bml_base_alloc(bml_btl, &des, 
            sizeof(mca_pml_ob1_rget_hdr_t) + (sizeof(mca_btl_base_segment_t)*(src->des_src_cnt-1)));
         if(NULL == des) {
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
         hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
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

         for(i=0; i<src->des_src_cnt; i++)
             hdr->hdr_rget.hdr_segs[i] = src->des_src[i];
         des->des_cbfunc = mca_pml_ob1_send_ctl_completion;

         /**
          * Well, it's a get so we will not know when the peer get the data anyway.
          * If we generate the PERUSE event here, at least we will know when do we
          * sent the GET message ...
          */
         PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                  &(sendreq->req_send.req_base), PERUSE_SEND );

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
         hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
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
         sendreq->req_send_offset = 0;
    
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

int mca_pml_ob1_send_request_start_rndv(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size,
    int flags)
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    if(size == 0) {
        mca_bml_base_alloc(
                           bml_btl, 
                           &des, 
                           sizeof(mca_pml_ob1_rendezvous_hdr_t)
                           ); 
    } else {
        mca_bml_base_prepare_src(
                                 bml_btl, 
                                 NULL,
                                 &sendreq->req_send.req_convertor,
                                 sizeof(mca_pml_ob1_rendezvous_hdr_t),
                                 &size,
                                 &des);
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
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
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
 *  costs of the rdma.
 */

int mca_pml_ob1_send_request_schedule(mca_pml_ob1_send_request_t* sendreq)
{ 
    /*
     * Only allow one thread in this routine for a given request.
     * However, we cannot block callers on a mutex, so simply keep track
     * of the number of times the routine has been called and run through
     * the scheduling logic once for every call.
    */
    
    mca_bml_base_endpoint_t* bml_endpoint = sendreq->req_endpoint;
    
    if(OPAL_THREAD_ADD32(&sendreq->req_lock,1) == 1) {
        do {
            /* allocate remaining bytes to BTLs */
            size_t bytes_remaining = sendreq->req_rdma_offset - sendreq->req_send_offset;
            while(bytes_remaining > 0 && 
                  (sendreq->req_pipeline_depth < mca_pml_ob1.send_pipeline_depth ||
                   sendreq->req_rdma_offset < sendreq->req_send.req_bytes_packed)) {
                
                mca_pml_ob1_frag_hdr_t* hdr;
                mca_btl_base_descriptor_t* des;
                int rc;
                size_t size; 
                mca_bml_base_btl_t* bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_send); 
                size_t num_btl_avail = bml_endpoint->btl_send.arr_size; 
                
                if(num_btl_avail == 1 || bytes_remaining < bml_btl->btl_min_send_size) {
                    size = bytes_remaining;
                } else {
                    /* otherwise attempt to give the BTL a percentage of the message
                     * based on a weighting factor. for simplicity calculate this as
                     * a percentage of the overall message length (regardless of amount
                     * previously assigned)
                     */
                    size = (size_t)(bml_btl->btl_weight * bytes_remaining);
                } 

                /* makes sure that we don't exceed BTL max send size */
                if (bml_btl->btl_max_send_size != 0 && 
                    size > bml_btl->btl_max_send_size - sizeof(mca_pml_ob1_frag_hdr_t)) {
                    size = bml_btl->btl_max_send_size - sizeof(mca_pml_ob1_frag_hdr_t);
#if defined(GEORGE_HAVE_TO_MAKE_SURE_THAT_WE_DONT_NEED_IT)
                    /* very expensive - however for buffered sends we need to send on a 
                     * boundary that the receiver will be able to unpack completely
                     * using the native datatype
                     */
                    if(sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {
                        ompi_convertor_t convertor;
                        size_t position = sendreq->req_send_offset + size;
                        /*
                         * We need this local convertor in order to correctly compute
                         * the correct position. Therefore we have to correctly construct and
                         * destruct it.
                         */
                        OBJ_CONSTRUCT( &convertor, ompi_convertor_t );
                        ompi_convertor_copy_and_prepare_for_send(
                            &sendreq->req_send.req_convertor,
                            sendreq->req_send.req_base.req_datatype,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_addr,
                            0,
                            &convertor);
                        ompi_convertor_set_position(&convertor, &position);
                        OBJ_DESTRUCT( &convertor );
                        size = position - sendreq->req_send_offset;
                    }
#endif
                }
                
                /* pack into a descriptor */
                ompi_convertor_set_position(&sendreq->req_send.req_convertor, 
                                            &sendreq->req_send_offset);

                mca_bml_base_prepare_src(
                                         bml_btl, 
                                         NULL, 
                                         &sendreq->req_send.req_convertor,
                                         sizeof(mca_pml_ob1_frag_hdr_t),
                                         &size,
                                         &des
                                         );
                
                if(des == NULL) {
                    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                    opal_list_append(&mca_pml_ob1.send_pending, (opal_list_item_t*)sendreq);
                    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
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
                /* if we are little endian and the remote side is big endian,
                   we're responsible for making sure the data is in network byte
                   order */
                if (sendreq->req_send.req_base.req_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
                    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
                    MCA_PML_OB1_FRAG_HDR_HTON(*hdr);
                }
#endif
#endif

#if OMPI_WANT_PERUSE
                if( 0 != sendreq->req_send_offset ) {
                    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_CONTINUE,
                                             &(sendreq->req_send.req_base), PERUSE_SEND );
                }
#endif  /* OMPI_WANT_PERUSE */

                /* update state */
                sendreq->req_send_offset += size;
                OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,1);

                /* initiate send - note that this may complete before the call returns */
                rc = mca_bml_base_send( bml_btl, des, MCA_BTL_TAG_PML);
                
                if(rc == OMPI_SUCCESS) {
                    bytes_remaining -= size;
                } else {
                    sendreq->req_send_offset -= size;
                    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
                    mca_bml_base_free(bml_btl,des);
                    OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
                    opal_list_append(&mca_pml_ob1.send_pending, (opal_list_item_t*)sendreq);
                    OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                mca_pml_ob1_progress(); 
            }
        } while (OPAL_THREAD_ADD32(&sendreq->req_lock,-1) > 0);
    }
    return OMPI_SUCCESS;
} 


/**
 * Return resources used by the RDMA
 */

static void mca_pml_ob1_fin_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context; 
    
    MCA_PML_OB1_RDMA_FRAG_RETURN(frag);
    MCA_BML_BASE_BTL_DES_RETURN(bml_btl, des);
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
    mca_pml_ob1_send_request_t* sendreq = frag->rdma_req;
    mca_btl_base_descriptor_t* fin;
    mca_pml_ob1_fin_hdr_t* hdr;
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*) des->des_context; 
    int rc;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(status);
        orte_errmgr.abort();
    }

    /* allocate descriptor for fin control message - note that
     * the rdma descriptor cannot be reused as it points directly
     * at the user buffer
     */
    frag->rdma_state = MCA_PML_OB1_RDMA_FIN;

    MCA_PML_OB1_DES_ALLOC(bml_btl, fin, sizeof(mca_pml_ob1_fin_hdr_t));
    if(NULL == fin) {
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        goto cleanup;
    }
    fin->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    fin->des_cbfunc = mca_pml_ob1_fin_completion;
    fin->des_cbdata = frag;

    /* fill in header */
    hdr = (mca_pml_ob1_fin_hdr_t*)fin->des_src->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FIN;
    hdr->hdr_des = frag->rdma_hdr.hdr_rdma.hdr_des;

#if OMPI_ENABLE_HETEROGENEOUS_SUPPORT
#ifdef WORDS_BIGENDIAN
    hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
#else
    /* if we are little endian and the remote side is big endian,
       we're responsible for making sure the data is in network byte
       order */
    if (sendreq->req_send.req_base.req_proc->proc_arch & OMPI_ARCH_ISBIGENDIAN) {
        hdr->hdr_common.hdr_flags |= MCA_PML_OB1_HDR_FLAGS_NBO;
        MCA_PML_OB1_FIN_HDR_HTON(*hdr);
    }
#endif
#endif

    /* queue request */
    rc = mca_bml_base_send(
                           bml_btl,
                           fin,
                           MCA_BTL_TAG_PML
                           );
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, fin);
        if(rc == OMPI_ERR_OUT_OF_RESOURCE) {
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
        goto cleanup;
    }

    /* check for request completion */
    if( OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, frag->rdma_length)
        >= sendreq->req_send.req_bytes_packed) {
        MCA_PML_OB1_SEND_REQUEST_PML_COMPLETE(sendreq);
    }

cleanup:
    /* return rdma descriptor - do this after queuing the fin message - as 
     * release rdma resources (unpin memory) can take some time.
     */
    des->des_dst = NULL; 
    des->des_dst_cnt = 0; 
    mca_bml_base_free(bml_btl,  des);

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
    mca_mpool_base_registration_t* reg = NULL;
    mca_bml_base_btl_t* bml_btl;
    mca_btl_base_descriptor_t* des;
    mca_pml_ob1_rdma_frag_t* frag;
    size_t offset = hdr->hdr_rdma_offset;
    size_t i, size = 0;
    int rc;
    bool release = false; 
    
    bml_btl = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);  
    MCA_PML_OB1_RDMA_FRAG_ALLOC(frag, rc); 
    if(NULL == frag) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(rc);
        orte_errmgr.abort();
    }

    /* setup fragment */
    for(i=0; i<hdr->hdr_seg_cnt; i++) {
        size += hdr->hdr_segs[i].seg_len;
        frag->rdma_segs[i] = hdr->hdr_segs[i];
    }
    frag->rdma_hdr.hdr_rdma = *hdr;
    frag->rdma_req = sendreq; 
    frag->rdma_ep = bml_endpoint;
    frag->rdma_state = MCA_PML_OB1_RDMA_PREPARE;

    /* lookup the corresponding registration */
    for(i=0; i<sendreq->req_rdma_cnt; i++) {
       if(sendreq->req_rdma[i].bml_btl == bml_btl) {
           reg = sendreq->req_rdma[i].btl_reg;
           break;
       }
    } 

    /* set convertor at current offset */
    ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset);

    /* if registration doesnt exist - create one */
    if (mca_pml_ob1.leave_pinned_pipeline && reg == NULL) {
        unsigned char* base;
        long lb;
        ompi_ddt_type_lb(sendreq->req_send.req_convertor.pDesc, &lb);
        base = (unsigned char*)sendreq->req_send.req_convertor.pBaseBuf + lb + offset;
        reg = mca_pml_ob1_rdma_register(bml_btl, base, size);
        release = true;
    }
    
    /* setup descriptor */
    mca_bml_base_prepare_src(
        bml_btl, 
        reg,
        &sendreq->req_send.req_convertor, 
        0,
        &size, 
        &des
        );
    
    if(NULL == des) { 
        OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
        opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
        OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
    }
    
    if(release == true && bml_btl->btl_mpool) {
        bml_btl->btl_mpool->mpool_release(bml_btl->btl_mpool, reg);
    }
    
    frag->rdma_state = MCA_PML_OB1_RDMA_PUT;
    frag->rdma_length = size;

    des->des_dst = frag->rdma_segs;
    des->des_dst_cnt = hdr->hdr_seg_cnt;
    des->des_cbfunc = mca_pml_ob1_put_completion;
    des->des_cbdata = frag;

    if(OMPI_SUCCESS != (rc = mca_bml_base_put(bml_btl, des))) {
        if(rc == OMPI_ERR_OUT_OF_RESOURCE) {
            OPAL_THREAD_LOCK(&mca_pml_ob1.lock);
            opal_list_append(&mca_pml_ob1.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_ob1.lock);
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
    }
}


