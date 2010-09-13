/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
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
#include "pml_bfo.h"
#include "pml_bfo_hdr.h"
#include "pml_bfo_sendreq.h"
#include "pml_bfo_rdmafrag.h"
#include "pml_bfo_recvreq.h"
/* BFO FAILOVER CODE - begin */
#include "pml_bfo_failover.h"
/* BFO FAILOVER CODE - end */
#include "ompi/mca/bml/base/base.h"
#include "ompi/memchecker.h"

OBJ_CLASS_INSTANCE(mca_pml_bfo_send_range_t, ompi_free_list_item_t,
        NULL, NULL);

void mca_pml_bfo_send_request_process_pending(struct mca_btl_base_module_t *btl)
{
    int i, s = opal_list_get_size(&mca_pml_bfo.send_pending);

    /* advance pending requests */
    for(i = 0; i < s; i++) {
        mca_pml_bfo_send_pending_t pending_type = MCA_PML_BFO_SEND_PENDING_NONE;
        mca_pml_bfo_send_request_t* sendreq;
        mca_bml_base_btl_t *send_dst;

        sendreq = get_request_from_send_pending(&pending_type);
        if(OPAL_UNLIKELY(NULL == sendreq))
            break;

        switch(pending_type) {
        case MCA_PML_BFO_SEND_PENDING_SCHEDULE:
            if(OPAL_SOS_GET_ERROR_CODE(mca_pml_bfo_send_request_schedule_exclusive(sendreq)) ==
                    OMPI_ERR_OUT_OF_RESOURCE) {
                return;
            }
            break;
        case MCA_PML_BFO_SEND_PENDING_START:
            send_dst = mca_bml_base_btl_array_find(
                    &sendreq->req_endpoint->btl_eager, btl);
            if( (NULL == send_dst) ||
                (OPAL_SOS_GET_ERROR_CODE(mca_pml_bfo_send_request_start_btl(sendreq, send_dst)) ==
                 OMPI_ERR_OUT_OF_RESOURCE) ) {
                /* prepend to the pending list to minimize reordering in case
                 * send_dst != 0 */
                add_request_to_send_pending(sendreq,
                        MCA_PML_BFO_SEND_PENDING_START, NULL == send_dst);
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
 * for others p2p communications. Therefore, in the case of the BFO PML it should
 * be added to the free request list.
 */
static int mca_pml_bfo_send_request_free(struct ompi_request_t** request)
{
    mca_pml_bfo_send_request_t* sendreq = *(mca_pml_bfo_send_request_t**)request;
    
    assert( false == sendreq->req_send.req_base.req_free_called );

    OPAL_THREAD_LOCK(&ompi_request_lock);
    sendreq->req_send.req_base.req_free_called = true;

    PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_NOTIFY,
                             &(sendreq->req_send.req_base), PERUSE_SEND );

    if( true == sendreq->req_send.req_base.req_pml_complete ) {
        /* make buffer defined when the request is compeleted,
           and before releasing the objects. */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );

        MCA_PML_BFO_SEND_REQUEST_RETURN( sendreq );
    }

    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_bfo_send_request_cancel(struct ompi_request_t* request, int complete)
{
    /* we dont cancel send requests by now */
    return OMPI_SUCCESS;
}

static void mca_pml_bfo_send_request_construct(mca_pml_bfo_send_request_t* req)
{
    req->req_send.req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_send.req_base.req_ompi.req_free = mca_pml_bfo_send_request_free;
    req->req_send.req_base.req_ompi.req_cancel = mca_pml_bfo_send_request_cancel;
    req->req_rdma_cnt = 0;
    req->req_throttle_sends = false;
    OBJ_CONSTRUCT(&req->req_send_ranges, opal_list_t);
    OBJ_CONSTRUCT(&req->req_send_range_lock, opal_mutex_t);
}

static void mca_pml_bfo_send_request_destruct(mca_pml_bfo_send_request_t* req)
{
    OBJ_DESTRUCT(&req->req_send_ranges);
    OBJ_DESTRUCT(&req->req_send_range_lock);
}

OBJ_CLASS_INSTANCE( mca_pml_bfo_send_request_t,
                    mca_pml_base_send_request_t,
                    mca_pml_bfo_send_request_construct,
                    mca_pml_bfo_send_request_destruct );

/**
 * Completion of a short message - nothing left to schedule.
 */

static inline void
mca_pml_bfo_match_completion_free_request( struct mca_btl_base_module_t* btl,  
                                           mca_pml_bfo_send_request_t* sendreq )
{
    if( sendreq->req_send.req_bytes_packed > 0 ) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                 &(sendreq->req_send.req_base), PERUSE_SEND );
    }

    /* signal request completion */
    send_request_pml_complete(sendreq);

    /* check for pending requests */
    MCA_PML_BFO_PROGRESS_PENDING(btl);
}

static void
mca_pml_bfo_match_completion_free( struct mca_btl_base_module_t* btl,  
                                   struct mca_btl_base_endpoint_t* ep,
                                   struct mca_btl_base_descriptor_t* des,
                                   int status )
{
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
/* BFO FAILOVER CODE - begin */
        mca_pml_bfo_repost_match_fragment(des); 
        return;
/* BFO FAILOVER CODE - end */
    }
    mca_pml_bfo_match_completion_free_request( btl, sendreq );
}

static inline void
mca_pml_bfo_rndv_completion_request( struct mca_btl_base_module_t* btl,
                                     mca_pml_bfo_send_request_t* sendreq,
                                     size_t req_bytes_delivered )
{
    if( sendreq->req_send.req_bytes_packed > 0 ) {
        PERUSE_TRACE_COMM_EVENT( PERUSE_COMM_REQ_XFER_BEGIN,
                                 &(sendreq->req_send.req_base), PERUSE_SEND );
    }

    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, req_bytes_delivered);

    /* advance the request */
    OPAL_THREAD_ADD32(&sendreq->req_state, -1);

    send_request_pml_complete_check(sendreq);

    /* check for pending requests */
    MCA_PML_BFO_PROGRESS_PENDING(btl);
}

/*
 *  Completion of the first fragment of a long message that 
 *  requires an acknowledgement
 */
static void
mca_pml_bfo_rndv_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* des,
                             int status )
{
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;
    size_t req_bytes_delivered = 0;

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
/* BFO FAILOVER CODE - begin */
        /* The completion event for the RNDV message has returned with 
         * an error. We know that the send request we are looking at is 
         * valid because it cannot be completed until the sendreq->req_state 
         * value reaches 0.  And for the sendreq->req_state to reach 0, 
         * the completion event on the RNDV message must occur.  So, we 
         * do not bother checking whether the send request is valid, 
         * because we know it is, but we put a few asserts in for good 
         * measure.  We then check a few fields in the request to decide what 
         * to do.  If the sendreq->req_error is set, that means that something 
         * has happend already to the request and we do not want to restart 
         * it.  Presumably, we may have received a RECVERRNOTIFY 
         * message from the receiver.  We also check the sendreq->req_acked 
         * field to see if it has been acked.  If it has, then again we 
         * do not restart everything because obviously the RNDV message 
         * has made it to the other side. */
        assert(((mca_pml_bfo_hdr_t*)(des->des_src->seg_addr.pval))->hdr_match.hdr_ctx ==
               sendreq->req_send.req_base.req_comm->c_contextid);
        assert(((mca_pml_bfo_hdr_t*)(des->des_src->seg_addr.pval))->hdr_match.hdr_src ==
               sendreq->req_send.req_base.req_comm->c_my_rank);
        assert(((mca_pml_bfo_hdr_t*)(des->des_src->seg_addr.pval))->hdr_match.hdr_seq ==
               (uint16_t)sendreq->req_send.req_base.req_sequence);

        if ((!sendreq->req_error) && (!sendreq->req_acked)) { 
            sendreq->req_events--; 
            /* Assume RNDV did not make it, so restart from the beginning. */ 
            mca_pml_bfo_send_request_restart(sendreq, true, MCA_PML_BFO_HDR_TYPE_RNDV); 
            return; 
        }
/* BFO FAILOVER CODE - end */
    }
/* BFO FAILOVER CODE - begin */
    sendreq->req_events--;

    /* Now check the error state.  This request can be in error if the
     * RNDV message made it over, but the receiver got an error trying
     * to send the ACK back and therefore sent a RECVERRNOTIFY message.
     * In that case, we want to start the restart dance as the receiver
     * has matched this message already.  Only restart if there are no
     * outstanding events on send request. */
    if (sendreq->req_error) {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDV: completion: sendreq has error, outstanding events=%d, "
                            "PML=%d, RQS=%d, src_req=%lx, dst_req=%lx, status=%d, peer=%d",
                            sendreq->req_events, (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq, (unsigned long)sendreq,
                            (unsigned long)sendreq->req_recv.pval,
                            status, sendreq->req_send.req_base.req_peer);
        if (0 == sendreq->req_events) {
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                       MCA_PML_BFO_HDR_TYPE_RNDV,
                                                       status, btl);
        }
        return;
    }
/* BFO FAILOVER CODE - end */

    /* count bytes of user data actually delivered. As the rndv completion only
     * happens in one thread, the increase of the req_bytes_delivered does not
     * have to be atomic.
     */
    MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( des->des_src,
                                        des->des_src_cnt,
                                        sizeof(mca_pml_bfo_rendezvous_hdr_t),
                                        req_bytes_delivered );

    mca_pml_bfo_rndv_completion_request( btl, sendreq, req_bytes_delivered );
}


/**
 * Completion of a get request.
 */

static void
mca_pml_bfo_rget_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* des,
                             int status )
{
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;
    size_t req_bytes_delivered = 0;
/* BFO FAILOVER CODE - begin */
    /* This can happen if a FIN message arrives after the request was
     * marked in error.  So, just drop the message.  Note that the
     * status field is not checked here.  That is because that is the
     * value returned in the FIN hdr.hdr_fail field and may be used for
     * other things. */
    if( OPAL_UNLIKELY(sendreq->req_error)) {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "FIN: received on broken request, skipping, "
                            "PML=%d, src_req=%lx, dst_req=%lx, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence,
                            (unsigned long)sendreq, (unsigned long)sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
        btl->btl_free(btl, des);
        return;
    }
/* BFO FAILOVER CODE - end */

    /* count bytes of user data actually delivered and check for request completion */
    MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( des->des_src, des->des_src_cnt,
                                        0, req_bytes_delivered );
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, req_bytes_delivered);

    send_request_pml_complete_check(sendreq);
    /* free the descriptor */
    btl->btl_free(btl, des);
    MCA_PML_BFO_PROGRESS_PENDING(btl);
}


/**
 * Completion of a control message - return resources.
 */

static void
mca_pml_bfo_send_ctl_completion( mca_btl_base_module_t* btl,
                                 struct mca_btl_base_endpoint_t* ep,
                                 struct mca_btl_base_descriptor_t* des,
                                 int status )
{
/* BFO FAILOVER CODE - begin */
    if(OPAL_LIKELY(OMPI_SUCCESS == status)) {
        /* check for pending requests */
        MCA_PML_BFO_PROGRESS_PENDING(btl);
    } else {
        mca_pml_bfo_hdr_t* hdr = des->des_src->seg_addr.pval;
        /* If we get an error on the RGET message, then first make
         * sure that header matches the send request that we are
         * pointing to.  This is necessary, because even though the
         * sending side got an error, the RGET may have made it to the
         * receiving side and the message transfer may have completed.
         * This would then mean the send request has been completed and
         * perhaps in use by another communication.  So there is no need
         * to restart this request.  Therefore, ensure that we are
         * looking at the same request that the header thinks we are
         * looking at.  If not, then there is nothing else to be done. */
        mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;

        switch (hdr->hdr_common.hdr_type) {
        case MCA_PML_BFO_HDR_TYPE_RGET:
            if ((hdr->hdr_match.hdr_ctx != sendreq->req_send.req_base.req_comm->c_contextid) ||
                (hdr->hdr_match.hdr_src != sendreq->req_send.req_base.req_comm->c_my_rank) ||
                (hdr->hdr_match.hdr_seq != (uint16_t)sendreq->req_send.req_base.req_sequence)) {
                opal_output_verbose(20, mca_pml_bfo_output,
                                    "RGET: completion event: dropping because no valid request "
                                    "PML:exp=%d,act=%d CTX:exp=%d,act=%d SRC:exp=%d,act=%d "
                                    "RQS:exp=%d,act=%d, dst_req=%p",
                                    (uint16_t)sendreq->req_send.req_base.req_sequence,
                                    hdr->hdr_match.hdr_seq,
                                    sendreq->req_send.req_base.req_comm->c_contextid,
                                    hdr->hdr_match.hdr_ctx,
                                    sendreq->req_send.req_base.req_comm->c_my_rank,
                                    hdr->hdr_match.hdr_src,
                                    sendreq->req_restartseq, hdr->hdr_fin.hdr_restartseq,
                                    (void *)sendreq);
                return;
            }
            mca_pml_bfo_send_request_restart(sendreq, true, MCA_PML_BFO_HDR_TYPE_RGET);
            return;
        default:
            opal_output(0, "%s:%d FATAL ERROR, unknown header (hdr=%d)",
                        __FILE__, __LINE__, hdr->hdr_common.hdr_type);
            orte_errmgr.abort(-1, NULL);
        }
    }
/* BFO FAILOVER CODE - end */
}

/**
 * Completion of additional fragments of a large message - may need
 * to schedule additional fragments.
 */

static void
mca_pml_bfo_frag_completion( mca_btl_base_module_t* btl,
                             struct mca_btl_base_endpoint_t* ep,
                             struct mca_btl_base_descriptor_t* des,
                             int status )
{
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)des->des_cbdata;
    size_t req_bytes_delivered = 0;
/* BFO FAILOVER CODE - begin */
    sendreq->req_events--;
/* BFO FAILOVER CODE - end */

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
/* BFO FAILOVER CODE - begin */
        sendreq->req_error++;
/* BFO FAILOVER CODE - end */
    }

    /* count bytes of user data actually delivered */
    MCA_PML_BFO_COMPUTE_SEGMENT_LENGTH( des->des_src,
                                        des->des_src_cnt,
                                        sizeof(mca_pml_bfo_frag_hdr_t),
                                        req_bytes_delivered );

    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth, -1);
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, req_bytes_delivered);

/* BFO FAILOVER CODE - begin */
    /* note we check error after bytes delivered computation in case frag made it */ 
    if( OPAL_UNLIKELY(sendreq->req_error)) { 
        opal_output_verbose(30, mca_pml_bfo_output, 
                            "FRAG: completion: sendreq has error, outstanding events=%d, " 
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d", 
                            sendreq->req_events, (uint16_t)sendreq->req_send.req_base.req_sequence, 
                            sendreq->req_restartseq, (void *)sendreq,
                            sendreq->req_recv.pval, 
                            status, sendreq->req_send.req_base.req_peer); 
        if (0 == sendreq->req_events) { 
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                       MCA_PML_BFO_HDR_TYPE_FRAG,
                                                       status, btl);
        } 
        return; 
    }
/* BFO FAILOVER CODE - end */
    if(send_request_pml_complete_check(sendreq) == false) {
        mca_pml_bfo_send_request_schedule(sendreq);
/* BFO FAILOVER CODE - begin */
        if( OPAL_UNLIKELY(sendreq->req_error)) {
            /* This situation can happen if the scheduling function
             * determined that a BTL was removed from underneath us
             * and therefore marked the request in error.  In that
             * case, the scheduling of fragments can no longer proceed
             * properly.  Therefore, if no outstanding events, initiate
             * the restart dance. */
            opal_output_verbose(30, mca_pml_bfo_output,
                                "FRAG: completion: BTL has been removed, outstanding events=%d, "
                                "PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d",
                                sendreq->req_events, (uint16_t)sendreq->req_send.req_base.req_sequence,
                                sendreq->req_restartseq, (void *)sendreq,
                                sendreq->req_recv.pval,
                                status, sendreq->req_send.req_base.req_peer);
            if (0 == sendreq->req_events) {
                mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                           MCA_PML_BFO_HDR_TYPE_FRAG,
                                                           status, btl);
            }
        }
/* BFO FAILOVER CODE - end */
    }

    /* check for pending requests */
    MCA_PML_BFO_PROGRESS_PENDING(btl);
}

/**
 *  Buffer the entire message and mark as complete.
 */

int mca_pml_bfo_send_request_start_buffered(
    mca_pml_bfo_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_bfo_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data, req_bytes_delivered;
    int rc;

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &des, 
                       MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_bfo_rendezvous_hdr_t) + size,
                       MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = des->des_src;

    /* pack the data into the BTL supplied buffer */
    iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval + 
                                    sizeof(mca_pml_bfo_rendezvous_hdr_t));
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if((rc = opal_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                  &iov,
                                  &iov_count,
                                  &max_data)) < 0) {
        mca_bml_base_free(bml_btl, des);
        return rc;
    }
    req_bytes_delivered = max_data;

    /* build rendezvous header */
    hdr = (mca_pml_bfo_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;
/* BFO FAILOVER CODE - begin */
    if (0 < sendreq->req_restartseq) {
        hdr->hdr_common.hdr_flags |= MCA_PML_BFO_HDR_FLAGS_RESTART;
        hdr->hdr_rndv.hdr_dst_req = sendreq->req_recv;
        hdr->hdr_rndv.hdr_restartseq = sendreq->req_restartseq;
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDV(buffered): restarting: PML=%d, RQS=%d, CTX=%d, SRC=%d, "
                            "src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence, sendreq->req_restartseq,
                            sendreq->req_send.req_base.req_comm->c_contextid,
                            sendreq->req_send.req_base.req_comm->c_my_rank, (void *)sendreq,
                            sendreq->req_recv.pval, sendreq->req_send.req_base.req_peer);
    }
/* BFO FAILOVER CODE - end */

    bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_RNDV,
                 sendreq->req_send.req_base.req_proc);

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_bfo_rendezvous_hdr_t) + max_data;

    des->des_cbfunc = mca_pml_bfo_rndv_completion;
    des->des_cbdata = sendreq;

    /* buffer the remainder of the message */
    rc = mca_pml_base_bsend_request_alloc((ompi_request_t*)sendreq);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
        mca_bml_base_free(bml_btl, des);
        return rc;
    }

    iov.iov_base = (IOVBASE_TYPE*)(((unsigned char*)sendreq->req_send.req_addr) + max_data);
    iov.iov_len = max_data = sendreq->req_send.req_bytes_packed - max_data;

    if((rc = opal_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                  &iov,
                                  &iov_count,
                                  &max_data)) < 0) {
        mca_bml_base_free(bml_btl, des);
        return rc;
    }

    /* re-init convertor for packed data */
    opal_convertor_prepare_for_send( &sendreq->req_send.req_base.req_convertor,
                                     &(ompi_mpi_byte.dt.super),
                                     sendreq->req_send.req_bytes_packed,
                                     sendreq->req_send.req_addr );
   
    /* wait for ack and completion */
    sendreq->req_state = 2;

    /* request is complete at mpi level */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_BFO_SEND_REQUEST_MPI_COMPLETE(sendreq, true);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_RNDV);
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_bfo_rndv_completion_request( bml_btl->btl, sendreq, req_bytes_delivered);
        }
/* BFO FAILOVER CODE - begin */
        if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
            sendreq->req_events++;
        }
/* BFO FAILOVER CODE - end */
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des );
    return rc;
}


/**
 *  We work on a buffered request with a size smaller than the eager size 
 *  or the BTL is not able to send the data IN_PLACE. Request a segment
 *  that is used for initial hdr and any eager data. This is used only
 *  from the _START macro.
 */
int mca_pml_bfo_send_request_start_copy( mca_pml_bfo_send_request_t* sendreq,
                                         mca_bml_base_btl_t* bml_btl,
                                         size_t size )
{
    mca_btl_base_descriptor_t* des = NULL;
    mca_btl_base_segment_t* segment;
    mca_pml_bfo_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data = size;
    int rc;

    if(NULL != bml_btl->btl->btl_sendi) {
        mca_pml_bfo_match_hdr_t match;
        match.hdr_common.hdr_flags = 0;
        match.hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_MATCH;
        match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
        match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        match.hdr_tag = sendreq->req_send.req_base.req_tag;
        match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
        
        bfo_hdr_hton(&match, MCA_PML_BFO_HDR_TYPE_MATCH,
                     sendreq->req_send.req_base.req_proc);

        /* try to send immediately */
        rc = mca_bml_base_sendi( bml_btl, &sendreq->req_send.req_base.req_convertor,
                                 &match, OMPI_PML_BFO_MATCH_HDR_LEN, 
                                 size, MCA_BTL_NO_ORDER, 
                                 MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                                 MCA_PML_BFO_HDR_TYPE_MATCH, 
                                 &des);
        if( OPAL_LIKELY(OMPI_SUCCESS == rc) ) {
/* BFO FAILOVER CODE - begin */
            /* Needed for failover */
            if (NULL != des) {
                des->des_cbfunc = mca_pml_bfo_match_completion_free;
                des->des_cbdata = sendreq->req_endpoint;
            }
/* BFO FAILOVER CODE - end */

            /* signal request completion */
            send_request_pml_complete(sendreq);

            /* check for pending requests */
            MCA_PML_BFO_PROGRESS_PENDING(bml_btl->btl);
            return OMPI_SUCCESS;
        }
    } else { 
        /* allocate descriptor */
        mca_bml_base_alloc( bml_btl, &des,
                            MCA_BTL_NO_ORDER,
                            OMPI_PML_BFO_MATCH_HDR_LEN + size,
                            MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
    }
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    segment = des->des_src;

    if(size > 0) {
        /* pack the data into the supplied buffer */
        iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval +
                                       OMPI_PML_BFO_MATCH_HDR_LEN);
        iov.iov_len  = size;
        iov_count    = 1;
        /*
         * Before copy the user buffer, make the target part 
         * accessible.
         */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        (void)opal_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                   &iov, &iov_count, &max_data );
         /*
          *  Packing finished, make the user buffer unaccessable.
          */
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
    }

    
    /* build match header */
    hdr = (mca_pml_bfo_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;

    bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_MATCH,
                 sendreq->req_send.req_base.req_proc);

    /* update lengths */
    segment->seg_len = OMPI_PML_BFO_MATCH_HDR_LEN + max_data;

    /* short message */
    des->des_cbdata = sendreq;
    des->des_cbfunc = mca_pml_bfo_match_completion_free;

    /* send */
    rc = mca_bml_base_send_status(bml_btl, des, MCA_PML_BFO_HDR_TYPE_MATCH);
    if( OPAL_LIKELY( rc >= OMPI_SUCCESS ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_bfo_match_completion_free_request( bml_btl->btl, sendreq );
        }
        return OMPI_SUCCESS;
    }
    switch(OPAL_SOS_GET_ERROR_CODE(rc)) {
        case OMPI_ERR_RESOURCE_BUSY:
            /* No more resources. Allow the upper level to queue the send */
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            break;
        default:
            mca_bml_base_free(bml_btl, des);
            break;
    }
    return rc;
}

/**
 *  BTL can send directly from user buffer so allow the BTL
 *  to prepare the segment list. Start sending a small message.
 */

int mca_pml_bfo_send_request_start_prepare( mca_pml_bfo_send_request_t* sendreq,
                                            mca_bml_base_btl_t* bml_btl,
                                            size_t size )
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_bfo_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    mca_bml_base_prepare_src( bml_btl,
                              NULL,
                              &sendreq->req_send.req_base.req_convertor,
                              MCA_BTL_NO_ORDER,
                              OMPI_PML_BFO_MATCH_HDR_LEN,
                              &size,
                              MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                              &des );
    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    segment = des->des_src;

    /* build match header */
    hdr = (mca_pml_bfo_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;

    bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_MATCH,
                 sendreq->req_send.req_base.req_proc);

    /* short message */
    des->des_cbfunc = mca_pml_bfo_match_completion_free;
    des->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_MATCH); 
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_bfo_match_completion_free_request( bml_btl->btl, sendreq );
        }
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des );
    return rc;
}


/**
 *  We have contigous data that is registered - schedule across
 *  available nics.
 */

int mca_pml_bfo_send_request_start_rdma( mca_pml_bfo_send_request_t* sendreq,
                                         mca_bml_base_btl_t* bml_btl,
                                         size_t size )
{
    /*
     * When req_rdma array is constructed the first element of the array always
     * assigned different btl in round robin fashion (if there are more than
     * one RDMA capable BTLs). This way round robin distribution of RDMA
     * operation is achieved.
     */

    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_bfo_hdr_t* hdr;
    bool need_local_cb = false;
    int rc;

    bml_btl = sendreq->req_rdma[0].bml_btl;
    if((sendreq->req_rdma_cnt == 1) && (bml_btl->btl_flags & MCA_BTL_FLAGS_GET)) {
        mca_mpool_base_registration_t* reg = sendreq->req_rdma[0].btl_reg;
        mca_btl_base_descriptor_t* src;
        size_t i;
        size_t old_position = sendreq->req_send.req_base.req_convertor.bConverted;

        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        /* prepare source descriptor/segment(s) */
        /* PML owns this descriptor and will free it in */
        /*  get_completion */
        mca_bml_base_prepare_src( bml_btl, 
                                  reg,
                                  &sendreq->req_send.req_base.req_convertor,
                                  MCA_BTL_NO_ORDER,
                                  0,
                                  &size,
                                  0,
                                  &src );
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        if( OPAL_UNLIKELY(NULL == src) ) {
            opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor,
                                        &old_position);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } 
        src->des_cbfunc = mca_pml_bfo_rget_completion;
        src->des_cbdata = sendreq;

        /* allocate space for get hdr + segment list */
        mca_bml_base_alloc(bml_btl, &des, MCA_BTL_NO_ORDER,
                           sizeof(mca_pml_bfo_rget_hdr_t) +
                           (sizeof(mca_btl_base_segment_t) * (src->des_src_cnt-1)),
                           MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if( OPAL_UNLIKELY(NULL == des) ) {
            opal_convertor_set_position( &sendreq->req_send.req_base.req_convertor,
                                         &old_position );
            mca_bml_base_free(bml_btl, src);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        segment = des->des_src;

        /* build match header */
        hdr = (mca_pml_bfo_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = MCA_PML_BFO_HDR_FLAGS_CONTIG|MCA_PML_BFO_HDR_FLAGS_PIN;
        hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RGET;
        hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
        hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_rndv.hdr_src_req.pval = sendreq;
/* BFO FAILOVER CODE - begin */
        if (0 < sendreq->req_restartseq) {
            hdr->hdr_common.hdr_flags |= MCA_PML_BFO_HDR_FLAGS_RESTART;
            hdr->hdr_rndv.hdr_dst_req = sendreq->req_recv;
            hdr->hdr_rndv.hdr_restartseq = sendreq->req_restartseq;
            opal_output_verbose(30, mca_pml_bfo_output,
                                "RGET: restarting: PML=%d, RQS=%d, CTX=%d, SRC=%d, "
                                "src_req=%p, dst_req=%p, peer=%d",
                                (uint16_t)sendreq->req_send.req_base.req_sequence,
                                sendreq->req_restartseq,
                                sendreq->req_send.req_base.req_comm->c_contextid,
                                sendreq->req_send.req_base.req_comm->c_my_rank,
                                (void *)sendreq, sendreq->req_recv.pval,
                                sendreq->req_send.req_base.req_peer);
        }
/* BFO FAILOVER CODE - end */
        hdr->hdr_rget.hdr_des.pval = src;
        hdr->hdr_rget.hdr_seg_cnt = src->des_src_cnt;

        bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_RGET,
                     sendreq->req_send.req_base.req_proc);

        for( i = 0; i < src->des_src_cnt; i++ ) {
            hdr->hdr_rget.hdr_segs[i].seg_addr.lval = ompi_ptr_ptol(src->des_src[i].seg_addr.pval);
            hdr->hdr_rget.hdr_segs[i].seg_len       = src->des_src[i].seg_len;
            hdr->hdr_rget.hdr_segs[i].seg_key.key64 = src->des_src[i].seg_key.key64;
        }

        des->des_cbfunc = mca_pml_bfo_send_ctl_completion;

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

        mca_bml_base_alloc(bml_btl, &des, 
                           MCA_BTL_NO_ORDER,
                           sizeof(mca_pml_bfo_rendezvous_hdr_t),
                           MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if( OPAL_UNLIKELY(NULL == des)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        segment = des->des_src;
            
        /* build hdr */
        hdr = (mca_pml_bfo_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = MCA_PML_BFO_HDR_FLAGS_CONTIG|MCA_PML_BFO_HDR_FLAGS_PIN;
        hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RNDV;
        hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
        hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_rndv.hdr_src_req.pval = sendreq;
/* BFO FAILOVER CODE - begin */
        if (0 < sendreq->req_restartseq) {
            hdr->hdr_common.hdr_flags |= MCA_PML_BFO_HDR_FLAGS_RESTART;
            hdr->hdr_rndv.hdr_dst_req = sendreq->req_recv;
            hdr->hdr_rndv.hdr_restartseq = sendreq->req_restartseq;
            opal_output_verbose(30, mca_pml_bfo_output,
                                "RNDV: restarting: PML=%d, RQS=%d, CTX=%d, SRC=%d, "
                                "src_req=%p, dst_req=%p, peer=%d",
                                (uint16_t)sendreq->req_send.req_base.req_sequence,
                                sendreq->req_restartseq,
                                sendreq->req_send.req_base.req_comm->c_contextid,
                                sendreq->req_send.req_base.req_comm->c_my_rank, 
                                (void *)sendreq, sendreq->req_recv.pval,
                                sendreq->req_send.req_base.req_peer);
        }
/* BFO FAILOVER CODE - end */

        bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_RNDV,
                     sendreq->req_send.req_base.req_proc);

        /* update lengths with number of bytes actually packed */
        segment->seg_len = sizeof(mca_pml_bfo_rendezvous_hdr_t);
    
        /* first fragment of a long message */
        des->des_cbfunc = mca_pml_bfo_rndv_completion;
        need_local_cb = true;

        /* wait for ack and completion */
        sendreq->req_state = 2;
    }

    des->des_cbdata = sendreq;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, hdr->hdr_common.hdr_type);
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) && (true == need_local_cb)) {
            mca_pml_bfo_rndv_completion_request( bml_btl->btl, sendreq, 0 );
        }
/* BFO FAILOVER CODE - begin */
        if ((des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) &&
            (MCA_PML_BFO_HDR_TYPE_RNDV == hdr->hdr_common.hdr_type)) {
            sendreq->req_events++;
        }
/* BFO FAILOVER CODE - end */
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des);
    return rc;
}


/**
 *  Rendezvous is required. Not doing rdma so eager send up to
 *  the btls eager limit.
 */

int mca_pml_bfo_send_request_start_rndv( mca_pml_bfo_send_request_t* sendreq,
                                         mca_bml_base_btl_t* bml_btl,
                                         size_t size,
                                         int flags )
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_bfo_hdr_t* hdr;
    int rc;

    /* prepare descriptor */
    if(size == 0) {
        mca_bml_base_alloc( bml_btl, 
                            &des, 
                            MCA_BTL_NO_ORDER,
                            sizeof(mca_pml_bfo_rendezvous_hdr_t),
                            MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP ); 
    } else {
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        mca_bml_base_prepare_src( bml_btl, 
                                  NULL,
                                  &sendreq->req_send.req_base.req_convertor,
                                  MCA_BTL_NO_ORDER,
                                  sizeof(mca_pml_bfo_rendezvous_hdr_t),
                                  &size,
                                  MCA_BTL_DES_FLAGS_PRIORITY | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                                  &des );
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
    }

    if( OPAL_UNLIKELY(NULL == des) ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    segment = des->des_src;

    /* build hdr */
    hdr = (mca_pml_bfo_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = (uint16_t)sendreq->req_send.req_base.req_sequence;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_rndv.hdr_src_req.pval = sendreq;
/* BFO FAILOVER CODE - begin */
    if (0 < sendreq->req_restartseq) {
        hdr->hdr_common.hdr_flags |= MCA_PML_BFO_HDR_FLAGS_RESTART;
        hdr->hdr_rndv.hdr_dst_req = sendreq->req_recv;
        hdr->hdr_rndv.hdr_restartseq = sendreq->req_restartseq;
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RNDV: restarting: PML=%d, RQS=%d, CTX=%d, SRC=%d, "
                            "src_req=%p, dst_req=%p, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence, sendreq->req_restartseq,
                            sendreq->req_send.req_base.req_comm->c_contextid,
                            sendreq->req_send.req_base.req_comm->c_my_rank,
                            (void *)sendreq, sendreq->req_recv.pval,
                            sendreq->req_send.req_base.req_peer);
    }
/* BFO FAILOVER CODE - end */

    bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_RNDV,
            sendreq->req_send.req_base.req_proc);

    /* first fragment of a long message */
    des->des_cbdata = sendreq;
    des->des_cbfunc = mca_pml_bfo_rndv_completion;

    /* wait for ack and completion */
    sendreq->req_state = 2;

    /* send */
    rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_RNDV);
    if( OPAL_LIKELY( rc >= 0 ) ) {
        if( OPAL_LIKELY( 1 == rc ) ) {
            mca_pml_bfo_rndv_completion_request( bml_btl->btl, sendreq, size );
        }
/* BFO FAILOVER CODE - begin */
        if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
            sendreq->req_events++;
        }
/* BFO FAILOVER CODE - end */
        return OMPI_SUCCESS;
    }
    mca_bml_base_free(bml_btl, des );
    return rc;
}

void mca_pml_bfo_send_request_copy_in_out( mca_pml_bfo_send_request_t *sendreq,
                                           uint64_t send_offset,
                                           uint64_t send_length )
{
    mca_pml_bfo_send_range_t *sr;
    ompi_free_list_item_t *i;
    mca_bml_base_endpoint_t* bml_endpoint = sendreq->req_endpoint;
    int num_btls = mca_bml_base_btl_array_get_size(&bml_endpoint->btl_send);
    int rc, n;
    double weight_total = 0;

    if( OPAL_UNLIKELY(0 == send_length) )
        return;

    OMPI_FREE_LIST_WAIT(&mca_pml_bfo.send_ranges, i, rc);

    sr = (mca_pml_bfo_send_range_t*)i;

    sr->range_send_offset = send_offset;
    sr->range_send_length = send_length;
    sr->range_btl_idx = 0;

    for(n = 0; n < num_btls && n < mca_pml_bfo.max_send_per_range; n++) {
        sr->range_btls[n].bml_btl =
            mca_bml_base_btl_array_get_next(&bml_endpoint->btl_send);
        weight_total += sr->range_btls[n].bml_btl->btl_weight;
    }

    sr->range_btl_cnt = n;
    mca_pml_bfo_calc_weighted_length(sr->range_btls, n, send_length,
            weight_total);

    OPAL_THREAD_LOCK(&sendreq->req_send_range_lock);
    opal_list_append(&sendreq->req_send_ranges, (opal_list_item_t*)sr);
    OPAL_THREAD_UNLOCK(&sendreq->req_send_range_lock);
}

static inline mca_pml_bfo_send_range_t *
get_send_range_nolock(mca_pml_bfo_send_request_t* sendreq)
{
    opal_list_item_t *item;

    item = opal_list_get_first(&sendreq->req_send_ranges);

    if(opal_list_get_end(&sendreq->req_send_ranges) == item)
        return NULL;

    return (mca_pml_bfo_send_range_t*)item;
}

static inline mca_pml_bfo_send_range_t *
get_send_range(mca_pml_bfo_send_request_t* sendreq)
{
    mca_pml_bfo_send_range_t *range;

    OPAL_THREAD_LOCK(&sendreq->req_send_range_lock);
    range = get_send_range_nolock(sendreq);
    OPAL_THREAD_UNLOCK(&sendreq->req_send_range_lock);

    return range;
}

static inline mca_pml_bfo_send_range_t *
get_next_send_range(mca_pml_bfo_send_request_t* sendreq,
        mca_pml_bfo_send_range_t *range)
{
    OPAL_THREAD_LOCK(&sendreq->req_send_range_lock);
    opal_list_remove_item(&sendreq->req_send_ranges, (opal_list_item_t *)range);
    OMPI_FREE_LIST_RETURN(&mca_pml_bfo.send_ranges, &range->base);
    range = get_send_range_nolock(sendreq);
    OPAL_THREAD_UNLOCK(&sendreq->req_send_range_lock);

    return range;
}

/**
 *  Schedule pipeline of send descriptors for the given request.
 *  Up to the rdma threshold. If this is a send based protocol,
 *  the rdma threshold is the end of the message. Otherwise, schedule
 *  fragments up to the threshold to overlap initial registration/setup
 *  costs of the rdma. Only one thread can be inside this function.
 */

int
mca_pml_bfo_send_request_schedule_once(mca_pml_bfo_send_request_t* sendreq)
{ 
    size_t prev_bytes_remaining = 0;
    mca_pml_bfo_send_range_t *range;
    int num_fail = 0;

    /* check pipeline_depth here before attempting to get any locks */
    if(true == sendreq->req_throttle_sends &&
            sendreq->req_pipeline_depth >= mca_pml_bfo.send_pipeline_depth)
        return OMPI_SUCCESS;

    range = get_send_range(sendreq);

    while(range && (false == sendreq->req_throttle_sends ||
            sendreq->req_pipeline_depth < mca_pml_bfo.send_pipeline_depth)) {
        mca_pml_bfo_frag_hdr_t* hdr;
        mca_btl_base_descriptor_t* des;
        int rc, btl_idx;
        size_t size, offset, data_remaining = 0;
        mca_bml_base_btl_t* bml_btl;

        assert(range->range_send_length != 0);
/* BFO FAILOVER CODE - begin */
        /* Failover code.  If this is true, this means the request thinks we
         * have more BTLs than there really are.  This can happen because
         * a BTL was removed from the available list.  In this case, we
         * want to start over. */
        if ((int)mca_bml_base_btl_array_get_size(&sendreq->req_endpoint->btl_send)
            != range->range_btl_cnt) {
            sendreq->req_error++;
            return OMPI_ERROR;
        }
/* BFO FAILOVER CODE - end */

        if(prev_bytes_remaining == range->range_send_length)
            num_fail++;
        else
            num_fail = 0;

        prev_bytes_remaining = range->range_send_length;

        if( OPAL_UNLIKELY(num_fail == range->range_btl_cnt) ) {
            assert(sendreq->req_pending == MCA_PML_BFO_SEND_PENDING_NONE);
            add_request_to_send_pending(sendreq,
                    MCA_PML_BFO_SEND_PENDING_SCHEDULE, true);
            /* Note that request remains locked. send_request_process_pending()
             * function will call shedule_exclusive() directly without taking
             * the lock */
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

cannot_pack:
        do {
            btl_idx = range->range_btl_idx;
            if(++range->range_btl_idx == range->range_btl_cnt)
                range->range_btl_idx = 0;
        } while(!range->range_btls[btl_idx].length);

        bml_btl = range->range_btls[btl_idx].bml_btl;
        /* If there is a remaining data from another BTL that was too small
         * for converter to pack then send it through another BTL */
        range->range_btls[btl_idx].length += data_remaining;
        size = range->range_btls[btl_idx].length;

        /* makes sure that we don't exceed BTL max send size */
        if(bml_btl->btl->btl_max_send_size != 0) {
            size_t max_send_size = bml_btl->btl->btl_max_send_size -
                sizeof(mca_pml_bfo_frag_hdr_t);

            if (size > max_send_size) {
                size = max_send_size;
            }
        }
            
        /* pack into a descriptor */
        offset = (size_t)range->range_send_offset;
        opal_convertor_set_position(&sendreq->req_send.req_base.req_convertor, 
                                    &offset);
        range->range_send_offset = (uint64_t)offset;

        data_remaining = size;
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_defined,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );
        mca_bml_base_prepare_src(bml_btl, NULL,
                                 &sendreq->req_send.req_base.req_convertor,
                                 MCA_BTL_NO_ORDER,
                                 sizeof(mca_pml_bfo_frag_hdr_t),
                                 &size, MCA_BTL_DES_FLAGS_BTL_OWNERSHIP | MCA_BTL_DES_SEND_ALWAYS_CALLBACK, &des);
        MEMCHECKER(
            memchecker_call(&opal_memchecker_base_mem_noaccess,
                            sendreq->req_send.req_base.req_addr,
                            sendreq->req_send.req_base.req_count,
                            sendreq->req_send.req_base.req_datatype);
        );

        if( OPAL_UNLIKELY(des == NULL || size == 0) ) {
            if(des) {
                /* Converter can't pack this chunk. Append to another chunk
                 * from other BTL */
                mca_bml_base_free(bml_btl, des);
                range->range_btls[btl_idx].length -= data_remaining;
                goto cannot_pack;
            }   
            continue;
        }

        des->des_cbfunc = mca_pml_bfo_frag_completion;
        des->des_cbdata = sendreq;

        /* setup header */
        hdr = (mca_pml_bfo_frag_hdr_t*)des->des_src->seg_addr.pval;
        hdr->hdr_common.hdr_flags = 0;
        hdr->hdr_common.hdr_type = MCA_PML_BFO_HDR_TYPE_FRAG;
        hdr->hdr_frag_offset = range->range_send_offset;
        hdr->hdr_src_req.pval = sendreq;
        hdr->hdr_dst_req = sendreq->req_recv;

        bfo_hdr_hton(hdr, MCA_PML_BFO_HDR_TYPE_FRAG,
                sendreq->req_send.req_base.req_proc);

#if OMPI_WANT_PERUSE
         PERUSE_TRACE_COMM_OMPI_EVENT(PERUSE_COMM_REQ_XFER_CONTINUE,
                 &(sendreq->req_send.req_base), size, PERUSE_SEND);
#endif  /* OMPI_WANT_PERUSE */

        /* initiate send - note that this may complete before the call returns */
        rc = mca_bml_base_send(bml_btl, des, MCA_PML_BFO_HDR_TYPE_FRAG);
        if( OPAL_LIKELY(rc >= 0) ) {
            /* update state */
            range->range_btls[btl_idx].length -= size;
            range->range_send_length -= size;
            range->range_send_offset += size;
            OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth, 1);
            if(range->range_send_length == 0) {
                range = get_next_send_range(sendreq, range);
                prev_bytes_remaining = 0;
            }
/* BFO FAILOVER CODE - begin */
            if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
                sendreq->req_events++;
            }
/* BFO FAILOVER CODE - end */
        } else { 
            mca_bml_base_free(bml_btl,des);
        }
    }

    return OMPI_SUCCESS;
} 


/**
 *  An RDMA put operation has completed:
 *  (1) Update request status and if required set completed
 *  (2) Send FIN control message to the destination 
 */

static void mca_pml_bfo_put_completion( mca_btl_base_module_t* btl,
                                        struct mca_btl_base_endpoint_t* ep,
                                        struct mca_btl_base_descriptor_t* des,
                                        int status )
{
    mca_pml_bfo_rdma_frag_t* frag = (mca_pml_bfo_rdma_frag_t*)des->des_cbdata;
    mca_pml_bfo_send_request_t* sendreq = (mca_pml_bfo_send_request_t*)frag->rdma_req;
    mca_bml_base_btl_t* bml_btl;
/* BFO FAILOVER CODE - begin */
    sendreq->req_events--;
/* BFO FAILOVER CODE - end */

    /* check completion status */
    if( OPAL_UNLIKELY(OMPI_SUCCESS != status) ) {
/* BFO FAILOVER CODE - begin */
        sendreq->req_error++;
/* BFO FAILOVER CODE - end */
    }

/* BFO FAILOVER CODE - begin */
    if ( OPAL_UNLIKELY(sendreq->req_error)) {
        opal_output_verbose(30, mca_pml_bfo_output,
                            "RDMA write: completion: sendreq has error, outstanding events=%d, "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d",
                            sendreq->req_events, (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq, (void *)sendreq,
                            sendreq->req_recv.pval,
                            status, sendreq->req_send.req_base.req_peer);
        if (0 == sendreq->req_events) {
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                       MCA_PML_BFO_HDR_TYPE_PUT,
                                                       status, btl);
        }
        MCA_PML_BFO_RDMA_FRAG_RETURN(frag);
        return;
    }
/* BFO FAILOVER CODE - end */

/* BFO FAILOVER CODE - begin */
    /* Find back the bml_btl that this btl belongs to.  If we cannot
     * find it, then it may have been removed from underneath us, so
     * find the next available one to send the FIN message on. */
    bml_btl = mca_bml_base_btl_array_find(&sendreq->req_endpoint->btl_rdma, btl);
    if( OPAL_UNLIKELY(NULL == bml_btl) ) {
        opal_output_verbose(20, mca_pml_bfo_output,
                            "RDMA write completion: BML was removed from underneath us, "
                            "PML=%d, RQS=%d, src_req=%p, dst_req=%p, status=%d, peer=%d",
                            (uint16_t)sendreq->req_send.req_base.req_sequence,
                            sendreq->req_restartseq, (void *)sendreq,
                            sendreq->req_recv.pval,
                            status, sendreq->req_send.req_base.req_peer);
        bml_btl = mca_bml_base_btl_array_get_next(&sendreq->req_endpoint->btl_rdma);
    }
/* BFO FAILOVER CODE - end */

    mca_pml_bfo_send_fin(sendreq->req_send.req_base.req_proc, 
                         bml_btl,
                         frag->rdma_hdr.hdr_rdma.hdr_des,
                         des->order, 0, (uint16_t)sendreq->req_send.req_base.req_sequence, 
                         sendreq->req_restartseq, sendreq->req_send.req_base.req_comm->c_contextid, 
                         sendreq->req_send.req_base.req_comm->c_my_rank); 
    
    /* check for request completion */
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_bytes_delivered, frag->rdma_length);

    send_request_pml_complete_check(sendreq);

    MCA_PML_BFO_RDMA_FRAG_RETURN(frag);

    MCA_PML_BFO_PROGRESS_PENDING(btl);
}

int mca_pml_bfo_send_request_put_frag( mca_pml_bfo_rdma_frag_t* frag )
{
    mca_mpool_base_registration_t* reg = NULL;
    mca_bml_base_btl_t* bml_btl = frag->rdma_bml;
    mca_btl_base_descriptor_t* des;
    size_t save_size = frag->rdma_length;
    int rc;

    /* setup descriptor */
    mca_bml_base_prepare_src( bml_btl, 
                              reg,
                              &frag->convertor, 
                              MCA_BTL_NO_ORDER,
                              0,
                              &frag->rdma_length,
                              MCA_BTL_DES_FLAGS_BTL_OWNERSHIP,
                              &des );
    
    if( OPAL_UNLIKELY(NULL == des) ) {
        if(frag->retries < mca_pml_bfo.rdma_put_retries_limit) {
            size_t offset = (size_t)frag->rdma_hdr.hdr_rdma.hdr_rdma_offset;
            frag->rdma_length = save_size; 
            opal_convertor_set_position(&frag->convertor, &offset);
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
        } else {
            mca_pml_bfo_send_request_t *sendreq =
                (mca_pml_bfo_send_request_t*)frag->rdma_req;

            /* tell receiver to unregister memory */
            mca_pml_bfo_send_fin(sendreq->req_send.req_base.req_proc,
                    bml_btl, frag->rdma_hdr.hdr_rdma.hdr_des,
                    MCA_BTL_NO_ORDER, 1, (uint16_t)sendreq->req_send.req_base.req_sequence, 
                    sendreq->req_restartseq, sendreq->req_send.req_base.req_comm->c_contextid, 
                    sendreq->req_send.req_base.req_comm->c_my_rank); 

            /* send fragment by copy in/out */
            mca_pml_bfo_send_request_copy_in_out(sendreq,
                    frag->rdma_hdr.hdr_rdma.hdr_rdma_offset, frag->rdma_length);
            /* if a pointer to a receive request is not set it means that
             * ACK was not yet received. Don't schedule sends before ACK */
            if(NULL != sendreq->req_recv.pval)
                mca_pml_bfo_send_request_schedule(sendreq);
        }
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    des->des_dst = frag->rdma_segs;
    des->des_dst_cnt = frag->rdma_hdr.hdr_rdma.hdr_seg_cnt;
    des->des_cbfunc = mca_pml_bfo_put_completion;
    des->des_cbdata = frag;

    PERUSE_TRACE_COMM_OMPI_EVENT( PERUSE_COMM_REQ_XFER_CONTINUE,
                                  &(((mca_pml_bfo_send_request_t*)frag->rdma_req)->req_send.req_base), save_size, PERUSE_SEND );

    rc = mca_bml_base_put(bml_btl, des);
    if( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) {
        mca_bml_base_free(bml_btl, des);
        frag->rdma_length = save_size;
        if(OMPI_ERR_OUT_OF_RESOURCE == OPAL_SOS_GET_ERROR_CODE(rc)) {
            OPAL_THREAD_LOCK(&mca_pml_bfo.lock);
            opal_list_append(&mca_pml_bfo.rdma_pending, (opal_list_item_t*)frag);
            OPAL_THREAD_UNLOCK(&mca_pml_bfo.lock);
            return OMPI_ERR_OUT_OF_RESOURCE;
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort(-1, NULL);
        }
    }
/* BFO FAILOVER CODE - begin */
    if (des->des_flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) {
        mca_pml_bfo_send_request_t *sendreq =
            (mca_pml_bfo_send_request_t*)frag->rdma_req;
        sendreq->req_events++;
    }
/* BFO FAILOVER CODE - end */

    return OMPI_SUCCESS;
}

/**
 *  Receiver has scheduled an RDMA operation:
 *  (1) Allocate an RDMA fragment to maintain the state of the operation
 *  (2) Call BTL prepare_src to pin/prepare source buffers
 *  (3) Queue the RDMA put 
 */

void mca_pml_bfo_send_request_put( mca_pml_bfo_send_request_t* sendreq,
                                   mca_btl_base_module_t* btl, 
                                   mca_pml_bfo_rdma_hdr_t* hdr )
{
    mca_bml_base_endpoint_t *bml_endpoint = sendreq->req_endpoint;
    mca_pml_bfo_rdma_frag_t* frag;
    int rc;
    size_t i, size = 0;

    if(hdr->hdr_common.hdr_flags & MCA_PML_BFO_HDR_TYPE_ACK) { 
/* BFO FAILOVER CODE - begin */
        /* Handle the failover case where a RNDV request may
         * have turned into a RGET and therefore the state
         * is not being tracked. */
        if (sendreq->req_state != 0) {
            OPAL_THREAD_ADD32(&sendreq->req_state, -1);
        }
/* BFO FAILOVER CODE - end */
    }
/* BFO FAILOVER CODE - begin */
    sendreq->req_recv = hdr->hdr_dst_req; /* only needed once, but it is OK */
    sendreq->req_acked = true;            /* only needed once, but it is OK */
/* BFO FAILOVER CODE - end */

    MCA_PML_BFO_RDMA_FRAG_ALLOC(frag, rc); 

    if( OPAL_UNLIKELY(NULL == frag) ) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(rc);
        orte_errmgr.abort(-1, NULL);
    }

    /* setup fragment */
    for( i = 0; i < hdr->hdr_seg_cnt; i++ ) {
        frag->rdma_segs[i].seg_addr.lval = hdr->hdr_segs[i].seg_addr.lval;
        frag->rdma_segs[i].seg_len       = hdr->hdr_segs[i].seg_len;
        frag->rdma_segs[i].seg_key.key64 = hdr->hdr_segs[i].seg_key.key64;

#if OPAL_ENABLE_HETEROGENEOUS_SUPPORT
        if ((sendreq->req_send.req_base.req_proc->proc_arch & OPAL_ARCH_ISBIGENDIAN) !=
            (ompi_proc_local()->proc_arch & OPAL_ARCH_ISBIGENDIAN)) {
            size += opal_swap_bytes4(frag->rdma_segs[i].seg_len);
        } else 
#endif
        {
            size += frag->rdma_segs[i].seg_len;
        }
    }

    frag->rdma_bml = mca_bml_base_btl_array_find(&bml_endpoint->btl_rdma, btl);
/* BFO FAILOVER CODE - begin */
    frag->rdma_btl = btl;
    if( OPAL_UNLIKELY(NULL == frag->rdma_bml) ) {
        opal_output(0, "[%s:%d] invalid bml for rdma put", __FILE__, __LINE__);
        MCA_PML_BFO_RDMA_FRAG_RETURN(frag);
        sendreq->req_error++;
        if (0 == sendreq->req_events) {
            opal_output(0, "[%s:%d] Issuing rndvrestartnotify", __FILE__, __LINE__);
            mca_pml_bfo_send_request_rndvrestartnotify(sendreq, false,
                                                       MCA_PML_BFO_HDR_TYPE_PUT,
                                                       OMPI_ERROR, btl);
        }
        return;
    }
/* BFO FAILOVER CODE - end */
    frag->rdma_hdr.hdr_rdma = *hdr;
    frag->rdma_req = sendreq; 
    frag->rdma_ep = bml_endpoint;
    frag->rdma_length = size;
    frag->rdma_state = MCA_PML_BFO_RDMA_PUT;
    frag->reg = NULL;
    frag->retries = 0;

    /* lookup the corresponding registration */
    for(i=0; i<sendreq->req_rdma_cnt; i++) {
       if(sendreq->req_rdma[i].bml_btl == frag->rdma_bml) {
           frag->reg = sendreq->req_rdma[i].btl_reg;
           break;
       }
    } 

    /*  RDMA writes may proceed in parallel to send and to each other, so
     *  create clone of the convertor for each RDMA fragment
     */
    size = hdr->hdr_rdma_offset;
    opal_convertor_clone_with_position(&sendreq->req_send.req_base.req_convertor,
            &frag->convertor, 0, &size);

    mca_pml_bfo_send_request_put_frag(frag);
}

