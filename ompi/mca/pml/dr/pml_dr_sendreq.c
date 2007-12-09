/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Mellanox Technologies. 
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "opal/util/crc.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/class/ompi_seq_tracker.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_dr.h"
#include "pml_dr_hdr.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_recvreq.h"
#include "ompi/mca/bml/base/base.h"


/*
 * The free call mark the final stage in a request life-cycle. Starting from this
 * point the request is completed at both PML and user level, and can be used
 * for others p2p communications. Therefore, in the case of the DR PML it should
 * be added to the free request list.
 */
static int mca_pml_dr_send_request_free(struct ompi_request_t** request)
{
    mca_pml_dr_send_request_t* sendreq = *(mca_pml_dr_send_request_t**)request;

    assert( false == sendreq->req_send.req_base.req_free_called );
    OPAL_THREAD_LOCK(&ompi_request_lock);
    sendreq->req_send.req_base.req_free_called = true;
    if( true == sendreq->req_send.req_base.req_pml_complete ) {
        MCA_PML_DR_SEND_REQUEST_RETURN( sendreq );
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_dr_send_request_cancel(struct ompi_request_t* request, int complete)
{
    /* we dont cancel send requests by now */
    return OMPI_SUCCESS;
}

static void mca_pml_dr_send_request_construct(mca_pml_dr_send_request_t* req)
{
    OBJ_CONSTRUCT(&req->req_vfrag0, mca_pml_dr_vfrag_t);
    OBJ_CONSTRUCT(&req->req_retrans, opal_list_t);

    req->req_vfrag0.vf_len = 1;
    req->req_vfrag0.vf_mask = 1;
    req->req_vfrag0.vf_send.pval = req;
    req->req_send.req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_send.req_base.req_ompi.req_free = mca_pml_dr_send_request_free;
    req->req_send.req_base.req_ompi.req_cancel = mca_pml_dr_send_request_cancel;
}

static void mca_pml_dr_send_request_destruct(mca_pml_dr_send_request_t* req)
{
    OBJ_DESTRUCT(&req->req_vfrag0);
    OBJ_DESTRUCT(&req->req_retrans);
}


OBJ_CLASS_INSTANCE(
    mca_pml_dr_send_request_t,
    mca_pml_base_send_request_t,
    mca_pml_dr_send_request_construct,
    mca_pml_dr_send_request_destruct);

/**
 * Handle error status on local completion
 */

static void mca_pml_dr_error_completion(
    struct mca_btl_base_module_t* btl,  
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)descriptor->des_cbdata;
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;

    switch(status) {
        case OMPI_ERR_UNREACH:
        case OMPI_ERR_CONNECTION_FAILED:
        case OMPI_ERR_CONNECTION_REFUSED:
            /**
             * peer is no longer reachable through this btl
             */
            mca_bml.bml_del_proc_btl(sendreq->req_proc->ompi_proc, btl);
            break;

        case OMPI_ERR_FATAL:
        case OMPI_ERR_COMM_FAILURE:
            /**
             * btl is no longer available
             */
            mca_bml.bml_del_btl(btl);
            break;
        default:
            orte_errmgr.abort();
            break;
    }

    /* update pending counts */
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
    OPAL_THREAD_ADD64(&vfrag->vf_pending,-1);

    /* reset vfrag state - select new BTL */
    mca_pml_dr_vfrag_reset(vfrag);
   
    /* reschedule vfrag */
    mca_pml_dr_vfrag_reschedule(vfrag);
}

/**
 * Completion of a short message - nothing left to schedule.
 */

static void mca_pml_dr_match_completion(
    struct mca_btl_base_module_t* btl,  
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)descriptor->des_cbdata;
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;

    /* kill pending wdog timer */
    MCA_PML_DR_VFRAG_WDOG_STOP(vfrag);

    /* free any descriptor used to retransmit */
    if(descriptor != sendreq->req_descriptor) {
        mca_bml_base_free( (mca_bml_base_btl_t*)descriptor->des_context, descriptor);
    }
    
    /* check completion status */
    if(OMPI_SUCCESS != status) {
        mca_pml_dr_error_completion(btl,ep,descriptor,status);
        return;
    }

    /* wait for local completion */
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
    if(OPAL_THREAD_ADD64(&vfrag->vf_pending,-1) > 0) 
        return;

    /* wait for positive ack to complete request */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    if(vfrag->vf_ack == vfrag->vf_mask) {

        /* return descriptor */
        if(NULL != sendreq->req_descriptor) {
            if(NULL != sendreq->req_descriptor->des_context) { 
                mca_bml_base_free((mca_bml_base_btl_t*)sendreq->req_descriptor->des_context, sendreq->req_descriptor); 
            }
            sendreq->req_descriptor = NULL;
        }

        /* update statistics and complete */
        sendreq->req_bytes_delivered = sendreq->req_send.req_bytes_packed;
        MCA_PML_DR_DEBUG(1, (0, "%s:%d: inserting vid %d into sequence tracker\n", 
                     __FILE__, __LINE__, vfrag->vf_id));
        
        ompi_seq_tracker_insert(&sendreq->req_endpoint->seq_sends, vfrag->vf_id);
        MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);

    /* on negative ack need to retransmit */
    } else if(vfrag->vf_state & MCA_PML_DR_VFRAG_NACKED) {
        MCA_PML_DR_VFRAG_WDOG_START(vfrag);
        MCA_PML_DR_SEND_REQUEST_EAGER_RETRY(sendreq, vfrag);

    /* start ack timer */
    }  else {
        MCA_PML_DR_VFRAG_ACK_START(vfrag);
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* check for pending requests */
    MCA_PML_DR_SEND_REQUEST_PROCESS_PENDING();
}

/*
 *  Completion of the first fragment of a long message that 
 *  requires an acknowledgement
 */

static void mca_pml_dr_rndv_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)descriptor->des_cbdata;
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;
    bool schedule = false;

    /* kill pending wdog timer */
    MCA_PML_DR_VFRAG_WDOG_STOP(vfrag);

    /* free any descriptor used to retransmit */
    if(descriptor != sendreq->req_descriptor) {
        mca_bml_base_free( (mca_bml_base_btl_t*)descriptor->des_context, descriptor );
    }

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        mca_pml_dr_error_completion(btl,ep,descriptor,status); 
        return;
    }

    /* local completion */
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
    if(OPAL_THREAD_ADD64(&vfrag->vf_pending,-1) > 0)
        return;
    
    /* wait for positive ack to complete request */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    if(vfrag->vf_ack == vfrag->vf_mask) {
        if(sendreq->req_descriptor) {
            if(NULL != sendreq->req_descriptor->des_context) { 
                mca_bml_base_free((mca_bml_base_btl_t*)sendreq->req_descriptor->des_context, sendreq->req_descriptor);
            }
            sendreq->req_descriptor = NULL;
        }

        /* update statistics and complete */
        MCA_PML_DR_DEBUG(1, (0, "%s:%d: inserting vid %d into sequence tracker\n", 
                             __FILE__, __LINE__, vfrag->vf_id));
            
        ompi_seq_tracker_insert(&sendreq->req_endpoint->seq_sends, vfrag->vf_id);
        if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
            MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
        } else {
            schedule = true;
        }
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        if(schedule) {
            mca_pml_dr_send_request_schedule(sendreq);
        }

    /* on negative ack need to retransmit */
    } else if(vfrag->vf_state & MCA_PML_DR_VFRAG_NACKED) {
        MCA_PML_DR_VFRAG_ACK_RESET(vfrag);
        MCA_PML_DR_SEND_REQUEST_RNDV_PROBE(sendreq, vfrag);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* setup ack timer */
    } else {
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        MCA_PML_DR_VFRAG_ACK_START(vfrag);
    }

    /* check for pending requests */
    MCA_PML_DR_SEND_REQUEST_PROCESS_PENDING();
}


/**
 * Completion of additional fragments of a large message - may need
 * to schedule additional fragments.
 */

static void mca_pml_dr_frag_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)descriptor->des_cbdata;
    mca_pml_dr_send_request_t* sendreq  = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;
    mca_bml_base_btl_t* bml_btl = vfrag->bml_btl;
    bool schedule = false;
    
    /* check completion status */
    if(OMPI_SUCCESS != status) {
        mca_pml_dr_error_completion(btl,ep,descriptor,status);
        return;
    }

    /* have all pending frags completed for this vfrag? */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    vfrag->vf_pending -= 1;
    if(vfrag->vf_pending == 0) {
        MCA_PML_DR_VFRAG_WDOG_STOP(vfrag);

        /* has the vfrag already been acked */
        if (vfrag->vf_ack == vfrag->vf_mask) {

            sendreq->req_bytes_delivered += vfrag->vf_size;
            assert(sendreq->req_bytes_delivered <= sendreq->req_send.req_bytes_packed);

            /* is this vfrag in the process of being retransmitted */
            if(vfrag->vf_state & MCA_PML_DR_VFRAG_RETRANS) {
                opal_list_remove_item(&sendreq->req_retrans, (opal_list_item_t*)vfrag);
                vfrag->vf_state &= ~MCA_PML_DR_VFRAG_RETRANS;
            } 

            /* record vfrag id to drop duplicate acks */
            MCA_PML_DR_DEBUG(1, (0, "%s:%d: inserting vid %d into sequence tracker\n", 
                                 __FILE__, __LINE__, vfrag->vf_id));
            
            ompi_seq_tracker_insert(&sendreq->req_endpoint->seq_sends, vfrag->vf_id);
            
            /* return this vfrag */
            MCA_PML_DR_VFRAG_RETURN(vfrag);

        /* waiting on ack */
        } else if (vfrag->vf_idx == vfrag->vf_len) {
            MCA_PML_DR_VFRAG_ACK_START(vfrag);
        }

    /* if not reset the watchdog timer */
    } else { 
        MCA_PML_DR_VFRAG_WDOG_RESET(vfrag);
    }

    /* are we done with this request ? */
    if(OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth, -1) == 0 && 
       sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
        MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
    }  else if (sendreq->req_send_offset < sendreq->req_send.req_bytes_packed ||
                opal_list_get_size(&sendreq->req_retrans)) {
        schedule = true;
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    
    /* return the descriptor */
    mca_bml_base_free(bml_btl, descriptor);

    /* schedule remainder of message? */
    if(schedule) {
        mca_pml_dr_send_request_schedule(sendreq);
    }

    /* check for pending requests */
    MCA_PML_DR_SEND_REQUEST_PROCESS_PENDING();
}



/**
 *  Buffer the entire message and mark as complete.
 */

int mca_pml_dr_send_request_start_buffered(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_dr_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int rc;
    uint32_t csum; 
    bool do_csum = mca_pml_dr.enable_csum && 
        (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 
    
    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_dr_rendezvous_hdr_t) + size,
                       MCA_BTL_DES_FLAGS_PRIORITY);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    sendreq->req_descriptor = descriptor; /* hang on to this for later */
    segment = descriptor->des_src;
    
    /* pack the data into the BTL supplied buffer */
    iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval + 
             sizeof(mca_pml_dr_rendezvous_hdr_t));
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
    csum = sendreq->req_send.req_base.req_convertor.checksum;

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_dr_rendezvous_hdr_t) + max_data;
    sendreq->req_vfrag0.vf_size = max_data;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    sendreq->req_vfrag0.vf_state |= MCA_PML_DR_VFRAG_RNDV;
    sendreq->req_vfrag0.vf_pending = 1;
    
    descriptor->des_cbfunc = mca_pml_dr_rndv_completion;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = &sendreq->req_vfrag0;

    /* buffer the remainder of the message */
    rc = mca_pml_base_bsend_request_alloc((ompi_request_t*)sendreq);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor);
        return rc;
    }

    iov.iov_base = (IOVBASE_TYPE*)(((unsigned char*)sendreq->req_send.req_addr) + max_data);
    iov.iov_len = sendreq->req_send.req_bytes_packed - max_data;

    max_data = iov.iov_len;
    if((rc = ompi_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                  &iov,
                                  &iov_count,
                                  &max_data)) < 0) {
            mca_bml_base_free(bml_btl, descriptor);
            return rc;
    }

    /* build rendezvous header */
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_csum = 0;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_RNDV;
    hdr->hdr_common.hdr_dst = sendreq->req_endpoint->dst;
    hdr->hdr_common.hdr_src = sendreq->req_endpoint->src;
    hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_common.hdr_vid = sendreq->req_vfrag0.vf_id;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_csum = csum;
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_common.hdr_csum = (do_csum ? 
                                opal_csum(hdr, sizeof(mca_pml_dr_rendezvous_hdr_t)) :
                                OPAL_CSUM_ZERO);
    
    /* re-init convertor for packed data (it keep the flags) */
    ompi_convertor_prepare_for_send( &sendreq->req_send.req_base.req_convertor,
                                     MPI_BYTE,
                                     sendreq->req_send.req_bytes_packed,
                                     sendreq->req_send.req_addr );

    /* request is complete at mpi level */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* send */
    MCA_PML_DR_VFRAG_WDOG_START(&sendreq->req_vfrag0);
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    }
    return rc;
}


/**
 *  BTL requires "specially" allocated memory. Request a segment that
 *  is used for initial hdr and any eager data.
 */

int mca_pml_dr_send_request_start_copy(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_dr_hdr_t* hdr;
    struct iovec iov;
    unsigned int iov_count;
    size_t max_data;
    int rc;
    bool do_csum = mca_pml_dr.enable_csum && 
        (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 

    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, 
                       MCA_BTL_NO_ORDER,
                       sizeof(mca_pml_dr_match_hdr_t) + size,
                       MCA_BTL_DES_FLAGS_PRIORITY);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    sendreq->req_descriptor = descriptor; /* hang on to this for later */
    segment = descriptor->des_src;

    /* pack the data into the supplied buffer */
    iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)segment->seg_addr.pval + sizeof(mca_pml_dr_match_hdr_t));
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if(size > 0) { 
        if((rc = ompi_convertor_pack( &sendreq->req_send.req_base.req_convertor,
                                      &iov,
                                      &iov_count,
                                      &max_data)) < 0) {
            mca_bml_base_free(bml_btl, descriptor);
            return rc;
        }
    }

    /* build match header */
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_csum = 0;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_MATCH;
    hdr->hdr_common.hdr_dst = sendreq->req_endpoint->dst;
    hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_common.hdr_src = sendreq->req_endpoint->src;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_csum = (size > 0 && do_csum ? 
                               sendreq->req_send.req_base.req_convertor.checksum : OPAL_CSUM_ZERO);
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_common.hdr_vid =  sendreq->req_vfrag0.vf_id;
    hdr->hdr_common.hdr_csum = (do_csum ? 
                                opal_csum(hdr, sizeof(mca_pml_dr_match_hdr_t)) :
                                OPAL_CSUM_ZERO);
    
    /* vfrag status */
    sendreq->req_vfrag0.vf_size = max_data;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    sendreq->req_vfrag0.vf_pending = 1;

    /* short message */
    descriptor->des_cbfunc = mca_pml_dr_match_completion;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = &sendreq->req_vfrag0;
    segment->seg_len = sizeof(mca_pml_dr_match_hdr_t) + max_data;

    /* signal request completion */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* send */
    MCA_PML_DR_VFRAG_WDOG_START(&sendreq->req_vfrag0);
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    }
    return rc;
}

/**
 *  BTL can send directly from user buffer so allow the BTL
 *  to prepare the segment list.
 */

int mca_pml_dr_send_request_start_prepare(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size)
{
    mca_btl_base_descriptor_t* descriptor;
    mca_btl_base_segment_t* segment;
    mca_pml_dr_hdr_t* hdr;
    int rc;
    bool do_csum = mca_pml_dr.enable_csum && 
        (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 

    /* prepare descriptor */
    mca_bml_base_prepare_src( bml_btl, 
                              NULL,
                              &sendreq->req_send.req_base.req_convertor,
                              MCA_BTL_NO_ORDER,
                              sizeof(mca_pml_dr_match_hdr_t),
                              &size,
                              MCA_BTL_DES_FLAGS_PRIORITY,
                              &descriptor );
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    sendreq->req_descriptor = descriptor; /* hang on to this for later */
    segment = descriptor->des_src;

    /* build match header */
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_csum = 0;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_MATCH;
    hdr->hdr_common.hdr_dst = sendreq->req_endpoint->dst;
    hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_common.hdr_src = sendreq->req_endpoint->src;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_csum = (size > 0 && do_csum ? 
                               sendreq->req_send.req_base.req_convertor.checksum : OPAL_CSUM_ZERO); 
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_common.hdr_vid =  sendreq->req_vfrag0.vf_id;
    hdr->hdr_common.hdr_csum = (do_csum ? 
                                opal_csum(hdr, sizeof(mca_pml_dr_match_hdr_t)) :
                                OPAL_CSUM_ZERO);

    /* short message */
    descriptor->des_cbfunc = mca_pml_dr_match_completion;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = &sendreq->req_vfrag0;
       
    /* vfrag state */
    sendreq->req_vfrag0.vf_size = size;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    sendreq->req_vfrag0.vf_pending = 1;

    /* send */
    MCA_PML_DR_VFRAG_WDOG_START(&sendreq->req_vfrag0);
    rc = mca_bml_base_send(bml_btl, descriptor, MCA_BTL_TAG_PML); 
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, descriptor );
    }
    return rc;
}


/**
 *  Rendezvous is required. Eager send up to
 *  the btls eager limit.
 */

int mca_pml_dr_send_request_start_rndv(
    mca_pml_dr_send_request_t* sendreq,
    mca_bml_base_btl_t* bml_btl,
    size_t size,
    int flags)
{
    mca_btl_base_descriptor_t* des;
    mca_btl_base_segment_t* segment;
    mca_pml_dr_hdr_t* hdr;
    int rc;
    bool do_csum = mca_pml_dr.enable_csum && 
        (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 
    
    
    /* prepare descriptor */
    if(size == 0) {
        mca_bml_base_alloc( bml_btl, 
                            &des, 
                            MCA_BTL_NO_ORDER,
                            sizeof(mca_pml_dr_rendezvous_hdr_t),
                            MCA_BTL_DES_FLAGS_PRIORITY ); 
    } else {
        mca_bml_base_prepare_src( bml_btl, 
                                  NULL,
                                  &sendreq->req_send.req_base.req_convertor,
                                  MCA_BTL_NO_ORDER,
                                  sizeof(mca_pml_dr_rendezvous_hdr_t),
                                  &size,
                                  MCA_BTL_DES_FLAGS_PRIORITY,
                                  &des );
    }

    if(NULL == des) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    sendreq->req_descriptor = des; /*hang on to this for later */
    segment = des->des_src;
    
    /* build hdr */
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_RNDV;
    hdr->hdr_common.hdr_dst = sendreq->req_endpoint->dst;
    hdr->hdr_common.hdr_src = sendreq->req_endpoint->src;
    hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_common.hdr_vid =  sendreq->req_vfrag0.vf_id;
    hdr->hdr_common.hdr_csum = 0;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_match.hdr_csum = size > 0 ? sendreq->req_send.req_base.req_convertor.checksum : OPAL_CSUM_ZERO;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_common.hdr_csum = (do_csum ? 
                                opal_csum(hdr, sizeof(mca_pml_dr_rendezvous_hdr_t)) :
                                OPAL_CSUM_ZERO);
    

    /* first fragment of a long message */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbdata = &sendreq->req_vfrag0;
    des->des_cbfunc = mca_pml_dr_rndv_completion;

    /* vfrag state */
    sendreq->req_vfrag0.vf_size = size;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    sendreq->req_vfrag0.vf_state |= MCA_PML_DR_VFRAG_RNDV;
    sendreq->req_vfrag0.vf_pending = 1;

    /* send */
    MCA_PML_DR_VFRAG_WDOG_START(&sendreq->req_vfrag0);
    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        mca_bml_base_free(bml_btl, des );
    }
    return rc;
}

/**
 *  Schedule pipeline of send descriptors for the given request,
 *  using send protocol.
 */

int mca_pml_dr_send_request_schedule(mca_pml_dr_send_request_t* sendreq)
{ 
    /*
     * Only allow one thread in this routine for a given request.
     * However, we cannot block callers on a mutex, so simply keep track
     * of the number of times the routine has been called and run through
     * the scheduling logic once for every call.
    */
        
    mca_pml_dr_endpoint_t* endpoint = sendreq->req_endpoint;
    assert(sendreq->req_vfrag0.vf_recv.pval != NULL);
    if(OPAL_THREAD_ADD32(&sendreq->req_lock,1) == 1) {
        do {
            size_t bytes_remaining;
            /*
             * VFrags w/ nacks or that timed out
             */
            while(opal_list_get_size(&sendreq->req_retrans) &&
                  sendreq->req_pipeline_depth < mca_pml_dr.send_pipeline_depth) {
                mca_pml_dr_vfrag_t* vfrag;

                OPAL_THREAD_LOCK(&ompi_request_lock);
                vfrag = (mca_pml_dr_vfrag_t*)opal_list_get_first(&sendreq->req_retrans);
                OPAL_THREAD_UNLOCK(&ompi_request_lock);
                if(NULL == vfrag) {
                    break;
                }
                
                /*
                 * Retransmit fragments that have not been acked.
                 */
                while(vfrag->vf_idx < vfrag->vf_len && 
                      sendreq->req_pipeline_depth < mca_pml_dr.send_pipeline_depth) {
                    if(((uint64_t)1 << vfrag->vf_idx) & ~vfrag->vf_ack) {
                        mca_bml_base_btl_t* bml_btl = vfrag->bml_btl; 
                        mca_pml_dr_frag_hdr_t* hdr;
                        mca_btl_base_descriptor_t* des;
                        size_t offset_in_vfrag = vfrag->vf_max_send_size * vfrag->vf_idx;
                        size_t offset_in_msg = vfrag->vf_offset + offset_in_vfrag;
                        size_t size;
                        int rc;
                        bool do_csum = mca_pml_dr.enable_csum && 
                            (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 
                        
                        if(vfrag->vf_idx == vfrag->vf_len - 1) {
                            size = vfrag->vf_size - offset_in_vfrag;
                        } else {
                            size = vfrag->vf_max_send_size;
                        }

                        /* pack into a descriptor */
                        ompi_convertor_set_position(&sendreq->req_send.req_base.req_convertor, &offset_in_msg);
                        mca_bml_base_prepare_src( bml_btl, 
                                                 NULL, 
                                                 &sendreq->req_send.req_base.req_convertor,
                                                 MCA_BTL_NO_ORDER,
                                                 sizeof(mca_pml_dr_frag_hdr_t),
                                                 &size,
                                                 0,
                                                 &des );
                        if(des == NULL) {
                            OPAL_THREAD_LOCK(&ompi_request_lock);
                            opal_list_remove_item(&mca_pml_dr.send_active, (opal_list_item_t*)sendreq);
                            opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                            OPAL_THREAD_UNLOCK(&ompi_request_lock);
                            break;
                        }
                        des->des_cbfunc = mca_pml_dr_frag_completion;
                        des->des_cbdata = vfrag;

                        /* setup header */
                        hdr = (mca_pml_dr_frag_hdr_t*)des->des_src->seg_addr.pval;
                        hdr->hdr_common.hdr_flags = 0;
                        hdr->hdr_common.hdr_csum = 0;
                        hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_FRAG;
                        hdr->hdr_common.hdr_dst = sendreq->req_endpoint->dst; 
                        hdr->hdr_common.hdr_vid = vfrag->vf_id;
                        hdr->hdr_common.hdr_src = sendreq->req_endpoint->src;
                        hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
                        hdr->hdr_vlen = vfrag->vf_len;
                        hdr->hdr_frag_idx = vfrag->vf_idx;
                        hdr->hdr_frag_csum = sendreq->req_send.req_base.req_convertor.checksum; 
                        hdr->hdr_frag_offset = offset_in_msg;
                        hdr->hdr_src_ptr.pval = vfrag;
                        hdr->hdr_dst_ptr = sendreq->req_vfrag0.vf_recv;
                        hdr->hdr_common.hdr_csum = (do_csum ? 
                                                    opal_csum(hdr, sizeof(mca_pml_dr_frag_hdr_t)) : 
                                                    OPAL_CSUM_ZERO);
                        
                        /* adjust number of outstanding operations */
                        if(OPAL_THREAD_ADD64(&vfrag->vf_pending, 1) == 1) {
                            MCA_PML_DR_VFRAG_WDOG_START(vfrag);
                        }
                        OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,1);

                        /* adjust send offset - may not have finished scheduling entire vfrag */
                        if(offset_in_msg + size > sendreq->req_send_offset) {
                            sendreq->req_send_offset = offset_in_msg + size;
                        }

                        /* initiate send - note that this may complete before the call returns */
                        rc = mca_bml_base_send( bml_btl, des, MCA_BTL_TAG_PML);
                        
                        if(rc == OMPI_SUCCESS) {
                            bytes_remaining -= size;
                        } else {
                            OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
                            mca_bml_base_free(bml_btl,des);
                            OPAL_THREAD_LOCK(&ompi_request_lock);
                            opal_list_remove_item(&mca_pml_dr.send_active, (opal_list_item_t*) sendreq);
                            opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                            OPAL_THREAD_UNLOCK(&ompi_request_lock);
                            break;
                        }
                    }
                    vfrag->vf_idx++;
                }

                /* remove from retrans list */
                if(vfrag->vf_idx == vfrag->vf_len) {
                    OPAL_THREAD_LOCK(&ompi_request_lock);
                    opal_list_remove_item(&sendreq->req_retrans, (opal_list_item_t*)vfrag);
                    vfrag->vf_state &= ~MCA_PML_DR_VFRAG_RETRANS;
                    OPAL_THREAD_UNLOCK(&ompi_request_lock);
                }
            }

            /* allocate remaining bytes to BTLs */
            bytes_remaining = sendreq->req_send.req_bytes_packed - sendreq->req_send_offset;
            while(bytes_remaining > 0 && 
                  sendreq->req_pipeline_depth < mca_pml_dr.send_pipeline_depth) {
                
                mca_pml_dr_frag_hdr_t* hdr;
                mca_btl_base_descriptor_t* des;
                mca_bml_base_btl_t* bml_btl = NULL; 
                mca_pml_dr_vfrag_t* vfrag = sendreq->req_vfrag;
                size_t size = bytes_remaining;
                
                /* offset tells us how much of the vfrag has been scheduled */
                size_t bytes_sent = sendreq->req_send_offset - vfrag->vf_offset;
                int rc;
                bool do_csum;
                    
                /* do we need to allocate a new vfrag 
                   (we scheduled all the vfrag already) */
                if(vfrag->vf_size == bytes_sent) {
                    bml_btl = mca_bml_base_btl_array_get_next(&endpoint->bml_endpoint->btl_send); 
                    MCA_PML_DR_VFRAG_ALLOC(vfrag,rc);
                    if(NULL == vfrag) {
                        OPAL_THREAD_LOCK(&mca_pml_dr.lock);
                        opal_list_remove_item(&mca_pml_dr.send_active, (opal_list_item_t*)sendreq);
                        opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                        OPAL_THREAD_UNLOCK(&mca_pml_dr.lock);
                        break;
                    }
                    MCA_PML_DR_SEND_REQUEST_VFRAG_INIT(sendreq,endpoint,bytes_remaining,vfrag);
                    vfrag->bml_btl = bml_btl;
                    bytes_sent = 0;
                    
                } else {  /* always schedule the vfrag accross the same btl */
                    bml_btl = vfrag->bml_btl;
                }

                do_csum = mca_pml_dr.enable_csum && 
                    (bml_btl->btl_flags & MCA_BTL_FLAGS_NEED_CSUM); 
                
                /* makes sure that we don't exceed vfrag size */
                if (size > vfrag->vf_max_send_size) {
                    size = vfrag->vf_max_send_size;
                }
                if (size > vfrag->vf_size - bytes_sent) {
                    size = vfrag->vf_size - bytes_sent;
                }

                /* pack into a descriptor */
                ompi_convertor_set_position(&sendreq->req_send.req_base.req_convertor, &sendreq->req_send_offset);
                mca_bml_base_prepare_src( bml_btl, 
                                          NULL, 
                                          &sendreq->req_send.req_base.req_convertor,
                                          MCA_BTL_NO_ORDER,
                                          sizeof(mca_pml_dr_frag_hdr_t),
                                          &size,
                                          0,
                                          &des );
                if(des == NULL) {
                    OPAL_THREAD_LOCK(&mca_pml_dr.lock);
                    opal_list_remove_item(&mca_pml_dr.send_active, (opal_list_item_t*)sendreq);
                    opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                    OPAL_THREAD_UNLOCK(&mca_pml_dr.lock);
                    break;
                }
                des->des_cbfunc = mca_pml_dr_frag_completion;
                des->des_cbdata = vfrag;

                /* setup header */
                hdr = (mca_pml_dr_frag_hdr_t*)des->des_src->seg_addr.pval;
                hdr->hdr_common.hdr_flags = 0;
                hdr->hdr_common.hdr_csum = 0;
                hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_FRAG;
                hdr->hdr_common.hdr_dst = sendreq->req_endpoint->dst; 
                hdr->hdr_common.hdr_vid = vfrag->vf_id;
                hdr->hdr_common.hdr_src = sendreq->req_endpoint->src;
                hdr->hdr_common.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
                hdr->hdr_vlen = vfrag->vf_len;
                hdr->hdr_frag_idx = vfrag->vf_idx;
                hdr->hdr_frag_csum =  (do_csum ? 
                                       sendreq->req_send.req_base.req_convertor.checksum : OPAL_CSUM_ZERO);
                hdr->hdr_frag_offset = sendreq->req_send_offset;
                hdr->hdr_src_ptr.pval = vfrag;
                hdr->hdr_dst_ptr = sendreq->req_vfrag0.vf_recv;
                hdr->hdr_common.hdr_csum = (do_csum ? 
                                            opal_csum(hdr, sizeof(mca_pml_dr_frag_hdr_t)): OPAL_CSUM_ZERO);
                
                assert(hdr->hdr_frag_offset < sendreq->req_send.req_bytes_packed);
                
                /* update state */
                vfrag->vf_idx++;
                if(OPAL_THREAD_ADD64(&vfrag->vf_pending,1) == 1) {
                    MCA_PML_DR_VFRAG_WDOG_START(vfrag);
                }
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
                    OPAL_THREAD_LOCK(&ompi_request_lock);
                    opal_list_remove_item(&mca_pml_dr.send_active, (opal_list_item_t*)sendreq);
                    opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                    OPAL_THREAD_UNLOCK(&ompi_request_lock);
                    break;
                }
            }
        } while (OPAL_THREAD_ADD32(&sendreq->req_lock,-1) > 0);
    }
    return OMPI_SUCCESS;
} 


/**
 *  Acknowledgment of match vfrag.
 */

void mca_pml_dr_send_request_match_ack(
    mca_btl_base_module_t* btl,
    mca_pml_dr_ack_hdr_t* ack)
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)ack->hdr_src_ptr.pval;
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;
    
    OPAL_THREAD_LOCK(&ompi_request_lock);
 
    vfrag->vf_ack = ack->hdr_vmask & vfrag->vf_mask;
    if (vfrag->vf_pending == 0) {
        MCA_PML_DR_VFRAG_ACK_STOP(vfrag);
        /* need to retransmit? */
        if(vfrag->vf_ack != vfrag->vf_mask) {
            MCA_PML_DR_VFRAG_WDOG_START(vfrag);
            MCA_PML_DR_SEND_REQUEST_EAGER_RETRY(sendreq, vfrag);
        } else { 
            /* if already have local completion free descriptor and complete message */
            /* return descriptor */
            if(NULL != sendreq->req_descriptor) {
                if(NULL != sendreq->req_descriptor->des_context) { 
                    mca_bml_base_free((mca_bml_base_btl_t*)sendreq->req_descriptor->des_context, sendreq->req_descriptor ); 
                }
                sendreq->req_descriptor = NULL;
            }
            
            /* update statistics */
            sendreq->req_bytes_delivered = vfrag->vf_size;
            MCA_PML_DR_DEBUG(1, (0, "%s:%d: inserting vid %d into sequence tracker\n", 
                                 __FILE__, __LINE__, vfrag->vf_id));
            ompi_seq_tracker_insert(&sendreq->req_endpoint->seq_sends, vfrag->vf_id);
            MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
        }

    /* wait for local completion */
    } else {
        /* need to retransmit? */
        if (vfrag->vf_ack != vfrag->vf_mask) {
            vfrag->vf_state |= MCA_PML_DR_VFRAG_NACKED;
        } else {
            vfrag->vf_recv = ack->hdr_dst_ptr;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}

/**
 * Acknowledgment of rendezvous vfrag.
 */

void mca_pml_dr_send_request_rndv_ack(
    mca_btl_base_module_t* btl,
    mca_pml_dr_ack_hdr_t* ack)
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)ack->hdr_src_ptr.pval;
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;
    
    OPAL_THREAD_LOCK(&ompi_request_lock);
    
    /* set acked bits */
    vfrag->vf_ack = ack->hdr_vmask & vfrag->vf_mask;

    /* local completion? */
    if (vfrag->vf_pending == 0) { 
        bool schedule = false;
        MCA_PML_DR_VFRAG_ACK_STOP(vfrag);

        /* need to retransmit? */
        if(vfrag->vf_ack != vfrag->vf_mask) {
            /* got a NACK, resend eager data! */
            MCA_PML_DR_VFRAG_WDOG_START(vfrag);
            MCA_PML_DR_SEND_REQUEST_EAGER_RETRY(sendreq, vfrag);
            OPAL_THREAD_UNLOCK(&ompi_request_lock);
        } else {
            /* return descriptor of first fragment */
            if(NULL != sendreq->req_descriptor) {
                if(NULL != sendreq->req_descriptor->des_context) { 
                    mca_bml_base_free((mca_bml_base_btl_t*)sendreq->req_descriptor->des_context, sendreq->req_descriptor); 
                }
                sendreq->req_descriptor = NULL;
            }

            /* done? */
            sendreq->req_send_offset = ack->hdr_vlen;
            sendreq->req_bytes_delivered = ack->hdr_vlen;
            if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
                MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
            } else {
                /* start scheduling with a new vfrag */
                vfrag->vf_recv = ack->hdr_dst_ptr;
/*                 vfrag->vf_state = 0; */
/*                 sendreq->req_send_offset = ack->hdr_vlen; */
                vfrag->vf_state = 0;
                vfrag->vf_size = ack->hdr_vlen;
                schedule = true;
            } 

            /* stash the vfrag id for duplicate acks.. */
            MCA_PML_DR_DEBUG(1, (0, "%s:%d: inserting vid %d into sequence tracker\n", 
                                 __FILE__, __LINE__, vfrag->vf_id));
            
            ompi_seq_tracker_insert(&sendreq->req_endpoint->seq_sends, vfrag->vf_id);
            OPAL_THREAD_UNLOCK(&ompi_request_lock);
            
            if(schedule) {
                mca_pml_dr_send_request_schedule(sendreq);
            }
        }

    /* wait for local completion */
    } else {
        /* need to retransmit? */
        if(vfrag->vf_ack != vfrag->vf_mask) {
            vfrag->vf_state |= MCA_PML_DR_VFRAG_NACKED;
        } else { 
            /* will need this to schedule rest of the message */
            vfrag->vf_recv = ack->hdr_dst_ptr;
            vfrag->vf_state = 0;
            vfrag->vf_size = ack->hdr_vlen;
            sendreq->req_send_offset = ack->hdr_vlen;
            sendreq->req_bytes_delivered = ack->hdr_vlen;
        }
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }
}

/**
 * Acknowledgment of vfrag.
 */

void mca_pml_dr_send_request_frag_ack(
    mca_btl_base_module_t* btl,
    mca_pml_dr_ack_hdr_t* ack)
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)ack->hdr_src_ptr.pval;
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;
    bool schedule = false;

    MCA_PML_DR_VFRAG_ACK_STOP(vfrag);
    OPAL_THREAD_LOCK(&ompi_request_lock);
    
    /* add in acknowledged fragments */
    vfrag->vf_ack |= (ack->hdr_vmask & vfrag->vf_mask);

    /* need to retransmit? */
    if(vfrag->vf_ack != vfrag->vf_mask) {
        MCA_PML_DR_DEBUG(0,(0, "got a vfrag NACK, retransmitting %llx\n", (long long)~vfrag->vf_ack));
        MCA_PML_DR_SEND_REQUEST_VFRAG_RETRANS(sendreq,vfrag);
        schedule = true;

    /* acked and local completion */
    } else if (vfrag->vf_pending == 0 && vfrag->vf_idx == vfrag->vf_len) {

        /* update statistics */
        sendreq->req_bytes_delivered += vfrag->vf_size;
        assert(sendreq->req_bytes_delivered <= sendreq->req_send.req_bytes_packed);

        /* stash the vfid for duplicate acks.. */
        MCA_PML_DR_DEBUG(1,(0, "%s:%d: inserting vid %d into sequence tracker\n", 
                            __FILE__, __LINE__, vfrag->vf_id));
            
        ompi_seq_tracker_insert(&sendreq->req_endpoint->seq_sends, vfrag->vf_id);
        /* return vfrag */
        MCA_PML_DR_VFRAG_RETURN(vfrag);
            
        /* are we done with this request ? */
        if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
            MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
        /* is there something left to schedule */
        } else if (sendreq->req_send_offset < sendreq->req_send.req_bytes_packed) {
            schedule = true;
        }
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    if(schedule) {
        mca_pml_dr_send_request_schedule(sendreq);
    }
}


void mca_pml_dr_sendreq_cleanup_active(mca_btl_base_module_t* btl) {  
   opal_list_item_t* item;
    
    for (item = opal_list_get_first(&mca_pml_dr.send_active) ;
         item != opal_list_get_end(&mca_pml_dr.send_active) ;
         item = opal_list_get_next(item)) {
        mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*) item;
        mca_btl_base_descriptor_t* des = sendreq->req_descriptor;
        /* The des may be NULL if 
         * first fragment of rndv was delivered */
        if (NULL != des) {
            mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
            if( bml_btl && bml_btl->btl == btl) { 
                des->des_context = NULL;
            }
        }
            
    }
}
