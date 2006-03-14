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


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "opal/util/crc.h"
#include "ompi/constants.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/mpool.h" 
#include "pml_dr.h"
#include "pml_dr_hdr.h"
#include "pml_dr_proc.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_recvreq.h"
#include "ompi/mca/bml/base/base.h"


static int mca_pml_dr_send_request_fini(struct ompi_request_t** request)
{
    mca_pml_dr_send_request_t* sendreq = *(mca_pml_dr_send_request_t**)(request); 
    if(sendreq->req_send.req_base.req_persistent) {
       if(sendreq->req_send.req_base.req_free_called) {
           MCA_PML_DR_FREE(request);
       } else {
           sendreq->req_send.req_base.req_ompi.req_state = OMPI_REQUEST_INACTIVE; 
           /* rewind convertor */ 
           if(sendreq->req_send.req_bytes_packed) {
               size_t offset = 0;
               ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset);
           }
           /* if buffered send - release any resources */
           if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED &&
               sendreq->req_send.req_addr != sendreq->req_send.req_base.req_addr) {
               mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);
           }
       }
    } else {
        MCA_PML_DR_FREE(request);
    }
    return OMPI_SUCCESS;
}

static int mca_pml_dr_send_request_free(struct ompi_request_t** request)
{
    MCA_PML_DR_FREE(request);
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
    req->req_vfrag0.vf_idx = 1;
    req->req_vfrag0.vf_ack = 0;
    req->req_vfrag0.vf_mask = 1;
    req->req_vfrag0.vf_mask_processed = 0;
    req->req_vfrag0.vf_send.pval = req;
    req->req_send.req_base.req_type = MCA_PML_REQUEST_SEND;
    req->req_send.req_base.req_ompi.req_fini = mca_pml_dr_send_request_fini;
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
 * Completion of a short message - nothing left to schedule.
 */

static void mca_pml_dr_match_completion(
    struct mca_btl_base_module_t* btl,  
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_dr_send_request_t* sendreq = descriptor->des_cbdata;
    mca_pml_dr_vfrag_t* vfrag = &sendreq->req_vfrag0;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    OPAL_THREAD_LOCK(&ompi_request_lock);

    /* local completion */
    assert(vfrag->vf_mask_processed == 0);
    vfrag->vf_mask_processed |= 0x1;

    /* been acked? */
    if(vfrag->vf_ack == vfrag->vf_mask) {

        /* return descriptor */
        if(NULL != sendreq->descriptor) {
            mca_bml_base_free(sendreq->descriptor->des_context, sendreq->descriptor); 
            sendreq->descriptor = NULL;
        }

        /* update statistics */
        sendreq->req_bytes_delivered = sendreq->req_send.req_bytes_packed;
        MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
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
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)descriptor->des_cbdata;
    mca_pml_dr_vfrag_t* vfrag = &sendreq->req_vfrag0;
    bool schedule = false;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    OPAL_THREAD_LOCK(&ompi_request_lock);

    /* local completion */
    vfrag->vf_mask_processed |= 0x1;
    
    /* been acked? */
    if(vfrag->vf_ack == vfrag->vf_mask) {

        if(sendreq->descriptor) {
            mca_bml_base_free(sendreq->descriptor->des_context, sendreq->descriptor);
            sendreq->descriptor = NULL;
        }
        sendreq->req_bytes_delivered = vfrag->vf_size;
        if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed){
            MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
        } else {
            schedule = true;
        }
    } 
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    if(schedule) {
        mca_pml_dr_send_request_schedule(sendreq);
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
    mca_pml_dr_vfrag_t* vfrag = descriptor->des_cbdata;
    mca_pml_dr_send_request_t* sendreq  = vfrag->vf_send.pval;
    mca_bml_base_btl_t* bml_btl = vfrag->bml_btl;
    mca_pml_dr_frag_hdr_t* hdr = (mca_pml_dr_frag_hdr_t*)descriptor->des_src->seg_addr.pval;
    bool schedule = false;
    uint64_t bit;
    
    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        opal_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    OPAL_THREAD_LOCK(&ompi_request_lock);
    bit = ((uint64_t)1 << hdr->hdr_frag_idx); 
    vfrag->vf_mask_processed |= bit;

    /* update pipeline depth */
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
    
    /* when we have local completion of the entire vfrag 
       we stop the local wdog timers and set our ack timer 
       as the peer should be sending us an ack for the vfrag
    */
    if(vfrag->vf_mask_processed == vfrag->vf_mask) {
        MCA_PML_DR_VFRAG_WDOG_STOP(vfrag);
        /* has the vfrag already been acked */
        if(vfrag->vf_ack == vfrag->vf_mask) {

            /* check to see if we need to schedule the remainder of the message */
            sendreq->req_bytes_delivered += vfrag->vf_size;

            /* return this vfrag */
            MCA_PML_DR_VFRAG_RETURN(vfrag);

        } else {
            /* MCA_PML_DR_VFRAG_ACK_START(vfrag); */
        }
    } else { 
        MCA_PML_DR_VFRAG_WDOG_RESET(vfrag);
    }

    /* are we done with this request ? */
    if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
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
    int32_t free_after;
    int rc;
    uint32_t csum; 
    
    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, sizeof(mca_pml_dr_rendezvous_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    sendreq->descriptor = descriptor; /* hang on to this for later */
    segment = descriptor->des_src;
    
    /* pack the data into the BTL supplied buffer */
    iov.iov_base = (void*)((unsigned char*)segment->seg_addr.pval + 
             sizeof(mca_pml_dr_rendezvous_hdr_t));
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
    csum = sendreq->req_send.req_convertor.checksum;

    /* update lengths */
    segment->seg_len = sizeof(mca_pml_dr_rendezvous_hdr_t) + max_data;
    sendreq->req_send_offset = max_data;
    sendreq->req_vfrag0.vf_size = max_data;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    
    descriptor->des_cbfunc = mca_pml_dr_rndv_completion;
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

    /* build rendezvous header */
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_csum = 0;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_vid = sendreq->req_vfrag0.vf_id;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_csum = csum;
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_rendezvous_hdr_t));
    
    /* re-init convertor for packed data */
    ompi_convertor_prepare_for_send(
            &sendreq->req_send.req_convertor,
            sendreq->req_send.req_datatype,
            sendreq->req_send.req_count,
            sendreq->req_send.req_addr);

    /* request is complete at mpi level */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);
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
    int32_t free_after;
    int rc;
    
    
    /* allocate descriptor */
    mca_bml_base_alloc(bml_btl, &descriptor, sizeof(mca_pml_dr_match_hdr_t) + size);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    sendreq->descriptor = descriptor; /* hang on to this for later */
    segment = descriptor->des_src;

    /* pack the data into the supplied buffer */
    iov.iov_base = (void*)((unsigned char*)segment->seg_addr.pval + sizeof(mca_pml_dr_match_hdr_t));
    iov.iov_len = size;
    iov_count = 1;
    max_data = size;
    if(size > 0) { 
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
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_csum = 0;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_csum = size > 0 ? sendreq->req_send.req_convertor.checksum : MCA_PML_DR_CSUM_ZERO;
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_match.hdr_vid =  sendreq->req_vfrag0.vf_id;
    hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_match_hdr_t));
    
    /* update lengths */
    segment->seg_len = sizeof(mca_pml_dr_match_hdr_t) + max_data;
    sendreq->req_send_offset = max_data;
    sendreq->req_vfrag0.vf_size = max_data;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    
    /* short message */
    descriptor->des_cbfunc = mca_pml_dr_match_completion;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;

    /* signal request completion */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    MCA_PML_DR_SEND_REQUEST_MPI_COMPLETE(sendreq);
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

    /* prepare descriptor */
    mca_bml_base_prepare_src(
            bml_btl, 
            NULL,
            &sendreq->req_send.req_convertor,
            sizeof(mca_pml_dr_match_hdr_t),
            &size,
            &descriptor);
    if(NULL == descriptor) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    sendreq->descriptor = descriptor; /* hang on to this for later */
    segment = descriptor->des_src;

    /* build match header */
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_csum = 0;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_MATCH;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_csum = size > 0 ? sendreq->req_send.req_convertor.checksum : MCA_PML_DR_CSUM_ZERO; 
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_match.hdr_vid =  sendreq->req_vfrag0.vf_id;
    hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_match_hdr_t));

    /* short message */
    descriptor->des_cbfunc = mca_pml_dr_match_completion;
    descriptor->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    descriptor->des_cbdata = sendreq;
       
    /* update lengths */
    sendreq->req_send_offset = size;
    sendreq->req_vfrag0.vf_size = size;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    /* send */
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

    
    /* prepare descriptor */
    if(size == 0) {
        mca_bml_base_alloc(
                           bml_btl, 
                           &des, 
                           sizeof(mca_pml_dr_rendezvous_hdr_t)
                           ); 
    } else {
        mca_bml_base_prepare_src(
                                 bml_btl, 
                                 NULL,
                                 &sendreq->req_send.req_convertor,
                                 sizeof(mca_pml_dr_rendezvous_hdr_t),
                                 &size,
                                 &des);
    }

    if(NULL == des) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    } 
    sendreq->descriptor = des; /* hang on to this for later */
    segment = des->des_src;
    
    /* build hdr */
    hdr = (mca_pml_dr_hdr_t*)segment->seg_addr.pval;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_RNDV;
    hdr->hdr_match.hdr_ctx = sendreq->req_send.req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
    hdr->hdr_match.hdr_seq = sendreq->req_send.req_base.req_sequence;
    hdr->hdr_match.hdr_src_ptr.pval = &sendreq->req_vfrag0;
    hdr->hdr_match.hdr_csum = size > 0 ? sendreq->req_send.req_convertor.checksum : MCA_PML_DR_CSUM_ZERO;
    hdr->hdr_match.hdr_vid =  sendreq->req_vfrag0.vf_id;
    hdr->hdr_rndv.hdr_msg_length = sendreq->req_send.req_bytes_packed;
    hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_rendezvous_hdr_t));
    
    /* first fragment of a long message */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbdata = sendreq;
    des->des_cbfunc = mca_pml_dr_rndv_completion;
    sendreq->req_send_offset = size;
    sendreq->req_vfrag0.vf_size = size;
    sendreq->req_vfrag0.bml_btl = bml_btl;
    
    /* send */
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
    
    mca_bml_base_endpoint_t* bml_endpoint = sendreq->req_endpoint;
    if(OPAL_THREAD_ADD32(&sendreq->req_lock,1) == 1) {
        do {
            /* allocate remaining bytes to BTLs */
            size_t bytes_remaining = sendreq->req_send.req_bytes_packed - sendreq->req_send_offset;
            while(bytes_remaining > 0 && 
                  sendreq->req_pipeline_depth < mca_pml_dr.send_pipeline_depth) {
                
                mca_pml_dr_frag_hdr_t* hdr;
                mca_btl_base_descriptor_t* des;
                mca_bml_base_btl_t* bml_btl = NULL; 
                mca_pml_dr_vfrag_t* vfrag = sendreq->req_vfrag;
                size_t size = bytes_remaining;

                /* offset tells us how much of the vfrag has been scheduled */
                size_t offset = sendreq->req_send_offset - vfrag->vf_offset;
                int rc;

                /* do we need to allocate a new vfrag 
                   (we scheduled all the vfrag already) */
                if(vfrag->vf_size == offset) {
                    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_send); 
                    MCA_PML_DR_VFRAG_ALLOC(vfrag,rc);
                    if(NULL == vfrag) {
                        OPAL_THREAD_LOCK(&mca_pml_dr.lock);
                        opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                        OPAL_THREAD_UNLOCK(&mca_pml_dr.lock);
                        break;
                    }
                    MCA_PML_DR_SEND_REQUEST_VFRAG_INIT(sendreq,bml_endpoint,bytes_remaining,vfrag);
                    vfrag->bml_btl = bml_btl;
                    offset = 0;
                    
                } else {  /* always schedule the vfrag accross the same btl */
                    bml_btl = vfrag->bml_btl;
                }

                /* makes sure that we don't exceed vfrag size */
                if (size > vfrag->vf_max_send_size) {
                    size = vfrag->vf_max_send_size;
                }
                if (size > vfrag->vf_size - offset) {
                    size = vfrag->vf_size - offset;
                }

                /* pack into a descriptor */
                ompi_convertor_set_position(&sendreq->req_send.req_convertor, &sendreq->req_send_offset);
                mca_bml_base_prepare_src(
                                         bml_btl, 
                                         NULL, 
                                         &sendreq->req_send.req_convertor,
                                         sizeof(mca_pml_dr_frag_hdr_t),
                                         &size,
                                         &des
                                         );
                if(des == NULL) {
                    OPAL_THREAD_LOCK(&mca_pml_dr.lock);
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
                hdr->hdr_vid = vfrag->vf_id;
                hdr->hdr_vlen = vfrag->vf_len;
                hdr->hdr_frag_idx = vfrag->vf_idx;
                hdr->hdr_frag_csum =  sendreq->req_send.req_convertor.checksum;
                hdr->hdr_frag_offset = sendreq->req_send_offset;
                hdr->hdr_src_ptr.pval = vfrag;

                hdr->hdr_dst_ptr = sendreq->req_vfrag0.vf_recv;
                hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_frag_hdr_t));

                /* update state */
                vfrag->vf_idx++;
                sendreq->req_send_offset += size;
                OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,1);

                /* start vfrag watchdog timer if this is the first part of the vfrag*/
                if(vfrag->vf_idx == 0) { 
                    MCA_PML_DR_VFRAG_WDOG_START(vfrag);
                }
                /* initiate send - note that this may complete before the call returns */
                rc = mca_bml_base_send( bml_btl, des, MCA_BTL_TAG_PML);
                
                if(rc == OMPI_SUCCESS) {
                    bytes_remaining -= size;
                } else {
                    sendreq->req_send_offset -= size;
                    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
                    mca_bml_base_free(bml_btl,des);
                    OPAL_THREAD_LOCK(&mca_pml_dr.lock);
                    opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                    OPAL_THREAD_UNLOCK(&mca_pml_dr.lock);
                    break;
                }
                mca_pml_dr_progress(); 
            }

            /*
             * VFrags w/ nacks or that timed out
             */
            while(opal_list_get_size(&sendreq->req_retrans) &&
                  sendreq->req_pipeline_depth < mca_pml_dr.send_pipeline_depth) {
                mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*)opal_list_get_first(&sendreq->req_retrans);

                /*
                 * Retransmit fragments that have not been acked.
                 */
                while(vfrag->vf_idx < vfrag->vf_len && 
                      sendreq->req_pipeline_depth < mca_pml_dr.send_pipeline_depth) {
                    if(((1 << vfrag->vf_idx) & vfrag->vf_mask_processed) == 0) {
                        mca_bml_base_btl_t* bml_btl = vfrag->bml_btl; 
                        mca_pml_dr_frag_hdr_t* hdr;
                        mca_btl_base_descriptor_t* des;
                        size_t offset = vfrag->vf_offset + (vfrag->vf_max_send_size * vfrag->vf_idx);
                        size_t size;
                        int rc;

                        
                        
                        if(vfrag->vf_idx == vfrag->vf_len - 1) {
                            size = vfrag->vf_size - offset;
                        } else {
                            size = vfrag->vf_max_send_size;
                        }

                        /* pack into a descriptor */
                        ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset);
                        mca_bml_base_prepare_src(
                                                 bml_btl, 
                                                 NULL, 
                                                 &sendreq->req_send.req_convertor,
                                                 sizeof(mca_pml_dr_frag_hdr_t),
                                                 &size,
                                                 &des
                                                 );
                        if(des == NULL) {
                            OPAL_THREAD_LOCK(&mca_pml_dr.lock);
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
                        hdr->hdr_vid = vfrag->vf_id;
                        hdr->hdr_vlen = vfrag->vf_len;
                        hdr->hdr_frag_idx = vfrag->vf_idx;
                        hdr->hdr_frag_csum = sendreq->req_send.req_convertor.checksum; 
                        hdr->hdr_frag_offset = sendreq->req_send_offset;
                        hdr->hdr_src_ptr.pval = vfrag;
                        hdr->hdr_dst_ptr = sendreq->req_vfrag0.vf_recv;
                        hdr->hdr_common.hdr_csum = opal_csum(hdr, sizeof(mca_pml_dr_frag_hdr_t));

                        /* update state */
                        vfrag->vf_idx++;
                        sendreq->req_send_offset += size;
                        OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,1);

                        /* reset the vfrag watchdog timer due to retransmission */
                        MCA_PML_DR_VFRAG_WDOG_RESET(vfrag);
                        
                        /* initiate send - note that this may complete before the call returns */
                        rc = mca_bml_base_send( bml_btl, des, MCA_BTL_TAG_PML);
                        
                        if(rc == OMPI_SUCCESS) {
                            bytes_remaining -= size;
                        } else {
                            vfrag->vf_idx--;
                            OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-1);
                            mca_bml_base_free(bml_btl,des);
                            OPAL_THREAD_LOCK(&mca_pml_dr.lock);
                            opal_list_append(&mca_pml_dr.send_pending, (opal_list_item_t*)sendreq);
                            OPAL_THREAD_UNLOCK(&mca_pml_dr.lock);
                            break;
                        }
                    }
                    vfrag->vf_idx++;
                }

                /* move from retrans to pending list */
                if(vfrag->vf_idx == vfrag->vf_len) {
                    OPAL_THREAD_LOCK(&ompi_request_lock);
                    opal_list_remove_item(&sendreq->req_retrans, (opal_list_item_t*)vfrag);
                    OPAL_THREAD_UNLOCK(&ompi_request_lock);
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
    mca_pml_dr_vfrag_t* vfrag = ack->hdr_src_ptr.pval;
    mca_pml_dr_send_request_t* sendreq = vfrag->vf_send.pval;

    OPAL_THREAD_LOCK(&ompi_request_lock);
    assert(vfrag->vf_ack == 0);
 
    /* need to retransmit? */
    if((ack->hdr_vmask & vfrag->vf_mask) != vfrag->vf_mask) {

        mca_bml_base_btl_t* bml_btl = sendreq->descriptor->des_context;
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        mca_bml_base_send(bml_btl, sendreq->descriptor, MCA_BTL_TAG_PML);

    /* if already have local completion free descriptor and complete message */
    } else if ((vfrag->vf_mask_processed & vfrag->vf_mask) == vfrag->vf_mask) {
      
        /* return descriptor */
        if(NULL != sendreq->descriptor) {
            mca_bml_base_free(sendreq->descriptor->des_context, sendreq->descriptor ); 
            sendreq->descriptor = NULL;
        }
        
        /* do NOT complete message until matched at peer */
        if (ack->hdr_common.hdr_flags & MCA_PML_DR_HDR_FLAGS_MATCHED) {
            /* update statistics */
            sendreq->req_bytes_delivered = vfrag->vf_size;
            MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
        }
        OPAL_THREAD_UNLOCK(&ompi_request_lock); 

    /* wait for local completion */
    } else {
        if (ack->hdr_common.hdr_flags & MCA_PML_DR_HDR_FLAGS_MATCHED) {
            vfrag->vf_ack = ack->hdr_vmask & vfrag->vf_mask;
        }
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
    }
}

/**
 * Acknowledgment of rndv vfrag.
 */

void mca_pml_dr_send_request_rndv_ack(
    mca_btl_base_module_t* btl,
    mca_pml_dr_ack_hdr_t* ack)
{
    mca_pml_dr_vfrag_t* vfrag = ack->hdr_src_ptr.pval;
    mca_pml_dr_send_request_t* sendreq = vfrag->vf_send.pval;

    OPAL_THREAD_LOCK(&ompi_request_lock);

    /* need to retransmit? */
    if((ack->hdr_vmask & vfrag->vf_mask) != vfrag->vf_mask) {

        mca_bml_base_btl_t* bml_btl = sendreq->descriptor->des_context;
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        mca_bml_base_send(bml_btl, sendreq->descriptor, MCA_BTL_TAG_PML);

    /* acked and local completion */
    } else if ((vfrag->vf_mask_processed & vfrag->vf_mask) == vfrag->vf_mask) {
        bool schedule = false;

        /* return descriptor for the first fragment */
        if(NULL != sendreq->descriptor) {
            mca_bml_base_free(sendreq->descriptor->des_context, sendreq->descriptor); 
            sendreq->descriptor = NULL;
        }

        /* do NOT schedule remainder of message until matched at peer */
        if (ack->hdr_common.hdr_flags & MCA_PML_DR_HDR_FLAGS_MATCHED) {
            sendreq->req_bytes_delivered = vfrag->vf_size;
            if(sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed){
                MCA_PML_DR_SEND_REQUEST_PML_COMPLETE(sendreq);
            } else { 
                vfrag->vf_recv = ack->hdr_dst_ptr;
                schedule = true;
            }

            /* vfrag has been matched at peer */
            vfrag->vf_ack = ack->hdr_vmask & vfrag->vf_mask;
        } 

        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        if(schedule) {
            mca_pml_dr_send_request_schedule(sendreq);
        }

    /* wait for local completion */
    } else {
        /* may need this to schedule rest of the message */
        vfrag->vf_recv = ack->hdr_dst_ptr;

        /* dont set ack until matched at peer */
        if (ack->hdr_common.hdr_flags & MCA_PML_DR_HDR_FLAGS_MATCHED) {
            vfrag->vf_ack = ack->hdr_vmask & vfrag->vf_mask;
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
    mca_pml_dr_vfrag_t* vfrag = ack->hdr_src_ptr.pval;
    mca_pml_dr_send_request_t* sendreq = vfrag->vf_send.pval;
    bool schedule = false;
    
    OPAL_THREAD_LOCK(&ompi_request_lock);

    /* add in acknowledged fragments */
    vfrag->vf_ack |= (ack->hdr_vmask & vfrag->vf_mask);

    /* need to retransmit? */
    if((vfrag->vf_ack & vfrag->vf_mask) != vfrag->vf_mask) {

        /* reset local completion flags to only those that have been successfully acked */
        vfrag->vf_mask_processed  = vfrag->vf_ack;
        vfrag->vf_idx = 0;
        opal_list_append(&sendreq->req_retrans, (opal_list_item_t*)vfrag);
        schedule = true;

    /* acked and local completion */
    } else if (vfrag->vf_mask_processed == vfrag->vf_mask) {

        /* update statistics */
        sendreq->req_bytes_delivered += vfrag->vf_size;

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

