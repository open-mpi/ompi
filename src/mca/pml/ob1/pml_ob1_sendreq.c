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


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "include/constants.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_endpoint.h"


                                                                                                         
static int mca_pml_ob1_send_request_fini(struct ompi_request_t** request)
{
    MCA_PML_OB1_FINI(request);
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_send_request_free(struct ompi_request_t** request)
{
    MCA_PML_OB1_FREE(request);
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
    req->req_send.req_base.req_ompi.req_fini = mca_pml_ob1_send_request_fini;
    req->req_send.req_base.req_ompi.req_free = mca_pml_ob1_send_request_free;
    req->req_send.req_base.req_ompi.req_cancel = mca_pml_ob1_send_request_cancel;
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
 *
 */

static void mca_pml_ob1_send_completion(
    mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* ep,
    struct mca_bmi_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_pml_ob1_endpoint_t* bmi_ep = sendreq->req_endpoint;

    /* for now - return the descriptor - may cache these at some point */
    MCA_PML_OB1_ENDPOINT_DES_RETURN(bmi_ep,descriptor);

    /* check for request completion */
    OMPI_THREAD_LOCK(&ompi_request_lock);
    if (OMPI_THREAD_ADD32(&sendreq->req_pending,-1) == 0 &&
        sendreq->req_offset == sendreq->req_send.req_bytes_packed) {
        sendreq->req_send.req_base.req_pml_complete = true;
        if (sendreq->req_send.req_base.req_ompi.req_complete == false) {
            sendreq->req_send.req_base.req_ompi.req_status.MPI_SOURCE = sendreq->req_send.req_base.req_comm->c_my_rank;
            sendreq->req_send.req_base.req_ompi.req_status.MPI_TAG = sendreq->req_send.req_base.req_tag;
            sendreq->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
            sendreq->req_send.req_base.req_ompi.req_status._count = sendreq->req_send.req_bytes_packed;
            sendreq->req_send.req_base.req_ompi.req_complete = true;
            sendreq->req_state = MCA_PML_OB1_SR_COMPLETE;
            if(ompi_request_waiting) {
                ompi_condition_broadcast(&ompi_request_cond);
            }
        } else if(sendreq->req_send.req_base.req_free_called) {
            MCA_PML_OB1_FREE((ompi_request_t**)&sendreq);
        } else if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {
            mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);
            sendreq->req_state = MCA_PML_OB1_SR_COMPLETE;
        }
    } 
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

    /* advance pending requests */
    while(NULL != sendreq) {
        switch(sendreq->req_state) {
            case MCA_PML_OB1_SR_SEND:
                mca_pml_ob1_send_request_schedule(sendreq);
                break;
            default:
                break;
        }
        OMPI_THREAD_LOCK(&mca_pml_ob1.ob1_lock);
        sendreq = (mca_pml_ob1_send_request_t*)ompi_list_remove_first(&mca_pml_ob1.send_pending);
        OMPI_THREAD_UNLOCK(&mca_pml_ob1.ob1_lock);
    }
}


/**
 *  BMI can send directly from user allocated memory.
 */

int mca_pml_ob1_send_request_start_user(
    mca_pml_ob1_send_request_t* sendreq,
    mca_pml_ob1_endpoint_t* endpoint)
{
    return OMPI_ERROR;
}


/**
 *  BMI requires "specially" allocated memory. Request a segment that
 *  is used for initial hdr and any eager data.
 */

int mca_pml_ob1_send_request_start_copy(
    mca_pml_ob1_send_request_t* sendreq,
    mca_pml_ob1_endpoint_t* endpoint)
{
    mca_bmi_base_descriptor_t* descriptor;
    mca_bmi_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    size_t size = sendreq->req_send.req_bytes_packed;
    int rc;

    /* shortcut for zero byte */
    if(size == 0) {

        /* allocate a descriptor */
        MCA_PML_OB1_ENDPOINT_DES_ALLOC(endpoint, descriptor);
        if(NULL == descriptor) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        } 
        descriptor->des_cbfunc = mca_pml_ob1_send_completion;
        segment = descriptor->des_src;

        /* build hdr */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = 0;
        hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;

        /* if an acknowledgment is not required - can get by w/ shorter hdr */
        if (sendreq->req_send.req_send_mode != MCA_PML_BASE_SEND_SYNCHRONOUS) {
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
            segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t);
            ompi_request_complete((ompi_request_t*)sendreq);
        } else {
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
            hdr->hdr_rndv.hdr_frag_length = 0;
            hdr->hdr_rndv.hdr_src_req.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
            hdr->hdr_rndv.hdr_src_req.pval = sendreq;
            segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t);
        }
       
    } else {

        struct iovec iov;
        unsigned int iov_count;
        unsigned int max_data;
        bool ack = false;

        /* determine first fragment size */
        if(size > endpoint->bmi_eager_limit - sizeof(mca_pml_ob1_hdr_t)) {
            size = endpoint->bmi_eager_limit - sizeof(mca_pml_ob1_hdr_t);
            ack = true;
        } else if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
            ack = true;
        }

        /* allocate space for hdr + first fragment */
        MCA_PML_OB1_ENDPOINT_DES_ALLOC(endpoint, descriptor);
        if(NULL == descriptor) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        descriptor->des_cbfunc = mca_pml_ob1_send_completion;
        segment = descriptor->des_src;

        /* build hdr */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = 0;
        hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;

        /* if an acknowledgment is not required - can get by w/ shorter hdr */
        if (ack == false) {
            int32_t free_after;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;

            /* pack the data into the supplied buffer */
            iov.iov_base = (unsigned char*)segment->seg_addr.pval + sizeof(mca_pml_ob1_match_hdr_t);
            iov.iov_len = size;
            iov_count = 1;
            max_data = size;
            if((rc = ompi_convertor_pack(
                &sendreq->req_send.req_convertor,
                &iov,
                &iov_count,
                &max_data,
                &free_after)) < 0) {
                endpoint->bmi_free(endpoint->bmi, descriptor);
                return rc;
            }
           
            /* update length w/ number of bytes actually packed */
            segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t) + max_data;

            sendreq->req_offset = max_data;
            if(sendreq->req_offset == sendreq->req_send.req_bytes_packed) {
                ompi_request_complete((ompi_request_t*)sendreq);
            }

        /* rendezvous header is required */
        } else {
            int32_t free_after;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
            hdr->hdr_rndv.hdr_src_req.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
            hdr->hdr_rndv.hdr_src_req.pval = sendreq;

            /* pack the data into the supplied buffer */
            iov.iov_base = (unsigned char*)segment->seg_addr.pval + sizeof(mca_pml_ob1_rendezvous_hdr_t);
            iov.iov_len = size;
            iov_count = 1;
            max_data = size;
            if((rc = ompi_convertor_pack(
                &sendreq->req_send.req_convertor,
                &iov,
                &iov_count,
                &max_data,
                &free_after)) < 0) {
                endpoint->bmi_free(endpoint->bmi, descriptor);
                return rc;
            }

            hdr->hdr_rndv.hdr_frag_length = max_data;
            segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t) + max_data;
            sendreq->req_offset = max_data;
        }
    }
    descriptor->des_cbdata = sendreq;
    OMPI_THREAD_ADD32(&sendreq->req_pending,1);

    /* send */
    rc = endpoint->bmi_send(
        endpoint->bmi, 
        endpoint->bmi_endpoint, 
        descriptor,
        MCA_BMI_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        endpoint->bmi_free(endpoint->bmi,descriptor);
    }
    return rc;
}


/**
 *
 */

int mca_pml_ob1_send_request_schedule(mca_pml_ob1_send_request_t* sendreq)
{ 
    /*
     * Only allow one thread in this routine for a given request.
     * However, we cannot block callers on a mutex, so simply keep track
     * of the number of times the routine has been called and run through
     * the scheduling logic once for every call.
    */
    if(OMPI_THREAD_ADD32(&sendreq->req_lock,1) == 1) {
        mca_pml_ob1_proc_t* proc = sendreq->req_proc;
        size_t num_bmi_avail = mca_pml_ob1_ep_array_get_size(&proc->bmi_next);
        do {
            /* allocate remaining bytes to PTLs */
            size_t bytes_remaining = sendreq->req_send.req_bytes_packed - sendreq->req_offset;
            while(bytes_remaining > 0 && sendreq->req_pending < mca_pml_ob1.send_pipeline_depth) {
                mca_pml_ob1_endpoint_t* ep = mca_pml_ob1_ep_array_get_next(&proc->bmi_next);
                mca_pml_ob1_frag_hdr_t* hdr;
                mca_bmi_base_descriptor_t* des;
                int rc;

                /* if this is the last PTL that is available to use, or the number of
                 * bytes remaining in the message is less than the PTLs minimum fragment
                 * size, then go ahead and give the rest of the message to this PTL.
                 */
                size_t size;
                if(bytes_remaining < ep->bmi_min_frag_size) {
                    size = bytes_remaining;

                /* otherwise attempt to give the PTL a percentage of the message
                 * based on a weighting factor. for simplicity calculate this as
                 * a percentage of the overall message length (regardless of amount
                 * previously assigned)
                 */
                } else if (num_bmi_avail > 1) {
                    size = (ep->bmi_weight * bytes_remaining) / 100;
                } else {
                    size = ep->bmi_min_frag_size;
                }

                /* makes sure that we don't exceed ptl_max_frag_size */
                if (ep->bmi_max_frag_size != 0 && 
                    size > ep->bmi_max_frag_size - sizeof(mca_pml_ob1_frag_hdr_t)) {
                    size = ep->bmi_max_frag_size - sizeof(mca_pml_ob1_frag_hdr_t);
                }
                                                                                                                  
                /* pack into a descriptor */
                des = ep->bmi_prepare_src(
                    ep->bmi,
                    ep->bmi_endpoint,
                    &sendreq->req_send.req_convertor,
                    sizeof(mca_pml_ob1_frag_hdr_t),
                    &size);
                if(des == NULL) {
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.send_pending, (ompi_list_item_t*)sendreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                des->des_cbfunc = mca_pml_ob1_send_completion;
                des->des_cbdata = sendreq;

                /* setup header */
                hdr = (mca_pml_ob1_frag_hdr_t*)des->des_src->seg_addr.pval;
                hdr->hdr_common.hdr_flags = 0;
                hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FRAG;
                hdr->hdr_frag_length = size;
                hdr->hdr_frag_offset = sendreq->req_offset;
                hdr->hdr_src_req.pval = sendreq;
                hdr->hdr_dst_req = sendreq->req_recv;

                /* update state */
                sendreq->req_offset += size;
                OMPI_THREAD_ADD32(&sendreq->req_pending,1);

                /* initiate send - note that this may complete before the call returns */
                rc = ep->bmi_send(ep->bmi, ep->bmi_endpoint, des, MCA_BMI_TAG_PML);
                if(rc == OMPI_SUCCESS) {
                    bytes_remaining = sendreq->req_send.req_bytes_packed - sendreq->req_offset;
                } else {
                    sendreq->req_offset -= size;
                    OMPI_THREAD_ADD32(&sendreq->req_pending,-1);
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.send_pending, (ompi_list_item_t*)sendreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
            }
        } while (OMPI_THREAD_ADD32(&sendreq->req_lock,-1) > 0);
    }
    return OMPI_SUCCESS;
} 


