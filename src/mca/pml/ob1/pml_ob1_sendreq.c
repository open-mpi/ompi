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
    mca_pml_ob1_endpoint_t* ob1_ep = sendreq->req_endpoint;

    OMPI_THREAD_LOCK(&ompi_request_lock);
    if (sendreq->req_offset == sendreq->req_send.req_bytes_packed) {
        sendreq->req_send.req_base.req_pml_complete = true;
        if (sendreq->req_send.req_base.req_ompi.req_complete == false) {
            sendreq->req_send.req_base.req_ompi.req_status.MPI_SOURCE = sendreq->req_send.req_base.req_comm->c_my_rank;
            sendreq->req_send.req_base.req_ompi.req_status.MPI_TAG = sendreq->req_send.req_base.req_tag;
            sendreq->req_send.req_base.req_ompi.req_status.MPI_ERROR = OMPI_SUCCESS;
            sendreq->req_send.req_base.req_ompi.req_status._count = sendreq->req_send.req_bytes_packed;
            sendreq->req_send.req_base.req_ompi.req_complete = true;
            if(ompi_request_waiting) {
                ompi_condition_broadcast(&ompi_request_cond);
            }
        } else if(sendreq->req_send.req_base.req_free_called) {
            MCA_PML_OB1_FREE((ompi_request_t**)&sendreq);
        } else if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_BUFFERED) {
            mca_pml_base_bsend_request_fini((ompi_request_t*)sendreq);
        }
    } 
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

    /* check for pending requests that need to be progressed */
    while(ompi_list_get_size(&mca_pml_ob1.send_pending) != 0) {
        OMPI_THREAD_LOCK(&mca_pml_ob1.ob1_lock);
        sendreq = (mca_pml_ob1_send_request_t*)ompi_list_remove_first(&mca_pml_ob1.send_pending);
        OMPI_THREAD_UNLOCK(&mca_pml_ob1.ob1_lock);
    }

    /* release NTL resources */
    if(ob1_ep->bmi_cache == NULL) {
       ob1_ep->bmi_cache = descriptor;
    } else {
       ob1_ep->bmi_free(bmi,descriptor);
    }
}


/**
 *  NTL can send directly from user allocated memory.
 */

int mca_pml_ob1_send_user(
    mca_pml_ob1_send_request_t* sendreq,
    mca_pml_ob1_endpoint_t* endpoint)
{
    return OMPI_ERROR;
}


/**
 *  NTL requires "specially" allocated memory. Request a segment that
 *  is used for initial hdr and any eager data.
 */

int mca_pml_ob1_send_copy(
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

        descriptor = endpoint->bmi_cache;
        if(NULL != descriptor) {
            endpoint->bmi_cache = NULL;
        } else {
            descriptor = endpoint->bmi_alloc(endpoint->bmi, sizeof(mca_pml_ob1_hdr_t));
            if(NULL == descriptor) {
                OBJ_RELEASE(sendreq);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            descriptor->des_cbfunc = mca_pml_ob1_send_completion;
        }
        segment = descriptor->des_src;

        /* build hdr */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;

        /* if an acknowledgment is not required - can get by w/ shorter hdr */
        if (sendreq->req_send.req_send_mode != MCA_PML_BASE_SEND_SYNCHRONOUS) {
            hdr->hdr_common.hdr_flags = 0;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
            segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t);
        } else {
            hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_ACK;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
            hdr->hdr_rndv.hdr_frag_length = 0;
            hdr->hdr_rndv.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
            hdr->hdr_rndv.hdr_src_ptr.pval = sendreq;
            segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t);
        }
        ompi_request_complete((ompi_request_t*)sendreq);
       
    } else {

        struct iovec iov;
        unsigned int iov_count;
        unsigned int max_data;

        /* determine first fragment size */
        if(size > endpoint->bmi_eager_limit - sizeof(mca_pml_ob1_hdr_t)) {
            size = endpoint->bmi_eager_limit - sizeof(mca_pml_ob1_hdr_t);
        } 

        /* allocate space for hdr + first fragment */
        descriptor = endpoint->bmi_alloc(endpoint->bmi, size + sizeof(mca_pml_ob1_hdr_t));
        if(NULL == descriptor) {
            OBJ_RELEASE(sendreq);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        descriptor->des_cbfunc = mca_pml_ob1_send_completion;
        segment = descriptor->des_src;

        /* build hdr */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_send.req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;

        /* if an acknowledgment is not required - can get by w/ shorter hdr */
        if (sendreq->req_send.req_send_mode != MCA_PML_BASE_SEND_SYNCHRONOUS) {
            hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_ACK;
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
                NULL)) < 0) {
                endpoint->bmi_free(endpoint->bmi, descriptor);
                OBJ_RELEASE(sendreq);
                return rc;
            }
           
            /* update length w/ number of bytes actually packed */
            segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t) + max_data;

        /* rendezvous header is required */
        } else {
            hdr->hdr_common.hdr_flags = MCA_PML_OB1_HDR_FLAGS_ACK;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
            hdr->hdr_rndv.hdr_src_ptr.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
            hdr->hdr_rndv.hdr_src_ptr.pval = sendreq;

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
                NULL)) < 0) {
                endpoint->bmi_free(endpoint->bmi, descriptor);
                OBJ_RELEASE(sendreq);
                return rc;
            }

            hdr->hdr_rndv.hdr_frag_length = max_data;
            segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t) + max_data;
        }
        sendreq->req_offset = max_data;
        if(sendreq->req_offset == sendreq->req_send.req_bytes_packed) {
            ompi_request_complete((ompi_request_t*)sendreq);
        }
    }
    descriptor->des_cbdata = sendreq;

    /* send */
    rc = endpoint->bmi_send(
        endpoint->bmi, 
        endpoint->bmi_endpoint, 
        descriptor,
        MCA_BMI_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        endpoint->bmi_free(endpoint->bmi,descriptor);
        OBJ_RELEASE(sendreq);
    }
    return rc;
}


