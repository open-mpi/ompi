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
#include "mca/errmgr/errmgr.h"
#include "pml_ob1.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_proc.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_rdmafrag.h"
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
 * Completion of a short message - nothing left to schedule.
 */

static void mca_pml_ob1_short_completion(
    mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* ep,
    struct mca_bmi_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_pml_ob1_endpoint_t* bmi_ep = sendreq->req_endpoint;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        ompi_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* attempt to cache the descriptor */
    MCA_PML_OB1_ENDPOINT_DES_RETURN(bmi_ep,descriptor);

    /* signal request completion */
    OMPI_THREAD_LOCK(&ompi_request_lock);
    sendreq->req_bytes_delivered = sendreq->req_send.req_bytes_packed;
    MCA_PML_OB1_SEND_REQUEST_COMPLETE(sendreq);
    OMPI_THREAD_UNLOCK(&ompi_request_lock);
}

/**
 * Completion of a long or synchronous message - may need to schedule
 * additional fragments.
 */

static void mca_pml_ob1_send_completion(
    mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* ep,
    struct mca_bmi_base_descriptor_t* descriptor,
    int status)
{
    mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)descriptor->des_cbdata;
    mca_pml_ob1_endpoint_t* bmi_ep = sendreq->req_endpoint;
    mca_bmi_base_segment_t* segments = descriptor->des_src;
    size_t i;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        ompi_output(0, "%s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }

    /* count bytes of user data actually delivered */
    for(i=0; i<descriptor->des_src_cnt; i++) {
        sendreq->req_bytes_delivered += segments[i].seg_len;
    }

    /* adjust for message header */
    switch(((mca_pml_ob1_common_hdr_t*)segments->seg_addr.pval)->hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:
            sendreq->req_bytes_delivered -= sizeof(mca_pml_ob1_match_hdr_t);
            break;
        case MCA_PML_OB1_HDR_TYPE_RNDV:
            sendreq->req_bytes_delivered -= sizeof(mca_pml_ob1_rendezvous_hdr_t);
            break;
        case MCA_PML_OB1_HDR_TYPE_FRAG:
            sendreq->req_bytes_delivered -= sizeof(mca_pml_ob1_frag_hdr_t);
            break;
        default:
            ompi_output(0, "mca_pml_ob1_send_completion: invalid header type\n");
            break;
    }

    /* check for request completion */
    OMPI_THREAD_LOCK(&ompi_request_lock);
    if (OMPI_THREAD_ADD32(&sendreq->req_pipeline_depth,-1) == 0 &&
        sendreq->req_bytes_delivered == sendreq->req_send.req_bytes_packed) {
        MCA_PML_OB1_SEND_REQUEST_COMPLETE(sendreq);
    } 
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

    /* return the descriptor */
    bmi_ep->bmi_free(bmi_ep->bmi, descriptor);

    /* advance pending requests */
    while(NULL != sendreq) {
        switch(sendreq->req_state) {
            case MCA_PML_OB1_SR_ACKED:
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
 *  BMI requires "specially" allocated memory. Request a segment that
 *  is used for initial hdr and any eager data.
 */

int mca_pml_ob1_send_request_start(
    mca_pml_ob1_send_request_t* sendreq,
    mca_pml_ob1_endpoint_t* endpoint)
{
    mca_bmi_base_descriptor_t* descriptor;
    mca_bmi_base_segment_t* segment;
    mca_pml_ob1_hdr_t* hdr;
    size_t size = sendreq->req_send.req_bytes_packed;
    int rc;

    /* shortcut for zero byte */
    if(size == 0 && sendreq->req_send.req_send_mode != MCA_PML_BASE_SEND_SYNCHRONOUS) {

        /* allocate a descriptor */
        MCA_PML_OB1_ENDPOINT_DES_ALLOC(endpoint, descriptor);
        if(NULL == descriptor) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        } 
        segment = descriptor->des_src;
        segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t);

        /* build hdr */
        hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
        hdr->hdr_common.hdr_flags = 0;
        hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
        hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = 0;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;

        /* short message */
        descriptor->des_cbfunc = mca_pml_ob1_short_completion;

        /* request is complete at mpi level */
        ompi_request_complete((ompi_request_t*)sendreq);
       
    } else {

        struct iovec iov;
        unsigned int iov_count;
        size_t max_data;
        bool ack = false;

        /* determine first fragment size */
        if(size > endpoint->bmi_eager_limit - sizeof(mca_pml_ob1_hdr_t)) {
            size = endpoint->bmi_eager_limit - sizeof(mca_pml_ob1_hdr_t);
            ack = true;
        } else if (sendreq->req_send.req_send_mode == MCA_PML_BASE_SEND_SYNCHRONOUS) {
            ack = true;
        }

        /* if an acknowledgment is not required - can get by w/ shorter hdr */
        if (ack == false) {
            int32_t free_after;

            /* allocate descriptor */
            MCA_PML_OB1_ENDPOINT_DES_ALLOC(endpoint, descriptor);
            if(NULL == descriptor) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            } 
            segment = descriptor->des_src;

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

            /* build match header */
            hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
            hdr->hdr_common.hdr_flags = 0;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_MATCH;
            hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
            hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
            hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
            hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
            hdr->hdr_match.hdr_msg_length = sendreq->req_send.req_bytes_packed;
            hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;

            /* update lengths */
            segment->seg_len = sizeof(mca_pml_ob1_match_hdr_t) + max_data;
            sendreq->req_send_offset = max_data;
            sendreq->req_rdma_offset = max_data;

            /* short message */
            descriptor->des_cbfunc = mca_pml_ob1_short_completion;
           
            /* request is complete at mpi level */
            ompi_request_complete((ompi_request_t*)sendreq);

        /* rendezvous header is required */
        } else {
            int32_t free_after;

            /* allocate space for hdr + first fragment */
            descriptor = endpoint->bmi_alloc(endpoint->bmi, size);
            if(NULL == descriptor) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            segment = descriptor->des_src;

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

            /* build hdr */
            hdr = (mca_pml_ob1_hdr_t*)segment->seg_addr.pval;
            hdr->hdr_common.hdr_flags = 0;
            hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_RNDV;
            hdr->hdr_match.hdr_contextid = sendreq->req_send.req_base.req_comm->c_contextid;
            hdr->hdr_match.hdr_src = sendreq->req_send.req_base.req_comm->c_my_rank;
            hdr->hdr_match.hdr_dst = sendreq->req_send.req_base.req_peer;
            hdr->hdr_match.hdr_tag = sendreq->req_send.req_base.req_tag;
            hdr->hdr_match.hdr_msg_length = sendreq->req_send.req_bytes_packed;
            hdr->hdr_match.hdr_msg_seq = sendreq->req_send.req_base.req_sequence;
            hdr->hdr_rndv.hdr_src_req.lval = 0; /* for VALGRIND/PURIFY - REPLACE WITH MACRO */
            hdr->hdr_rndv.hdr_src_req.pval = sendreq;
            hdr->hdr_rndv.hdr_frag_length = max_data;

            /* update lengths with number of bytes actually packed */
            segment->seg_len = sizeof(mca_pml_ob1_rendezvous_hdr_t) + max_data;
            sendreq->req_send_offset = max_data;

            /* long message */
            descriptor->des_cbfunc = mca_pml_ob1_send_completion;
        }
    }
    descriptor->des_cbdata = sendreq;
    OMPI_THREAD_ADD32(&sendreq->req_pipeline_depth,1);

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
        size_t num_bmi_avail = mca_pml_ob1_ep_array_get_size(&proc->bmi_send);
        do {
            /* allocate remaining bytes to BMIs */
            size_t bytes_remaining = sendreq->req_rdma_offset - sendreq->req_send_offset;
            while(bytes_remaining > 0 && sendreq->req_pipeline_depth < mca_pml_ob1.send_pipeline_depth) {
                mca_pml_ob1_endpoint_t* ep = mca_pml_ob1_ep_array_get_next(&proc->bmi_send);
                mca_pml_ob1_frag_hdr_t* hdr;
                mca_bmi_base_descriptor_t* des;
                int rc;

                /* if there is only one bmi available or the size is less than
                 * than the min fragment size, schedule the rest via this bmi
                 */
                size_t size;
                if(num_bmi_avail == 1 || bytes_remaining < ep->bmi_min_send_size) {
                    size = bytes_remaining;

                /* otherwise attempt to give the BMI a percentage of the message
                 * based on a weighting factor. for simplicity calculate this as
                 * a percentage of the overall message length (regardless of amount
                 * previously assigned)
                 */
                } else {
                    size = (ep->bmi_weight * bytes_remaining) / 100;
                } 

                /* makes sure that we don't exceed BMI max send size */
                if (ep->bmi_max_send_size != 0 && 
                    size > ep->bmi_max_send_size - sizeof(mca_pml_ob1_frag_hdr_t)) {
                    size = ep->bmi_max_send_size - sizeof(mca_pml_ob1_frag_hdr_t);
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
                hdr->hdr_frag_offset = sendreq->req_send_offset;
                hdr->hdr_src_req.pval = sendreq;
                hdr->hdr_dst_req = sendreq->req_recv;

                /* update state */
                sendreq->req_send_offset += size;
                OMPI_THREAD_ADD32(&sendreq->req_pipeline_depth,1);

                /* initiate send - note that this may complete before the call returns */
                rc = ep->bmi_send(ep->bmi, ep->bmi_endpoint, des, MCA_BMI_TAG_PML);
                if(rc == OMPI_SUCCESS) {
                    bytes_remaining -= size;
                } else {
                    sendreq->req_send_offset -= size;
                    OMPI_THREAD_ADD32(&sendreq->req_pipeline_depth,-1);
                    ep->bmi_free(ep->bmi,des);
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


/**
 * Return resources used by the RDMA
 */

static void mca_pml_ob1_fin_completion(
    mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* ep,
    struct mca_bmi_base_descriptor_t* des,
    int status)
{
    
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    MCA_PML_OB1_RDMA_FRAG_RETURN(frag);
    bmi->bmi_free(bmi,des);
}

/**
 *  An RDMA put operation has completed:
 *  (1) Update request status and if required set completed
 *  (2) Send FIN control message to the destination 
 */

static void mca_pml_ob1_put_completion(
    mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* ep,
    struct mca_bmi_base_descriptor_t* des,
    int status)
{
    mca_pml_ob1_rdma_frag_t* frag = (mca_pml_ob1_rdma_frag_t*)des->des_cbdata;
    mca_pml_ob1_send_request_t* sendreq = frag->rdma_req;
    mca_bmi_base_descriptor_t* fin;
    mca_pml_ob1_fin_hdr_t* hdr;
    int rc;

    /* check completion status */
    if(OMPI_SUCCESS != status) {
        /* TSW - FIX */
        ORTE_ERROR_LOG(status);
        orte_errmgr.abort();
    }

    /* check for request completion */
    OMPI_THREAD_LOCK(&ompi_request_lock);
    sendreq->req_bytes_delivered += frag->rdma_length;
    if(sendreq->req_bytes_delivered >= sendreq->req_send.req_bytes_packed) {
        MCA_PML_OB1_SEND_REQUEST_COMPLETE(sendreq);
    }
    OMPI_THREAD_UNLOCK(&ompi_request_lock);

    /* allocate descriptor for fin control message - note that
     * the rdma descriptor cannot be reused as it points directly
     * at the user buffer
     */
    frag->rdma_state = MCA_PML_OB1_RDMA_FIN;
    fin = bmi->bmi_alloc(bmi,sizeof(mca_pml_ob1_fin_hdr_t));
    if(NULL == fin) {
        OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
        ompi_list_append(&mca_pml_ob1.rdma_pending, (ompi_list_item_t*)frag);
        OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
        goto cleanup;
    }
    fin->des_cbfunc = mca_pml_ob1_fin_completion;
    fin->des_cbdata = frag;

    /* fill in header */
    hdr = (mca_pml_ob1_fin_hdr_t*)fin->des_src->seg_addr.pval;
    hdr->hdr_common.hdr_flags = 0;
    hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_FIN;
    hdr->hdr_src = frag->rdma_hdr.hdr_rdma.hdr_src;
    hdr->hdr_dst = frag->rdma_hdr.hdr_rdma.hdr_dst;
    hdr->hdr_rdma_offset = frag->rdma_hdr.hdr_rdma.hdr_rdma_offset;
    hdr->hdr_rdma_length = frag->rdma_length;

    /* queue request */
    rc = bmi->bmi_send(
        bmi,
        ep,
        fin,
        MCA_BMI_TAG_PML);
    if(OMPI_SUCCESS != rc) {
        bmi->bmi_free(bmi, fin);
        if(rc == OMPI_ERR_OUT_OF_RESOURCE) {
            OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
            ompi_list_append(&mca_pml_ob1.rdma_pending, (ompi_list_item_t*)frag);
            OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
    }

cleanup:
    /* return rdma descriptor - do this after queuing the fin message - as 
     * release rdma resources (unpin memory) can take some time.
     */
    des->des_dst = NULL; 
    des->des_dst_cnt = 0; 
    des->des_src = NULL;
    des->des_src_cnt = 0;
    bmi->bmi_free(bmi, des);

}


/**
 *  Receiver has scheduled an RDMA operation:
 *  (1) Allocate an RDMA fragment to maintain the state of the operation
 *  (2) Call BMI prepare_src to pin/prepare source buffers
 *  (3) Queue the RDMA put 
 */

void mca_pml_ob1_send_request_put(
    mca_pml_ob1_send_request_t* sendreq,
    mca_bmi_base_module_t* bmi,
    mca_pml_ob1_rdma_hdr_t* hdr)
{ 
    mca_pml_ob1_proc_t* proc = sendreq->req_proc;
    mca_pml_ob1_endpoint_t* ep = mca_pml_ob1_ep_array_find(&proc->bmi_rdma,bmi);
    mca_bmi_base_descriptor_t* des;
    mca_pml_ob1_rdma_frag_t* frag;
    size_t offset = hdr->hdr_rdma_offset;
    size_t i, size = 0;
    int rc;

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
    frag->rdma_state = MCA_PML_OB1_RDMA_PREPARE;

    /* setup descriptor */
    ompi_convertor_set_position(&sendreq->req_send.req_convertor, &offset);
    des = bmi->bmi_prepare_src(
        bmi, 
        ep->bmi_endpoint,
        &sendreq->req_send.req_convertor, 
        0,
        &size);
    if(NULL == des) { 
        OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
        ompi_list_append(&mca_pml_ob1.rdma_pending, (ompi_list_item_t*)frag);
        OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
    }
    frag->rdma_state = MCA_PML_OB1_RDMA_PUT;
    frag->rdma_length = size;
    frag->rdma_req = sendreq; 

    des->des_dst = frag->rdma_segs;
    des->des_dst_cnt = hdr->hdr_seg_cnt;
    des->des_cbfunc = mca_pml_ob1_put_completion;
    des->des_cbdata = frag;

    /* queue put */
    if(OMPI_SUCCESS != (rc = bmi->bmi_put(bmi, ep->bmi_endpoint, des))) {
        if(rc == OMPI_ERR_OUT_OF_RESOURCE) {
            OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
            ompi_list_append(&mca_pml_ob1.rdma_pending, (ompi_list_item_t*)frag);
            OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
        } else {
            /* TSW - FIX */
            ORTE_ERROR_LOG(rc);
            orte_errmgr.abort();
        }
    }
}


