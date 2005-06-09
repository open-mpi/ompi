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

#include "ompi_config.h"

#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_sendreq.h"

                                                                                                               
static mca_pml_ob1_fragment_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, mca_pml_ob1_comm_proc_t* proc);


static int mca_pml_ob1_recv_request_fini(struct ompi_request_t** request)
{
    MCA_PML_OB1_FINI(request);
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_recv_request_free(struct ompi_request_t** request)
{
    MCA_PML_OB1_FREE(request);
    return OMPI_SUCCESS;
} 

static int mca_pml_ob1_recv_request_cancel(struct ompi_request_t* ompi_request, int complete)
{
    mca_pml_ob1_recv_request_t* request = (mca_pml_ob1_recv_request_t*)ompi_request;
    mca_pml_ob1_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;

    if( true == ompi_request->req_complete ) { /* way to late to cancel this one */
       return OMPI_SUCCESS;
    }
    
    /* The rest should be protected behind the match logic lock */
    OMPI_THREAD_LOCK(&comm->matching_lock);
    if( OMPI_ANY_TAG == ompi_request->req_status.MPI_TAG ) { /* the match has not been already done */
       if( request->req_recv.req_base.req_peer == OMPI_ANY_SOURCE ) {
          ompi_list_remove_item( &comm->wild_receives, (ompi_list_item_t*)request );
       } else {
          mca_pml_ob1_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
          ompi_list_remove_item(&proc->specific_receives, (ompi_list_item_t*)request);
       }
    }
    OMPI_THREAD_UNLOCK(&comm->matching_lock);
    
    OMPI_THREAD_LOCK(&ompi_request_lock);
    ompi_request->req_status._cancelled = true;
    ompi_request->req_complete = true;  /* mark it as completed so all the test/wait  functions
                                    * on this particular request will finish */
    /* Now we have a problem if we are in a multi-threaded environment. We shou ld
     * broadcast the condition on the request in order to allow the other threa ds
     * to complete their test/wait functions.
     */
    if(ompi_request_waiting) {
       ompi_condition_broadcast(&ompi_request_cond);
    }
    OMPI_THREAD_UNLOCK(&ompi_request_lock);
    return OMPI_SUCCESS;
}

static void mca_pml_ob1_recv_request_construct(mca_pml_ob1_recv_request_t* request)
{
    request->req_recv.req_base.req_type = MCA_PML_REQUEST_RECV;
    request->req_recv.req_base.req_ompi.req_fini = mca_pml_ob1_recv_request_fini;
    request->req_recv.req_base.req_ompi.req_free = mca_pml_ob1_recv_request_free;
    request->req_recv.req_base.req_ompi.req_cancel = mca_pml_ob1_recv_request_cancel;
}

static void mca_pml_ob1_recv_request_destruct(mca_pml_ob1_recv_request_t* request)
{
}


OBJ_CLASS_INSTANCE(
    mca_pml_ob1_recv_request_t,
    mca_pml_base_recv_request_t,
    mca_pml_ob1_recv_request_construct,
    mca_pml_ob1_recv_request_destruct);


/*
 * Release resources.
 */

static void mca_pml_ob1_send_ctl_complete(
    mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* ep,
    struct mca_bmi_base_descriptor_t* des,
    int status)
{
    bmi->bmi_free(bmi,des);
}


/*
 *
 */

static void mca_pml_ob1_recv_request_ack(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_pml_ob1_rendezvous_hdr_t* hdr)
{
    mca_pml_ob1_proc_t* proc = recvreq->req_proc;
    mca_pml_ob1_endpoint_t* ep = mca_pml_ob1_ep_array_get_next(&proc->bmi_eager);
    mca_bmi_base_descriptor_t* des;
    mca_pml_ob1_fragment_t* frag;
    mca_pml_ob1_ack_hdr_t* ack;
    bool schedule;
    int rc;

    /* allocate descriptor */
    des = ep->bmi_alloc(ep->bmi, sizeof(mca_pml_ob1_ack_hdr_t));
    if(NULL == des) {
        goto retry;
    }

    /* fill out header */
    ack = (mca_pml_ob1_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_ACK;

    /* use the rdma protocol for this request if:
     * - size is larger than the rdma threshold
     * - rdma devices are available
    */
    if(recvreq->req_recv.req_bytes_packed >= mca_pml_ob1.rdma_threshold &&
       mca_pml_ob1_ep_array_get_size(&proc->bmi_rdma)) {

        /* use convertor to figure out the rdma offset for this request */
        recvreq->req_rdma_offset = mca_pml_ob1.rdma_offset;
        ompi_convertor_set_position(
            &recvreq->req_recv.req_convertor,
            &recvreq->req_rdma_offset);
        ack->hdr_rdma_offset = recvreq->req_rdma_offset;
        schedule = true;
    } else {
        recvreq->req_rdma_offset = recvreq->req_recv.req_bytes_packed;
        ack->hdr_rdma_offset = recvreq->req_recv.req_bytes_packed;
        schedule = false;
    }
    
    ack->hdr_common.hdr_flags = 0;
    ack->hdr_src_req = hdr->hdr_src_req;
    ack->hdr_dst_req.pval = recvreq;

    /* initialize descriptor */
    des->des_cbfunc = mca_pml_ob1_send_ctl_complete;
    des->des_cbdata = recvreq;

    rc = ep->bmi_send(ep->bmi, ep->bmi_endpoint, des, MCA_BMI_TAG_PML);
    if(rc != OMPI_SUCCESS) {
        ep->bmi_free(ep->bmi,des);
        goto retry;
    }

    /* after sending ack - attempt to schedule rdma */
    if(schedule)
        mca_pml_ob1_recv_request_schedule(recvreq);
    return;

    /* queue request to retry later */
retry:
    MCA_PML_OB1_FRAG_ALLOC(frag,rc);
    frag->bmi = NULL;
    frag->hdr.hdr_rndv = *hdr;
    frag->num_segments = 0;
    frag->request = recvreq;
    ompi_list_append(&mca_pml_ob1.acks_pending, (ompi_list_item_t*)frag);
}


/*
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_ob1_recv_request_progress(
    mca_pml_ob1_recv_request_t* recvreq,
    mca_bmi_base_module_t* bmi,
    mca_bmi_base_segment_t* segments,
    size_t num_segments)
{
    size_t bytes_received = 0;
    size_t bytes_delivered = 0;
    size_t data_offset = 0;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:

            bytes_received = hdr->hdr_match.hdr_msg_length;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_match_hdr_t),
                data_offset,
                bytes_received,
                bytes_delivered);
            break;

        case MCA_PML_OB1_HDR_TYPE_RNDV:

            recvreq->req_send = hdr->hdr_rndv.hdr_src_req;
            mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv);
            bytes_received = hdr->hdr_rndv.hdr_frag_length;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_rendezvous_hdr_t),
                data_offset,
                bytes_received,
                bytes_delivered);
            break;

        case MCA_PML_OB1_HDR_TYPE_FRAG:

            bytes_received = hdr->hdr_frag.hdr_frag_length;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_frag_hdr_t),
                hdr->hdr_frag.hdr_frag_offset,
                bytes_received,
                bytes_delivered);
            break;

        default:
            break;
    }

    /* check completion status */
    OMPI_THREAD_LOCK(&ompi_request_lock);
    recvreq->req_bytes_received += bytes_received;
    recvreq->req_bytes_delivered += bytes_delivered;
    if (recvreq->req_bytes_received >= recvreq->req_recv.req_bytes_packed) {
        /* initialize request status */
        recvreq->req_recv.req_base.req_ompi.req_status._count = recvreq->req_bytes_delivered;
        recvreq->req_recv.req_base.req_pml_complete = true; 
        recvreq->req_recv.req_base.req_ompi.req_complete = true;
        if(ompi_request_waiting) {
            ompi_condition_broadcast(&ompi_request_cond);
        }
    }
    OMPI_THREAD_UNLOCK(&ompi_request_lock);
}


/*
 * Schedule RDMA protocol.
 *
*/

void mca_pml_ob1_recv_request_schedule(mca_pml_ob1_recv_request_t* recvreq)
{
    if(OMPI_THREAD_ADD32(&recvreq->req_lock,1) == 1) {
        mca_pml_ob1_proc_t* proc = recvreq->req_proc;
        size_t num_bmi_avail = mca_pml_ob1_ep_array_get_size(&proc->bmi_rdma);
        do {
            size_t bytes_remaining = recvreq->req_recv.req_bytes_packed - recvreq->req_rdma_offset;
            while(bytes_remaining > 0 && recvreq->req_pipeline_depth < mca_pml_ob1.recv_pipeline_depth) {
                mca_pml_ob1_endpoint_t* ep = mca_pml_ob1_ep_array_get_next(&proc->bmi_send); 
                size_t hdr_size;
                mca_pml_ob1_rdma_hdr_t* hdr;
                mca_bmi_base_descriptor_t* dst;
                mca_bmi_base_descriptor_t* ctl;
                int rc;

               /* if there is only one bmi available or the size is less than
                 * than the min fragment size, schedule the rest via this bmi
                 */
                size_t size;
                if(num_bmi_avail == 1 || bytes_remaining < ep->bmi_min_rdma_size) {
                    size = bytes_remaining;

                /* otherwise attempt to give the BMI a percentage of the message
                 * based on a weighting factor. for simplicity calculate this as
                 * a percentage of the overall message length (regardless of amount
                 * previously assigned)
                 */
                } else {
                    size = (ep->bmi_weight * bytes_remaining) / 100;
                }

                /* makes sure that we don't exceed BMI max rdma size */
                if (ep->bmi_max_rdma_size != 0 && size > ep->bmi_max_rdma_size) {
                    size = ep->bmi_max_rdma_size;
                }

                /* prepare a descriptor for RDMA */
                ompi_convertor_set_position(&recvreq->req_recv.req_convertor, &recvreq->req_rdma_offset);
                dst = ep->bmi_prepare_dst(
                    ep->bmi,
                    ep->bmi_endpoint,
                    &recvreq->req_recv.req_convertor,
                    0,
                    &size);
                if(dst == NULL) {
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.recv_pending, (ompi_list_item_t*)recvreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                dst->des_cbdata = recvreq;

                /* prepare a descriptor for rdma control message */
                hdr_size = sizeof(mca_pml_ob1_rdma_hdr_t);
                if(dst->des_dst_cnt > 1) {
                    hdr_size += (sizeof(mca_bmi_base_segment_t) * (dst->des_dst_cnt-1));
                }
                ctl = ep->bmi_alloc(ep->bmi, hdr_size);
                if(ctl == NULL) {
                    ep->bmi_free(ep->bmi,dst);
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.recv_pending, (ompi_list_item_t*)recvreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
                ctl->des_cbfunc = mca_pml_ob1_send_ctl_complete;
                ctl->des_cbdata = recvreq;
                
                /* fill in rdma header */
                hdr = (mca_pml_ob1_rdma_hdr_t*)ctl->des_src->seg_addr.pval;
                hdr->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_PUT;
                hdr->hdr_common.hdr_flags = 0;
                hdr->hdr_src = recvreq->req_send;
                hdr->hdr_dst.pval = dst;
                hdr->hdr_offset = recvreq->req_rdma_offset;
                hdr->hdr_seg_cnt = dst->des_dst_cnt;
                memcpy(hdr->hdr_segs, dst->des_dst, dst->des_dst_cnt * sizeof(mca_bmi_base_segment_t));

                /* update request state */
                recvreq->req_rdma_offset += size;
                OMPI_THREAD_ADD32(&recvreq->req_pipeline_depth,1);

                /* send rdma request to peer */
                rc = ep->bmi_send(ep->bmi, ep->bmi_endpoint, ctl, MCA_BMI_TAG_PML);
                if(rc == OMPI_SUCCESS) {
                    bytes_remaining = recvreq->req_recv.req_bytes_packed - recvreq->req_rdma_offset;
                } else {
                    ep->bmi_free(ep->bmi,ctl);
                    ep->bmi_free(ep->bmi,dst);
                    recvreq->req_rdma_offset -= size;
                    OMPI_THREAD_ADD32(&recvreq->req_pipeline_depth,-1);
                    OMPI_THREAD_LOCK(&mca_pml_ob1.lock);
                    ompi_list_append(&mca_pml_ob1.recv_pending, (ompi_list_item_t*)recvreq);
                    OMPI_THREAD_UNLOCK(&mca_pml_ob1.lock);
                    break;
                }
            } 
        } while(OMPI_THREAD_ADD32(&recvreq->req_lock,-1) > 0);
    }
}

/*
 * This routine is used to match a posted receive when the source process 
 * is specified.
*/

void mca_pml_ob1_recv_request_match_specific(mca_pml_ob1_recv_request_t* request)
{
    mca_pml_ob1_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_ob1_comm_proc_t* proc = comm->procs + request->req_recv.req_base.req_peer;
    mca_pml_ob1_fragment_t* frag;
   
    /* check for a specific match */
    OMPI_THREAD_LOCK(&comm->matching_lock);

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    if (ompi_list_get_size(&proc->unexpected_frags) > 0 &&
        (frag = mca_pml_ob1_recv_request_match_specific_proc(request, proc)) != NULL) {
        OMPI_THREAD_UNLOCK(&comm->matching_lock);

        mca_pml_ob1_recv_request_progress(request,frag->bmi,frag->segments,frag->num_segments);
        if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
              (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
            MCA_PML_OB1_FRAG_RETURN(frag);
        }
        return; /* match found */
    }

    /* We didn't find any matches.  Record this irecv so we can match 
     * it when the message comes in.
    */
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE) { 
        ompi_list_append(&proc->specific_receives, (ompi_list_item_t*)request);
    }
    OMPI_THREAD_UNLOCK(&comm->matching_lock);
}


/*
 * this routine is used to try and match a wild posted receive - where
 * wild is determined by the value assigned to the source process
*/

void mca_pml_ob1_recv_request_match_wild(mca_pml_ob1_recv_request_t* request)
{
    mca_pml_ob1_comm_t* comm = request->req_recv.req_base.req_comm->c_pml_comm;
    mca_pml_ob1_comm_proc_t* proc = comm->procs;
    size_t proc_count = comm->num_procs;
    size_t i;

    /*
     * Loop over all the outstanding messages to find one that matches.
     * There is an outer loop over lists of messages from each
     * process, then an inner loop over the messages from the
     * process.
    */
    OMPI_THREAD_LOCK(&pml_comm->c_matching_lock);

    /* assign sequence number */
    request->req_recv.req_base.req_sequence = comm->recv_sequence++;

    for (i = 0; i < proc_count; i++) {
        mca_pml_ob1_fragment_t* frag;

        /* continue if no frags to match */
        if (ompi_list_get_size(&proc->unexpected_frags) == 0) {
            proc++;
            continue;
        }

        /* loop over messages from the current proc */
        if ((frag = mca_pml_ob1_recv_request_match_specific_proc(request, proc)) != NULL) {
            OMPI_THREAD_UNLOCK(&comm->matching_lock);

            mca_pml_ob1_recv_request_progress(request,frag->bmi,frag->segments,frag->num_segments);
            if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
                  (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
                MCA_PML_OB1_FRAG_RETURN(frag);
            }
            return; /* match found */
        }
        proc++;
    } 

    /* We didn't find any matches.  Record this irecv so we can match to
     * it when the message comes in.
    */
 
    if(request->req_recv.req_base.req_type != MCA_PML_REQUEST_IPROBE)
        ompi_list_append(&comm->wild_receives, (ompi_list_item_t*)request);
    OMPI_THREAD_UNLOCK(&comm->matching_lock);
}


/*
 *  this routine tries to match a posted receive.  If a match is found,
 *  it places the request in the appropriate matched receive list. 
*/

static mca_pml_ob1_fragment_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, 
    mca_pml_ob1_comm_proc_t* proc)
{
    ompi_list_t* unexpected_frags = &proc->unexpected_frags;
    mca_pml_ob1_fragment_t* frag;
    mca_pml_ob1_match_hdr_t* hdr;
    int tag = request->req_recv.req_base.req_tag;

    if( OMPI_ANY_TAG == tag ) {
        for (frag =  (mca_pml_ob1_fragment_t*)ompi_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_fragment_t*)ompi_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_fragment_t*)ompi_list_get_next(frag)) {
            hdr = &(frag->hdr.hdr_match);
            
            /* check first frag - we assume that process matching has been done already */
            if( hdr->hdr_tag >= 0 ) {
                goto find_fragment;
            } 
        }
    } else {
        for (frag =  (mca_pml_ob1_fragment_t*)ompi_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_fragment_t*)ompi_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_fragment_t*)ompi_list_get_next(frag)) {
            hdr = &(frag->hdr.hdr_match);
            
            /* check first frag - we assume that process matching has been done already */
            if ( tag == hdr->hdr_tag ) {
                /* we assume that the tag is correct from MPI point of view (ie. >= 0 ) */
                goto find_fragment;
            } 
        }
    }
    return NULL;
 find_fragment:
    MCA_PML_OB1_RECV_REQUEST_MATCHED(request, hdr);
    if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
          (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
        ompi_list_remove_item(unexpected_frags, (ompi_list_item_t*)frag);
        frag->request = request;
    } 
    return frag;
}

