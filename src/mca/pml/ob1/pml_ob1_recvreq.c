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

static void mca_pml_ob1_recv_request_acked(
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
    mca_pml_ob1_proc_t* proc = mca_pml_ob1_proc_lookup_remote(
        recvreq->req_recv.req_base.req_comm,
        hdr->hdr_match.hdr_src);
    mca_pml_ob1_endpoint_t* ep = mca_pml_ob1_ep_array_get_next(&proc->bmi_first);
    mca_bmi_base_descriptor_t* des;
    mca_pml_ob1_fragment_t* frag;
    mca_pml_ob1_ack_hdr_t* ack;
    int rc;

    /* allocate descriptor */
    des = ep->bmi_alloc(ep->bmi, sizeof(mca_pml_ob1_ack_hdr_t));
    if(NULL == des) {
        goto retry;
    }

    /* fill out header */
    ack = (mca_pml_ob1_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_OB1_HDR_TYPE_ACK;
    ack->hdr_common.hdr_flags = 0;
    ack->hdr_src_req = hdr->hdr_src_req;
    ack->hdr_dst_req.pval = recvreq;

    /* initialize descriptor */
    des->des_cbfunc = mca_pml_ob1_recv_request_acked;
    des->des_cbdata = recvreq;

    rc = ep->bmi_send(ep->bmi, ep->bmi_endpoint, des, MCA_BMI_TAG_PML);
    if(rc != OMPI_SUCCESS) {
        ep->bmi_free(ep->bmi,des);
        goto retry;
    }
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
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:

            bytes_received = hdr->hdr_match.hdr_msg_length;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_match_hdr_t),
                0,
                bytes_received,
                bytes_delivered);
            break;

        case MCA_PML_OB1_HDR_TYPE_RNDV:

            mca_pml_ob1_recv_request_ack(recvreq, &hdr->hdr_rndv);
            bytes_received = hdr->hdr_rndv.hdr_frag_length;
            MCA_PML_OB1_RECV_REQUEST_UNPACK(
                recvreq,
                segments,
                num_segments,
                sizeof(mca_pml_ob1_rendezvous_hdr_t),
                0,
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
                hdr->hdr_frag.frag_offset,
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

