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

                                                                                                               
static mca_pml_ob1_recv_frag_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, mca_pml_ob1_comm_proc_t* proc);


static int mca_pml_ob1_recv_request_fini(struct ompi_request_t** request)
{
    MCA_PML_GEN2_FINI(request);
    return OMPI_SUCCESS;
}

static int mca_pml_ob1_recv_request_free(struct ompi_request_t** request)
{
    MCA_PML_GEN2_FREE(request);
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
 * Update the recv request status to reflect the number of bytes
 * received and actually delivered to the application. 
 */

void mca_pml_ob1_recv_request_progress(
    mca_pml_ob1_recv_request_t* req,
    mca_bmi_base_module_t* bmi,
    mca_bmi_base_segment_t* segments,
    size_t num_segments)
{
    size_t bytes_received = 0;
    size_t bytes_delivered = 0;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_GEN2_HDR_TYPE_MATCH:
            bytes_received = hdr->hdr_match.hdr_msg_length;
            break;
        case MCA_PML_GEN2_HDR_TYPE_RNDV:
            bytes_received = hdr->hdr_frag.hdr_frag_length;
            break;
        default:
            break;
    }
    bytes_delivered = bytes_received;

    OMPI_THREAD_LOCK(&ompi_request_lock);
    req->req_bytes_received += bytes_received;
    req->req_bytes_delivered += bytes_delivered;
    if (req->req_bytes_received >= req->req_recv.req_bytes_packed) {
        /* initialize request status */
        req->req_recv.req_base.req_ompi.req_status._count = req->req_bytes_delivered;
        req->req_recv.req_base.req_pml_complete = true; 
        req->req_recv.req_base.req_ompi.req_complete = true;
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
    mca_pml_ob1_recv_frag_t* frag;
   
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
            MCA_PML_GEN2_RECV_FRAG_RETURN(frag);
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
        mca_pml_ob1_recv_frag_t* frag;

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
                MCA_PML_GEN2_RECV_FRAG_RETURN(frag);
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

static mca_pml_ob1_recv_frag_t* mca_pml_ob1_recv_request_match_specific_proc(
    mca_pml_ob1_recv_request_t* request, 
    mca_pml_ob1_comm_proc_t* proc)
{
    ompi_list_t* unexpected_frags = &proc->unexpected_frags;
    mca_pml_ob1_recv_frag_t* frag;
    mca_pml_ob1_match_hdr_t* hdr;
    int tag = request->req_recv.req_base.req_tag;

    if( OMPI_ANY_TAG == tag ) {
        for (frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_recv_frag_t*)ompi_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_next(frag)) {
            hdr = &(frag->hdr.hdr_match);
            
            /* check first frag - we assume that process matching has been done already */
            if( hdr->hdr_tag >= 0 ) {
                goto find_fragment;
            } 
        }
    } else {
        for (frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_first(unexpected_frags);
             frag != (mca_pml_ob1_recv_frag_t*)ompi_list_get_end(unexpected_frags);
             frag =  (mca_pml_ob1_recv_frag_t*)ompi_list_get_next(frag)) {
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
    request->req_recv.req_bytes_packed = hdr->hdr_msg_length;
    request->req_recv.req_base.req_ompi.req_status.MPI_TAG = hdr->hdr_tag;
    request->req_recv.req_base.req_ompi.req_status.MPI_SOURCE = hdr->hdr_src;

    if( !((MCA_PML_REQUEST_IPROBE == request->req_recv.req_base.req_type) ||
          (MCA_PML_REQUEST_PROBE == request->req_recv.req_base.req_type)) ) {
        ompi_list_remove_item(unexpected_frags, (ompi_list_item_t*)frag);
        frag->request = request;
    } 
    return frag;
}

