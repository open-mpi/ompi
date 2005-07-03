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

/**
 * @file
 */

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "include/constants.h"
#include "communicator/communicator.h"
#include "mca/pml/pml.h"
#include "pml_ob1.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_hdr.h"



OBJ_CLASS_INSTANCE(
    mca_pml_ob1_buffer_t,
    opal_list_item_t,
    NULL,
    NULL
);

OBJ_CLASS_INSTANCE(
    mca_pml_ob1_recv_frag_t,
    opal_list_item_t,
    NULL,
    NULL
);



/**
 *  Callback from BTL on receive.
 */
                                                                                                                      
void mca_pml_ob1_recv_frag_callback(
    mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_descriptor_t* des,
    void* cbdata)
{
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)segments->seg_addr.pval;
    if(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) {
        return;
    }
                                                                                                                      
    switch(hdr->hdr_common.hdr_type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:
        case MCA_PML_OB1_HDR_TYPE_RNDV:
            {
            mca_pml_ob1_recv_frag_match(btl,&hdr->hdr_match,segments,des->des_dst_cnt);
            break;
            }
        case MCA_PML_OB1_HDR_TYPE_ACK:
            {
            mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)
                hdr->hdr_ack.hdr_src_req.pval;
            sendreq->req_state = MCA_PML_OB1_SR_ACKED;
            sendreq->req_recv = hdr->hdr_ack.hdr_dst_req;
            sendreq->req_rdma_offset = hdr->hdr_ack.hdr_rdma_offset;
#if MCA_PML_OB1_TIMESTAMPS
            sendreq->t_send1 = get_profiler_timestamp();
#endif
            mca_pml_ob1_send_request_schedule(sendreq);
            break;
            }
        case MCA_PML_OB1_HDR_TYPE_FRAG:
            {
            mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)
                hdr->hdr_frag.hdr_dst_req.pval;
            mca_pml_ob1_recv_request_progress(recvreq,btl,segments,des->des_dst_cnt);
            break;
            }
        case MCA_PML_OB1_HDR_TYPE_PUT:
            {
            mca_pml_ob1_send_request_t* sendreq = (mca_pml_ob1_send_request_t*)
                hdr->hdr_rdma.hdr_src.pval;
            mca_pml_ob1_send_request_put(sendreq,btl,&hdr->hdr_rdma);
            break;
            }
        case MCA_PML_OB1_HDR_TYPE_FIN:
            {
            mca_btl_base_descriptor_t* dst = (mca_btl_base_descriptor_t*)
                hdr->hdr_fin.hdr_dst.pval;
            mca_pml_ob1_recv_request_t* recvreq = (mca_pml_ob1_recv_request_t*)dst->des_cbdata;
#if MCA_PML_OB1_TIMESTAMPS
            recvreq->fin1[recvreq->fin_index] = get_profiler_timestamp();
            btl->btl_free(btl,dst);
            recvreq->fin2[recvreq->fin_index] = get_profiler_timestamp();
            recvreq->fin_index++;
            mca_pml_ob1_recv_request_progress(recvreq,btl,segments,des->des_dst_cnt);
#else
            mca_pml_ob1_recv_request_progress(recvreq,btl,segments,des->des_dst_cnt);
            btl->btl_free(btl,dst);
#endif
            break;
            }
        default:
            break;
    }
}
                                                                                                                      

/**
 * Try and match the incoming message fragment to the list of
 * "wild" receives
 *
 * @param hdr Matching data from recived fragment (IN)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */

#define MCA_PML_OB1_CHECK_WILD_RECEIVES_FOR_MATCH(hdr,comm,proc,return_match) \
do { \
    /* local parameters */ \
    opal_list_t* wild_receives = &comm->wild_receives; \
    mca_pml_ob1_recv_request_t *wild_recv; \
    int frag_tag,recv_tag; \
 \
    /* initialization */ \
    frag_tag=hdr->hdr_tag; \
 \
    /* \
     * Loop over the wild irecvs - no need to lock, the upper level \
     * locking is protecting from having other threads trying to \
     * change this list. \
     */ \
    for(wild_recv = (mca_pml_ob1_recv_request_t *)  \
            opal_list_get_first(wild_receives); \
            wild_recv != (mca_pml_ob1_recv_request_t *) \
            opal_list_get_end(wild_receives); \
            wild_recv = (mca_pml_ob1_recv_request_t *)  \
            ((opal_list_item_t *)wild_recv)->opal_list_next) { \
 \
        recv_tag = wild_recv->req_recv.req_base.req_tag; \
        if (  \
                /* exact tag match */ \
                (frag_tag == recv_tag) || \
                /* wild tag match - negative tags (except for \
                 * OMPI_ANY_TAG) are reserved for internal use, and will \
                 * not be matched with OMPI_ANY_TAG */ \
                ( (recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) )  ) \
 \
        { \
            /* \
             * Mark that this is the matching irecv, and go to process it. \
             */ \
            return_match = wild_recv; \
 \
            /* remove this irecv from the postd wild ireceive list */ \
            opal_list_remove_item(wild_receives, \
                    (opal_list_item_t *)wild_recv); \
\
            /* found match - no need to continue */ \
            break; \
        } \
    } \
} while(0)


/**
 * Try and match the incoming message fragment to the list of
 * "specific" receives
 *
 * @param hdr Matching data from recived fragment (IN)
 *
 * @param comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */
#define MCA_PML_OB1_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr,comm,proc,return_match) \
do { \
    /* local variables */ \
    opal_list_t* specific_receives = &proc->specific_receives; \
    mca_pml_ob1_recv_request_t *specific_recv; \
    int recv_tag,frag_tag; \
 \
    /* initialization */ \
    frag_tag=hdr->hdr_tag; \
 \
    /* \
     * Loop over the specific irecvs. \
     */ \
    for(specific_recv = (mca_pml_ob1_recv_request_t *)  \
            opal_list_get_first(specific_receives); \
            specific_recv != (mca_pml_ob1_recv_request_t *) \
            opal_list_get_end(specific_receives); \
            specific_recv = (mca_pml_ob1_recv_request_t *)  \
            ((opal_list_item_t *)specific_recv)->opal_list_next) { \
        /* \
         * Check for a match \
         */ \
        recv_tag = specific_recv->req_recv.req_base.req_tag; \
        if ( (frag_tag == recv_tag) || \
             ( (recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) ) ) { \
 \
            /* \
             * Match made \
             */ \
            return_match = specific_recv; \
 \
            /* remove descriptor from posted specific ireceive list */ \
            opal_list_remove_item(specific_receives, \
                    (opal_list_item_t *)specific_recv); \
 \
            break; \
        } \
    } \
} while(0)

/**
 * Try and match the incoming message fragment to the list of
 * "wild" receives and "specific" receives.  Used when both types
 * of receives have been posted,  i.e. when we need to coordinate
 * between multiple lists to make sure ordered delivery occurs.
 *
 * @param hdr Matching data from recived fragment (IN)
 *
 * @param comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */

#define MCA_PML_OB1_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH( \
    hdr,comm,proc,return_match) \
do {  \
    /* local variables */  \
    mca_pml_ob1_recv_request_t *specific_recv, *wild_recv; \
    mca_ptl_sequence_t wild_recv_seq, specific_recv_seq;  \
    int frag_tag, wild_recv_tag, specific_recv_tag;  \
  \
    /* initialization */  \
    frag_tag=hdr->hdr_tag;  \
  \
    /*  \
     * We know that when this is called, both specific and wild irecvs  \
     *  have been posted.  \
     */  \
    specific_recv = (mca_pml_ob1_recv_request_t *)   \
            opal_list_get_first(&(proc)->specific_receives);  \
    wild_recv = (mca_pml_ob1_recv_request_t *)  \
            opal_list_get_first(&comm->wild_receives);  \
  \
    specific_recv_seq = specific_recv->req_recv.req_base.req_sequence;  \
    wild_recv_seq = wild_recv->req_recv.req_base.req_sequence;  \
  \
    while (true) {  \
        if (wild_recv_seq < specific_recv_seq) {  \
            /*  \
             * wild recv is earlier than the specific one.  \
             */  \
            /*  \
             * try and match  \
             */  \
            wild_recv_tag = wild_recv->req_recv.req_base.req_tag;  \
            if ( (frag_tag == wild_recv_tag) ||  \
                 ( (wild_recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) ) ) {  \
                /*  \
                 * Match made  \
                 */  \
                return_match=wild_recv;  \
  \
                /* remove this recv from the wild receive queue */  \
                opal_list_remove_item(&comm->wild_receives,  \
                        (opal_list_item_t *)wild_recv);  \
                break;  \
            }  \
  \
            /*  \
             * No match, go to the next.  \
             */  \
            wild_recv=(mca_pml_ob1_recv_request_t *)  \
                ((opal_list_item_t *)wild_recv)->opal_list_next;  \
  \
            /*  \
             * If that was the last wild one, just look at the  \
             * rest of the specific ones.  \
             */  \
            if (wild_recv == (mca_pml_ob1_recv_request_t *)  \
                    opal_list_get_end(&comm->wild_receives) )   \
            {   \
                MCA_PML_OB1_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, return_match);  \
                break;  \
            }  \
  \
            /*  \
             * Get the sequence number for this recv, and go  \
             * back to the top of the loop.  \
             */  \
            wild_recv_seq = wild_recv->req_recv.req_base.req_sequence;  \
  \
        } else {  \
            /*  \
             * specific recv is earlier than the wild one.  \
             */  \
            specific_recv_tag=specific_recv->req_recv.req_base.req_tag;  \
            if ( (frag_tag == specific_recv_tag) || \
                 ( (specific_recv_tag == OMPI_ANY_TAG) && (0<=frag_tag)) )   \
            {  \
                /*  \
                 * Match made  \
                 */  \
                return_match = specific_recv;  \
                /* remove descriptor from specific receive list */  \
                opal_list_remove_item(&(proc)->specific_receives,  \
                    (opal_list_item_t *)specific_recv);  \
                break; \
            }  \
  \
            /*  \
             * No match, go on to the next specific irecv.  \
             */  \
            specific_recv = (mca_pml_ob1_recv_request_t *)  \
                ((opal_list_item_t *)specific_recv)->opal_list_next;  \
  \
            /*  \
             * If that was the last specific irecv, process the  \
             * rest of the wild ones.  \
             */  \
            if (specific_recv == (mca_pml_ob1_recv_request_t *)  \
                    opal_list_get_end(&(proc)->specific_receives))  \
            {  \
                MCA_PML_OB1_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, return_match);  \
                break; \
            }  \
            /*  \
             * Get the sequence number for this recv, and go  \
             * back to the top of the loop.  \
             */ \
            specific_recv_seq = specific_recv->req_recv.req_base.req_sequence;  \
        }  \
    }  \
} while(0)


/*
 * Specialized matching routines for internal use only.
 */

static bool mca_pml_ob1_check_cantmatch_for_match(
    opal_list_t *additional_matches,
    mca_pml_ob1_comm_t* comm,
    mca_pml_ob1_comm_proc_t *proc);


/**
 * RCS/CTS receive side matching
 *
 * @param hdr list of parameters needed for matching
 *                    This list is also embeded in frag,
 *                    but this allows to save a memory copy when
 *                    a match is made in this routine. (IN)
 * @param frag   pointer to receive fragment which we want
 *                    to match (IN/OUT).  If a match is not made,
 *                    hdr is copied to frag.
 * @param match_made  parameter indicating if we matched frag/
 *                    hdr (OUT)
 * @param additional_matches  if a match is made with frag, we
 *                    may be able to match fragments that previously
 *                    have arrived out-of-order.  If this is the
 *                    case, the associated fragment descriptors are
 *                    put on this list for further processing. (OUT)
 *
 * @return OMPI error code
 *
 * This routine is used to try and match a newly arrived message fragment
 *   to pre-posted receives.  The following assumptions are made
 *   - fragments are received out of order
 *   - for long messages, e.g. more than one fragment, a RTS/CTS algorithm
 *       is used.
 *   - 2nd and greater fragments include a receive descriptor pointer
 *   - fragments may be dropped
 *   - fragments may be corrupt
 *   - this routine may be called simultaneously by more than one thread
 */
int mca_pml_ob1_recv_frag_match(
    mca_btl_base_module_t* btl,
    mca_pml_ob1_match_hdr_t *hdr,
    mca_btl_base_segment_t* segments,
    size_t num_segments)
{
    /* local variables */
    uint16_t next_msg_seq_expected, frag_msg_seq;
    ompi_communicator_t *comm_ptr;
    mca_pml_ob1_recv_request_t *match = NULL;
    mca_pml_ob1_comm_t *comm;
    mca_pml_ob1_comm_proc_t *proc;
    bool additional_match=false;
    opal_list_t additional_matches;
    int rc;

    /* communicator pointer */
    comm_ptr=ompi_comm_lookup(hdr->hdr_contextid);
    comm=(mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;

    /* source sequence number */
    frag_msg_seq = hdr->hdr_msg_seq;
    proc = comm->procs + hdr->hdr_src;

    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing 
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "loosing"
     * the fragment.
     */
    OPAL_THREAD_LOCK(&comm->matching_lock);

    /* get sequence number of next message that can be processed */
    next_msg_seq_expected = (uint16_t)proc->expected_sequence;
    if (frag_msg_seq == next_msg_seq_expected) {

        /*
         * This is the sequence number we were expecting,
         * so we can try matching it to already posted
         * receives.
         */

        /* We're now expecting the next sequence number. */
        (proc->expected_sequence)++;

        /*
         * figure out what sort of matching logic to use, if need to
         *   look only at "specific" receives, or "wild" receives,
         *   or if we need to traverse both sets at the same time.
         */
        if (opal_list_get_size(&proc->specific_receives) == 0 ){
            /*
             * There are only wild irecvs, so specialize the algorithm.
             */
            MCA_PML_OB1_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
    
        } else if (opal_list_get_size(&comm->wild_receives) == 0 ) {
            /*
             * There are only specific irecvs, so specialize the algorithm.
             */
            MCA_PML_OB1_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
        } else {
            /*
             * There are some of each.
             */
            MCA_PML_OB1_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
        }

        /* if match found, process data */
        if (match) {

            /*
             * update delivered sequence number information, if needed.
             */
            if( (match->req_recv.req_base.req_type == MCA_PML_REQUEST_PROBE) ) {
                /* Match a probe, rollback the next expected sequence number */
                (proc->expected_sequence)--;
            }
        } else {

            /* if no match found, place on unexpected queue */
            mca_pml_ob1_recv_frag_t* frag;
            MCA_PML_OB1_RECV_FRAG_ALLOC(frag, rc);
            if(OMPI_SUCCESS != rc) {
                OPAL_THREAD_UNLOCK(&pml_comm->matching_lock);
                return rc;
            }
            MCA_PML_OB1_RECV_FRAG_INIT(frag,btl,hdr,segments,num_segments);
            opal_list_append( &proc->unexpected_frags, (opal_list_item_t *)frag );
        }

        /* 
         * Now that new message has arrived, check to see if
         *   any fragments on the c_c_frags_cant_match list
         *   may now be used to form new matchs
         */
        if (0 < opal_list_get_size(&proc->frags_cant_match)) {
            additional_match = mca_pml_ob1_check_cantmatch_for_match(&additional_matches,comm,proc);
        }

    } else {

        /*
         * This message comes after the next expected, so it
         * is ahead of sequence.  Save it for later.
         */
        mca_pml_ob1_recv_frag_t* frag;
        MCA_PML_OB1_RECV_FRAG_ALLOC(frag, rc);
        if(OMPI_SUCCESS != rc) {
            OPAL_THREAD_UNLOCK(&pml_comm->matching_lock);
            return rc;
        }
        MCA_PML_OB1_RECV_FRAG_INIT(frag,btl,hdr,segments,num_segments);
        opal_list_append(&proc->frags_cant_match, (opal_list_item_t *)frag);

    }
    OPAL_THREAD_UNLOCK(&pml_comm->matching_lock);


    /* release matching lock before processing fragment */
    if(match != NULL) {
        MCA_PML_OB1_RECV_REQUEST_MATCHED(match, hdr);
        mca_pml_ob1_recv_request_progress(match,btl,segments,num_segments);
    } 
    if(additional_match) {
        opal_list_item_t* item;
        while(NULL != (item = opal_list_remove_first(&additional_matches))) {
            mca_pml_ob1_recv_frag_t* frag = (mca_pml_ob1_recv_frag_t*)item;
            MCA_PML_OB1_RECV_REQUEST_MATCHED(frag->request, hdr);
            mca_pml_ob1_recv_request_progress(frag->request,frag->btl,frag->segments,frag->num_segments);
            MCA_PML_OB1_RECV_FRAG_RETURN(frag);
        }
    }
    return OMPI_SUCCESS;
}


/**
 * Scan the list of frags that came in ahead of time to see if any
 * can be processed at this time.  If they can, try and match the
 * frags.
 *
 * @param additional_matches List to hold new matches with fragments
 * from the c_frags_cant_match list. (IN/OUT)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */

static bool mca_pml_ob1_check_cantmatch_for_match(
    opal_list_t *additional_matches,
    mca_pml_ob1_comm_t* comm,
    mca_pml_ob1_comm_proc_t *proc)
{
    /* local parameters */
    int match_found;
    uint16_t next_msg_seq_expected, frag_seq;
    mca_pml_ob1_recv_frag_t *frag;
    mca_pml_ob1_recv_request_t *match = NULL;
    bool match_made = false;

    /*
     * Loop over all the out of sequence messages.  No ordering is assumed
     * in the c_frags_cant_match list.
     */

    match_found = 1;
    while ((0 < opal_list_get_size(&proc->frags_cant_match)) && match_found) {

        /* initialize match flag for this search */
        match_found = 0;

        /* get sequence number of next message that can be processed */
        next_msg_seq_expected = proc->expected_sequence;

        /* search the list for a fragment from the send with sequence
         * number next_msg_seq_expected
         */
        for(frag = (mca_pml_ob1_recv_frag_t *) 
            opal_list_get_first(&proc->frags_cant_match);
            frag != (mca_pml_ob1_recv_frag_t *)
            opal_list_get_end(&proc->frags_cant_match);
            frag = (mca_pml_ob1_recv_frag_t *) 
            opal_list_get_next(frag))
        {
            /*
             * If the message has the next expected seq from that proc...
             */
            frag_seq=frag->hdr.hdr_match.hdr_msg_seq;
            if (frag_seq == next_msg_seq_expected) {
                mca_pml_ob1_match_hdr_t* hdr = &frag->hdr.hdr_match;

                /* We're now expecting the next sequence number. */
                (proc->expected_sequence)++;

                /* signal that match was made */
                match_found = 1;

                /*
                 * remove frag from list
                 */
                opal_list_remove_item(&proc->frags_cant_match,
                        (opal_list_item_t *)frag);

                /*
                 * figure out what sort of matching logic to use, if need to
                 *   look only at "specific" receives, or "wild" receives,
                 *   or if we need to traverse both sets at the same time.
                 */
                proc = comm->procs + hdr->hdr_src;
                if (opal_list_get_size(&proc->specific_receives) == 0 ) {
                    /*
                     * There are only wild irecvs, so specialize the algorithm.
                     */
                    MCA_PML_OB1_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
                } else if (opal_list_get_size(&comm->wild_receives) == 0 ) {
                    /*
                     * There are only specific irecvs, so specialize the algorithm.
                     */
                    MCA_PML_OB1_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
                } else {
                    /*
                     * There are some of each.
                     */
                    MCA_PML_OB1_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);

                }

                /* if match found, process data */
                if (match) {

                    /* associate the receive descriptor with the fragment
                     * descriptor */
                    frag->request=match;

                    /* add this fragment descriptor to the list of
                     * descriptors to be processed later
                     */
                    if(match_made == false) {
                        match_made = true;
                        OBJ_CONSTRUCT(additional_matches, opal_list_t);
                    }
                    opal_list_append(additional_matches, (opal_list_item_t *)frag);

                } else {
    
                    /* if no match found, place on unexpected queue */
                    opal_list_append( &proc->unexpected_frags, (opal_list_item_t *)frag);

                }

                /* c_frags_cant_match is not an ordered list, so exit loop
                 * and re-start search for next sequence number */
                break;

            } /* end if (frag_seq == next_msg_seq_expected) */
            
        } /* end for (frag) loop */
        
    } /* end while loop */

    return match_made;
}

