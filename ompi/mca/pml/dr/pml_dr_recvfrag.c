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

/**
 * @file
 */

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "opal/threads/mutex.h"
#include "opal/util/crc.h"
#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/datatype.h"
#include "ompi/mca/pml/pml.h"
#include "pml_dr.h"
#include "pml_dr_comm.h"
#include "pml_dr_recvfrag.h"
#include "pml_dr_recvreq.h"
#include "pml_dr_sendreq.h"
#include "pml_dr_hdr.h"


#define MCA_PML_DR_HDR_VALIDATE(hdr, type)                                                     \
do {                                                                                           \
    uint16_t csum = opal_csum(hdr, sizeof(type));                                              \
    if(hdr->hdr_common.hdr_csum != csum) {                                                     \
        opal_output(0, "%s:%d: invalid header checksum: 0x%04x != 0x%04x\n",                   \
            __FILE__, __LINE__, hdr->hdr_common.hdr_csum, csum);                               \
        return;                                                                                \
    }                                                                                          \
    if(hdr->hdr_common.hdr_dst != (ompi_comm_lookup(hdr->hdr_common.hdr_ctx))->c_my_rank ) {   \
        opal_output(0, "%s:%d: misdelivered packet [rank %d -> rank %d]\n",                    \
            __FILE__, __LINE__, hdr->hdr_common.hdr_src, hdr->hdr_common.hdr_dst);             \
        return;                                                                                \
    }                                                                                          \
} while (0)


OBJ_CLASS_INSTANCE(
    mca_pml_dr_buffer_t,
    opal_list_item_t,
    NULL,
    NULL
);

OBJ_CLASS_INSTANCE(
    mca_pml_dr_recv_frag_t,
    opal_list_item_t,
    NULL,
    NULL
);

/*
 * Release resources.
 */

static void mca_pml_dr_ctl_completion(
    mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* ep,
    struct mca_btl_base_descriptor_t* des,
    int status)
{
    mca_bml_base_btl_t* bml_btl = (mca_bml_base_btl_t*)des->des_context;
    MCA_BML_BASE_BTL_DES_RETURN(bml_btl, des);
}


/**
 *  Callback from BTL on receive.
 */

void mca_pml_dr_recv_frag_callback(
                                    mca_btl_base_module_t* btl, 
                                    mca_btl_base_tag_t tag,
                                    mca_btl_base_descriptor_t* des,
                                    void* cbdata)
{
    mca_btl_base_segment_t* segments = des->des_dst;
    mca_pml_dr_hdr_t* hdr = (mca_pml_dr_hdr_t*)segments->seg_addr.pval;
    bool duplicate = false;
    if(segments->seg_len < sizeof(mca_pml_dr_common_hdr_t)) {
        return;
    }

    switch(hdr->hdr_common.hdr_type) {
    case MCA_PML_DR_HDR_TYPE_MATCH:
        {
            MCA_PML_DR_HDR_VALIDATE(hdr, mca_pml_dr_match_hdr_t);
            MCA_PML_DR_RECV_FRAG_CHECK_DUP(hdr, duplicate);
            if(false == duplicate) { 
                mca_pml_dr_recv_frag_match(btl, &hdr->hdr_match, segments,des->des_dst_cnt);
            } else {
                OPAL_OUTPUT((0, "%s:%d: dropping duplicate fragment\n", __FILE__, __LINE__));
            }
            break;
        }
    case MCA_PML_DR_HDR_TYPE_MATCH_ACK:
        {
            MCA_PML_DR_HDR_VALIDATE(hdr, mca_pml_dr_ack_hdr_t);
            mca_pml_dr_send_request_match_ack(btl, &hdr->hdr_ack);
            break;
        }
    case MCA_PML_DR_HDR_TYPE_RNDV:
        {
            MCA_PML_DR_HDR_VALIDATE(hdr, mca_pml_dr_rendezvous_hdr_t);
            MCA_PML_DR_RECV_FRAG_CHECK_DUP(hdr, duplicate);
            if(false == duplicate) {
                mca_pml_dr_recv_frag_match(btl, &hdr->hdr_match, segments,des->des_dst_cnt);
            } else {
                OPAL_OUTPUT((0, "%s:%d: dropping duplicate fragment\n", __FILE__, __LINE__));
            }
            break;
        }
    case MCA_PML_DR_HDR_TYPE_RNDV_ACK:
        {
            MCA_PML_DR_HDR_VALIDATE(hdr, mca_pml_dr_ack_hdr_t);
            mca_pml_dr_send_request_rndv_ack(btl, &hdr->hdr_ack);
            break;
        }
    case MCA_PML_DR_HDR_TYPE_FRAG:
        {
            mca_pml_dr_recv_request_t* recvreq;
            mca_pml_dr_comm_proc_t* comm_proc; 
            
            MCA_PML_DR_HDR_VALIDATE(hdr, mca_pml_dr_frag_hdr_t);
            recvreq = hdr->hdr_frag.hdr_dst_ptr.pval;
            comm_proc =  recvreq->req_recv.req_base.req_comm->c_pml_comm->procs +
                recvreq->req_recv.req_base.req_ompi.req_status.MPI_SOURCE;
            
            if(mca_pml_dr_comm_proc_check_acked(comm_proc,
                                                hdr->hdr_common.hdr_vid)) {
                mca_pml_dr_recv_frag_send_ack(comm_proc->ompi_proc,
                                              &hdr->hdr_common,
                                              hdr->hdr_frag.hdr_src_ptr,
                                              ~(uint64_t) 0);
            } else {
                mca_pml_dr_recv_request_progress(recvreq,btl,segments,des->des_dst_cnt);
            }
            break;

        }
    case MCA_PML_DR_HDR_TYPE_FRAG_ACK:
        {
            MCA_PML_DR_HDR_VALIDATE(hdr, mca_pml_dr_ack_hdr_t);
            mca_pml_dr_send_request_frag_ack(btl, &hdr->hdr_ack);
            break;
        }
    default:
        OPAL_OUTPUT((0, "%s:%d: dropping unknown header type\n"));
        return; /* drop it on the floor.. */
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

#define MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr,comm,proc,return_match) \
do { \
    /* local parameters */ \
    opal_list_t* wild_receives = &comm->wild_receives; \
    mca_pml_dr_recv_request_t *wild_recv; \
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
    for(wild_recv = (mca_pml_dr_recv_request_t *)  \
            opal_list_get_first(wild_receives); \
            wild_recv != (mca_pml_dr_recv_request_t *) \
            opal_list_get_end(wild_receives); \
            wild_recv = (mca_pml_dr_recv_request_t *)  \
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
#define MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr,comm,proc,return_match) \
do { \
    /* local variables */ \
    opal_list_t* specific_receives = &proc->specific_receives; \
    mca_pml_dr_recv_request_t *specific_recv; \
    int recv_tag,frag_tag; \
 \
    /* initialization */ \
    frag_tag=hdr->hdr_tag; \
 \
    /* \
     * Loop over the specific irecvs. \
     */ \
    for(specific_recv = (mca_pml_dr_recv_request_t *)  \
            opal_list_get_first(specific_receives); \
            specific_recv != (mca_pml_dr_recv_request_t *) \
            opal_list_get_end(specific_receives); \
            specific_recv = (mca_pml_dr_recv_request_t *)  \
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

#define MCA_PML_DR_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH( \
    hdr,comm,proc,return_match) \
do {  \
    /* local variables */  \
    mca_pml_dr_recv_request_t *specific_recv, *wild_recv; \
    mca_pml_sequence_t wild_recv_seq, specific_recv_seq;  \
    int frag_tag, wild_recv_tag, specific_recv_tag;  \
  \
    /* initialization */  \
    frag_tag=hdr->hdr_tag;  \
  \
    /*  \
     * We know that when this is called, both specific and wild irecvs  \
     *  have been posted.  \
     */  \
    specific_recv = (mca_pml_dr_recv_request_t *)   \
            opal_list_get_first(&(proc)->specific_receives);  \
    wild_recv = (mca_pml_dr_recv_request_t *)  \
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
            wild_recv=(mca_pml_dr_recv_request_t *)  \
                ((opal_list_item_t *)wild_recv)->opal_list_next;  \
  \
            /*  \
             * If that was the last wild one, just look at the  \
             * rest of the specific ones.  \
             */  \
            if (wild_recv == (mca_pml_dr_recv_request_t *)  \
                    opal_list_get_end(&comm->wild_receives) )   \
            {   \
                MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, return_match);  \
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
            specific_recv = (mca_pml_dr_recv_request_t *)  \
                ((opal_list_item_t *)specific_recv)->opal_list_next;  \
  \
            /*  \
             * If that was the last specific irecv, process the  \
             * rest of the wild ones.  \
             */  \
            if (specific_recv == (mca_pml_dr_recv_request_t *)  \
                    opal_list_get_end(&(proc)->specific_receives))  \
            {  \
                MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, return_match);  \
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

static bool mca_pml_dr_check_cantmatch_for_match(
    opal_list_t *additional_matches,
    mca_pml_dr_comm_t* comm,
    mca_pml_dr_comm_proc_t *proc);


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
bool mca_pml_dr_recv_frag_match(
                                mca_btl_base_module_t *btl, 
                                mca_pml_dr_match_hdr_t *hdr,
                                mca_btl_base_segment_t* segments,
                                size_t num_segments)
{
    /* local variables */
    uint16_t next_msg_seq_expected, frag_msg_seq;
    ompi_communicator_t *comm_ptr;
    mca_pml_dr_recv_request_t *match = NULL;
    mca_pml_dr_comm_t *comm;
    mca_pml_dr_comm_proc_t *proc;
    bool additional_match=false;
    opal_list_t additional_matches;
    ompi_proc_t* ompi_proc;
    int rc;
    uint32_t csum = OPAL_CSUM_ZERO;

    /* communicator pointer */
    comm_ptr=ompi_comm_lookup(hdr->hdr_common.hdr_ctx);
    comm=(mca_pml_dr_comm_t *)comm_ptr->c_pml_comm;

    /* source sequence number */
    frag_msg_seq = hdr->hdr_seq;
    proc = comm->procs + hdr->hdr_common.hdr_src;
    ompi_proc = proc->ompi_proc;

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
rematch:

        /*
         * figure out what sort of matching logic to use, if need to
         *   look only at "specific" receives, or "wild" receives,
         *   or if we need to traverse both sets at the same time.
         */
        if (opal_list_get_size(&proc->specific_receives) == 0 ){
            /*
             * There are only wild irecvs, so specialize the algorithm.
             */
            MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
    
        } else if (opal_list_get_size(&comm->wild_receives) == 0 ) {
            /*
             * There are only specific irecvs, so specialize the algorithm.
             */
            MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
        } else {
            /*
             * There are some of each.
             */
            MCA_PML_DR_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
        }

        /* if match found, process data */
        if (match) {

            /*
             * update delivered sequence number information, if needed.
             */
            if( (match->req_recv.req_base.req_type == MCA_PML_REQUEST_PROBE) ) {

                /* complete the probe */
                mca_pml_dr_recv_request_matched_probe(match,btl,segments,num_segments);

                /* retry the matchh */
                match = NULL;
                goto rematch;
            }
        } else {

            /* if no match found, verify csum, if pass place on unexpected queue */
            mca_pml_dr_recv_frag_t* frag;
            MCA_PML_DR_RECV_FRAG_ALLOC(frag, rc);
            if(OMPI_SUCCESS != rc) {
                OPAL_THREAD_UNLOCK(&comm->matching_lock);
                return rc;
            }
            MCA_PML_DR_RECV_FRAG_INIT(frag,proc->ompi_proc,hdr,segments,num_segments,btl,csum);
            if(csum != hdr->hdr_csum) { 
                mca_pml_dr_recv_frag_send_ack(ompi_proc, 
                                              &hdr->hdr_common,
                                              hdr->hdr_src_ptr,
                                              0);
                opal_output(0, "%s:%d: corrupted data 0x%08x != 0x%08x\n", 
                    __FILE__, __LINE__, csum, hdr->hdr_csum);
                MCA_PML_DR_RECV_FRAG_RETURN(frag);
                OPAL_THREAD_UNLOCK(&comm->matching_lock);
                return false;
            }
            opal_list_append( &proc->unexpected_frags, (opal_list_item_t *)frag );
        }

        /* 
         * Now that new message has arrived, check to see if
         *   any fragments on the c_c_frags_cant_match list
         *   may now be used to form new matchs
         */
        if (0 < opal_list_get_size(&proc->frags_cant_match)) {
            additional_match = mca_pml_dr_check_cantmatch_for_match(&additional_matches,comm,proc);
        }
        
    } else {

        /*
         * This message comes after the next expected, so it
         * is ahead of sequence.  If passes csum save it for later.
         */

        mca_pml_dr_recv_frag_t* frag;
        MCA_PML_DR_RECV_FRAG_ALLOC(frag, rc);
        if(OMPI_SUCCESS != rc) {
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            return rc;
        }
        MCA_PML_DR_RECV_FRAG_INIT(frag,proc->ompi_proc,hdr,segments,num_segments,btl,csum);
        if(csum != hdr->hdr_csum) { 
            mca_pml_dr_recv_frag_send_ack(ompi_proc, 
                                          &hdr->hdr_common,
                                          hdr->hdr_src_ptr,
                                          0);
            opal_output(0, "%s:%d: corrupted data 0x%08x != 0x%08x\n", 
                        __FILE__, __LINE__, csum, hdr->hdr_csum);
            MCA_PML_DR_RECV_FRAG_RETURN(frag);
            OPAL_THREAD_UNLOCK(&comm->matching_lock);
            return false;
        }
        opal_list_append(&proc->frags_cant_match, (opal_list_item_t *)frag);
    }
    OPAL_THREAD_UNLOCK(&comm->matching_lock);


    /* release matching lock before processing fragment */
    if(match != NULL) {
        mca_pml_dr_recv_request_progress(match,btl,segments,num_segments);
    } 
    else { 
        /* no need to csum, if it wasn't matched and 
           csum failed, we already nack'd it */
        mca_pml_dr_recv_frag_send_ack(ompi_proc, 
                                      &hdr->hdr_common, 
                                      hdr->hdr_src_ptr,
                                      1);
    }
    if(additional_match) {
        opal_list_item_t* item;
        while(NULL != (item = opal_list_remove_first(&additional_matches))) {
            mca_pml_dr_recv_frag_t* frag = (mca_pml_dr_recv_frag_t*)item;
            mca_pml_dr_recv_request_progress(frag->request,frag->btl,frag->segments,frag->num_segments);
            MCA_PML_DR_RECV_FRAG_RETURN(frag);
        }
    }
    return (match != NULL);
}



void mca_pml_dr_recv_frag_send_ack(
                                   ompi_proc_t* ompi_proc, 
                                   mca_pml_dr_common_hdr_t* hdr,
                                   ompi_ptr_t src_ptr,
                                   uint64_t mask)
    {
    mca_bml_base_endpoint_t* bml_endpoint = NULL; 
    mca_btl_base_descriptor_t* des;
    mca_bml_base_btl_t* bml_btl;
    mca_pml_dr_recv_frag_t* frag;
    mca_pml_dr_ack_hdr_t* ack;
    int rc;
    
    bml_endpoint = (mca_bml_base_endpoint_t*) ompi_proc->proc_pml; 
    bml_btl = mca_bml_base_btl_array_get_next(&bml_endpoint->btl_eager);
    
    /* allocate descriptor */
    MCA_PML_DR_DES_ALLOC(bml_btl, des, sizeof(mca_pml_dr_ack_hdr_t));
    if(NULL == des) {
        goto retry;
    }

    /* fill out header */
    ack = (mca_pml_dr_ack_hdr_t*)des->des_src->seg_addr.pval;
    ack->hdr_common.hdr_type = MCA_PML_DR_HDR_TYPE_ACK | hdr->hdr_type;
    ack->hdr_common.hdr_flags = 0;
    ack->hdr_common.hdr_dst = hdr->hdr_src;
    ack->hdr_common.hdr_vid = hdr->hdr_vid;
    ack->hdr_common.hdr_ctx = hdr->hdr_ctx;
    ack->hdr_vmask = mask;
    ack->hdr_src_ptr = src_ptr;
    assert(ack->hdr_src_ptr.pval);
    ack->hdr_dst_ptr.pval = NULL;
    ack->hdr_common.hdr_csum = opal_csum(ack, sizeof(mca_pml_dr_ack_hdr_t));
    
    /* initialize descriptor */
    des->des_flags |= MCA_BTL_DES_FLAGS_PRIORITY;
    des->des_cbfunc = mca_pml_dr_ctl_completion;

    rc = mca_bml_base_send(bml_btl, des, MCA_BTL_TAG_PML);
    if(rc != OMPI_SUCCESS) {
        mca_bml_base_free(bml_btl, des);
        goto retry;
    }
    return;

    /* queue request to retry later */
retry:
    MCA_PML_DR_RECV_FRAG_ALLOC(frag,rc);
    /* frag->hdr.hdr_match = *hdr; */
    frag->num_segments = 0;
    opal_list_append(&mca_pml_dr.acks_pending, (opal_list_item_t*)frag);
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

static bool mca_pml_dr_check_cantmatch_for_match(
    opal_list_t *additional_matches,
    mca_pml_dr_comm_t* comm,
    mca_pml_dr_comm_proc_t *proc)
{
    /* local parameters */
    int match_found;
    uint16_t next_msg_seq_expected, frag_seq;
    mca_pml_dr_recv_frag_t *frag;
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
        for(frag = (mca_pml_dr_recv_frag_t *) 
            opal_list_get_first(&proc->frags_cant_match);
            frag != (mca_pml_dr_recv_frag_t *)
            opal_list_get_end(&proc->frags_cant_match);
            frag = (mca_pml_dr_recv_frag_t *) 
            opal_list_get_next(frag))
        {
            /*
             * If the message has the next expected seq from that proc...
             */
            frag_seq=frag->hdr.hdr_match.hdr_seq;
            if (frag_seq == next_msg_seq_expected) {
                mca_pml_dr_recv_request_t *match = NULL;
                mca_pml_dr_match_hdr_t* hdr = &frag->hdr.hdr_match;

                /* We're now expecting the next sequence number. */
                (proc->expected_sequence)++;

                /* signal that match was made */
                match_found = 1;

                /*
                 * remove frag from list
                 */
                opal_list_remove_item(&proc->frags_cant_match,
                        (opal_list_item_t *)frag);

rematch:
                /*
                 * figure out what sort of matching logic to use, if need to
                 *   look only at "specific" receives, or "wild" receives,
                 *   or if we need to traverse both sets at the same time.
                 */
                proc = comm->procs + hdr->hdr_common.hdr_src;
                if (opal_list_get_size(&proc->specific_receives) == 0 ) {
                    /*
                     * There are only wild irecvs, so specialize the algorithm.
                     */
                    MCA_PML_DR_CHECK_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
                } else if (opal_list_get_size(&comm->wild_receives) == 0 ) {
                    /*
                     * There are only specific irecvs, so specialize the algorithm.
                     */
                    MCA_PML_DR_CHECK_SPECIFIC_RECEIVES_FOR_MATCH(hdr, comm, proc, match);
                } else {
                    /*
                     * There are some of each.
                     */
                    MCA_PML_DR_CHECK_SPECIFIC_AND_WILD_RECEIVES_FOR_MATCH(hdr, comm, proc, match);

                }

                /* if match found, process data */
                if (match) {

                    /*
                     * If this was a probe need to queue fragment on unexpected list
                     */
                    if( (match->req_recv.req_base.req_type == MCA_PML_REQUEST_PROBE) ) {

                        /* complete the probe */
                        mca_pml_dr_recv_request_matched_probe(match,frag->btl,frag->segments,frag->num_segments);

                        /* retry the match */
                        match = NULL;
                        goto rematch;

                    } else {

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
                    }

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


