/** @file */

/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "class/ompi_list.h"
#include "threads/mutex.h"
#include "include/constants.h"
#include "communicator/communicator.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_comm.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_match.h"


/*
 * Specialized matching routines for internal use only.
 */

static mca_pml_base_recv_request_t *mca_ptl_base_check_receives_for_match(
    mca_ptl_base_match_header_t *frag_header,
    mca_pml_ptl_comm_t *ptl_comm);
                                                                                                                            
static mca_pml_base_recv_request_t *mca_ptl_base_check_wild_receives_for_match(
    mca_ptl_base_match_header_t *frag_header,
    mca_pml_ptl_comm_t *ptl_comm);
                                                                                                                            
static mca_pml_base_recv_request_t *mca_ptl_base_check_specific_receives_for_match(
    mca_ptl_base_match_header_t *frag_header,
    mca_pml_ptl_comm_t *ptl_comm);
                                                                                                                            
static mca_pml_base_recv_request_t *mca_ptl_base_check_specific_and_wild_receives_for_match(
    mca_ptl_base_match_header_t *frag_header,
    mca_pml_ptl_comm_t *ptl_comm);
                                                                                                                            
static void mca_ptl_base_check_cantmatch_for_match(
    ompi_list_t *additional_matches,
    mca_pml_ptl_comm_t *pml_comm, int frag_src);


/**
 * RCS/CTS receive side matching
 *
 * @param frag_header list of parameters needed for matching
 *                    This list is also embeded in frag_desc,
 *                    but this allows to save a memory copy when
 *                    a match is made in this routine. (IN)
 * @param frag_desc   pointer to receive fragment which we want
 *                    to match (IN/OUT).  If a match is not made,
 *                    frag_header is copied to frag_desc.
 * @param match_made  parameter indicating if we matched frag_desc/
 *                    frag_header (OUT)
 * @param additional_matches  if a match is made with frag_desc, we
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
 *   - this routine may be called simoultaneously by more than one thread
 */
bool mca_ptl_base_match(
    mca_ptl_base_match_header_t *frag_header,
    mca_ptl_base_recv_frag_t *frag_desc, 
    ompi_list_t *additional_matches)
{
    /* local variables */
    mca_ptl_sequence_t frag_msg_seq,next_msg_seq_expected;
    ompi_communicator_t *comm_ptr;
    mca_pml_base_recv_request_t *matched_receive;
    mca_pml_ptl_comm_t *pml_comm;
    int frag_src;
    bool match_made=false;

    /* communicator pointer */
    comm_ptr=ompi_comm_lookup(frag_header->hdr_contextid);
    pml_comm=(mca_pml_ptl_comm_t *)comm_ptr->c_pml_comm;

    /* source sequence number */
    frag_msg_seq = frag_header->hdr_msg_seq;

    /* get fragment communicator source rank */
    frag_src = frag_header->hdr_src;

    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing 
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "loosing"
     * the fragment.
     */
    OMPI_THREAD_LOCK(&pml_comm->c_matching_lock);

    /* get sequence number of next message that can be processed */
    next_msg_seq_expected = *((pml_comm->c_next_msg_seq)+frag_src);

    if (frag_msg_seq == next_msg_seq_expected) {

        /*
         * This is the sequence number we were expecting,
         * so we can try matching it to already posted
         * receives.
         */

        /* We're now expecting the next sequence number. */
        (pml_comm->c_next_msg_seq[frag_src])++;

        /* see if receive has already been posted */
        matched_receive = mca_ptl_base_check_receives_for_match(frag_header,
                pml_comm);

        /* if match found, process data */
        if (matched_receive) {

            /* set flag indicating the input fragment was matched */
            match_made=true;
            /* associate the receive descriptor with the fragment
             * descriptor */
            frag_desc->frag_request=matched_receive;

            /*
             * update deliverd sequence number information,
             *   if need be.
             */

        } else {
            /* if no match found, place on unexpected queue */
            ompi_list_append( ((pml_comm->c_unexpected_frags)+frag_src),
                    (ompi_list_item_t *)frag_desc);

        }

        /* 
         * Now that new message has arrived, check to see if
         *   any fragments on the c_c_frags_cant_match list
         *   may now be used to form new matchs
         */
        if (0 < ompi_list_get_size((pml_comm->c_frags_cant_match)+frag_src)) {

            mca_ptl_base_check_cantmatch_for_match(additional_matches,pml_comm,frag_src);

        }

    } else {
        /*
         * This message comes after the next expected, so it
         * is ahead of sequence.  Save it for later.
         */
        ompi_list_append( ((pml_comm->c_frags_cant_match)+frag_src),
                    (ompi_list_item_t *)frag_desc);
    }

    OMPI_THREAD_UNLOCK(&pml_comm->c_matching_lock);
    return match_made;
}

/**
 * Upper level routine for matching a received message header to posted
 * receives.
 *
 * @param frag_header Matching data from received fragment (IN)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * try and match frag to posted receives The calling routine
 * garantees that no other thread will access the posted receive
 * queues while this routine is being executed.
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */
static mca_pml_base_recv_request_t *mca_ptl_base_check_receives_for_match
  (mca_ptl_base_match_header_t *frag_header, mca_pml_ptl_comm_t *pml_comm)
{
    /* local parameters */
    mca_pml_base_recv_request_t *return_match;
    int frag_src;

    /* initialization */
    return_match=(mca_pml_base_recv_request_t *)NULL;

    /*
     * figure out what sort of matching logic to use, if need to
     *   look only at "specific" receives, or "wild" receives,
     *   or if we need to traverse both sets at the same time.
     */
    frag_src = frag_header->hdr_src;

    if (ompi_list_get_size((pml_comm->c_specific_receives)+frag_src) == 0 ){
        /*
         * There are only wild irecvs, so specialize the algorithm.
         */
        return_match = mca_ptl_base_check_wild_receives_for_match(frag_header, pml_comm);

    } else if (ompi_list_get_size(&(pml_comm->c_wild_receives)) == 0 ) {
        /*
         * There are only specific irecvs, so specialize the algorithm.
         */
        return_match = mca_ptl_base_check_specific_receives_for_match(frag_header, 
                pml_comm);
    } else {
        /*
         * There are some of each.
         */
        return_match = mca_ptl_base_check_specific_and_wild_receives_for_match(frag_header, 
                pml_comm);
    }

    return return_match;
}


/**
 * Try and match the incoming message fragment to the list of
 * "wild" receives
 *
 * @param frag_header Matching data from recived fragment (IN)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */
static mca_pml_base_recv_request_t *mca_ptl_base_check_wild_receives_for_match(
        mca_ptl_base_match_header_t *frag_header,
        mca_pml_ptl_comm_t *pml_comm)
{
    /* local parameters */
    mca_pml_base_recv_request_t *return_match, *wild_recv;
    int frag_tag,recv_tag;

    /* initialization */
    return_match=(mca_pml_base_recv_request_t *)NULL;
    frag_tag=frag_header->hdr_tag;

    /*
     * Loop over the wild irecvs - no need to lock, the upper level
     * locking is protecting from having other threads trying to
     * change this list.
     */
    for(wild_recv = (mca_pml_base_recv_request_t *) 
            ompi_list_get_first(&(pml_comm->c_wild_receives));
            wild_recv != (mca_pml_base_recv_request_t *)
            ompi_list_get_end(&(pml_comm->c_wild_receives));
            wild_recv = (mca_pml_base_recv_request_t *) 
            ((ompi_list_item_t *)wild_recv)->ompi_list_next) {

        recv_tag = wild_recv->req_base.req_tag;
        if ( 
                /* exact tag match */
                (frag_tag == recv_tag) ||
                /* wild tag match - negative tags (except for
                 * OMPI_ANY_TAG) are reserved for internal use, and will
                 * not be matched with OMPI_ANY_TAG */
                ( (recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) )  )

        {
            /*
             * Mark that this is the matching irecv, and go to process it.
             */
            return_match = wild_recv;

            /* remove this irecv from the postd wild ireceive list */
            ompi_list_remove_item(&(pml_comm->c_wild_receives),
                    (ompi_list_item_t *)wild_recv);

            /* found match - no need to continue */
            break;
        }
    }

    return return_match;
}


/**
 * Try and match the incoming message fragment to the list of
 * "specific" receives
 *
 * @param frag_header Matching data from recived fragment (IN)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */
static mca_pml_base_recv_request_t *mca_ptl_base_check_specific_receives_for_match(
        mca_ptl_base_match_header_t *frag_header,
        mca_pml_ptl_comm_t *pml_comm)
{
    /* local variables */
    mca_pml_base_recv_request_t *specific_recv, *return_match;
    int frag_src,recv_tag,frag_tag;


    /* initialization */
    return_match=(mca_pml_base_recv_request_t *)NULL;
    frag_src = frag_header->hdr_src;
    frag_tag=frag_header->hdr_tag;

    /*
     * Loop over the specific irecvs.
     */
    for(specific_recv = (mca_pml_base_recv_request_t *) 
            ompi_list_get_first((pml_comm->c_specific_receives)+frag_src);
            specific_recv != (mca_pml_base_recv_request_t *)
            ompi_list_get_end((pml_comm->c_specific_receives)+frag_src);
            specific_recv = (mca_pml_base_recv_request_t *) 
            ((ompi_list_item_t *)specific_recv)->ompi_list_next) {
        /*
         * Check for a match
         */
        recv_tag = specific_recv->req_base.req_tag;
        if ( (frag_tag == recv_tag) ||
             ( (recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) ) ) {

            /*
             * Match made
             */
            return_match = specific_recv;

            /* remove descriptor from posted specific ireceive list */
            ompi_list_remove_item((pml_comm->c_specific_receives)+frag_src,
                    (ompi_list_item_t *)specific_recv);

            break;
        }
    }

    return return_match;
}

/**
 * Try and match the incoming message fragment to the list of
 * "wild" receives and "specific" receives.  Used when both types
 * of receives have been posted,  i.e. when we need to coordinate
 * between multiple lists to make sure ordered delivery occurs.
 *
 * @param frag_header Matching data from recived fragment (IN)
 *
 * @param pml_comm Pointer to the communicator structure used for
 * matching purposes. (IN)
 *
 * @return Matched receive
 *
 * This routine assumes that the appropriate matching locks are
 * set by the upper level routine.
 */
static mca_pml_base_recv_request_t *mca_ptl_base_check_specific_and_wild_receives_for_match(
        mca_ptl_base_match_header_t *frag_header,
        mca_pml_ptl_comm_t *pml_comm)
{
    /* local variables */
    mca_pml_base_recv_request_t *specific_recv, *wild_recv, *return_match;
    mca_ptl_sequence_t wild_recv_seq, specific_recv_seq;
    int frag_src,frag_tag, wild_recv_tag, specific_recv_tag;

    /* initialization */
    return_match=(mca_pml_base_recv_request_t *)NULL;
    frag_src = frag_header->hdr_src;
    frag_tag=frag_header->hdr_tag;

    /*
     * We know that when this is called, both specific and wild irecvs
     *  have been posted.
     */
    specific_recv = (mca_pml_base_recv_request_t *) 
            ompi_list_get_first((pml_comm->c_specific_receives)+frag_src);
    wild_recv = (mca_pml_base_recv_request_t *) 
            ompi_list_get_first(&(pml_comm->c_wild_receives));

    specific_recv_seq = specific_recv->req_base.req_sequence;
    wild_recv_seq = wild_recv->req_base.req_sequence;

    while (true) {
        if (wild_recv_seq < specific_recv_seq) {
            /*
             * wild recv is earlier than the specific one.
             */
            /*
             * try and match
             */
            wild_recv_tag = wild_recv->req_base.req_tag;
            if ( (frag_tag == wild_recv_tag) ||
                 ( (wild_recv_tag == OMPI_ANY_TAG) && (0 <= frag_tag) ) ) {
                    
                /*
                 * Match made
                 */
                return_match=wild_recv;

                /* remove this recv from the wild receive queue */
                ompi_list_remove_item(&(pml_comm->c_wild_receives),
                        (ompi_list_item_t *)wild_recv);

                return return_match;
            }

            /*
             * No match, go to the next.
             */
            wild_recv=(mca_pml_base_recv_request_t *)
                ((ompi_list_item_t *)wild_recv)->ompi_list_next;

            /*
             * If that was the last wild one, just look at the
             * rest of the specific ones.
             */
            if (wild_recv == (mca_pml_base_recv_request_t *)
                    ompi_list_get_end(&(pml_comm->c_wild_receives)) ) 
            { 
                return_match = mca_ptl_base_check_specific_receives_for_match(frag_header,
                        pml_comm);

                return return_match;
            }

            /*
             * Get the sequence number for this recv, and go
             * back to the top of the loop.
             */
            wild_recv_seq = wild_recv->req_base.req_sequence;

        } else {
            /*
             * specific recv is earlier than the wild one.
             */
            specific_recv_tag=specific_recv->req_base.req_tag;
            if ( (frag_tag == specific_recv_tag) ||
                 ( (specific_recv_tag == OMPI_ANY_TAG) && (0<=frag_tag)) ) 
            {

                /*
                 * Match made
                 */
                return_match = specific_recv;

                /* remove descriptor from specific receive list */
                ompi_list_remove_item((pml_comm->c_specific_receives)+frag_src,
                    (ompi_list_item_t *)specific_recv);

                return return_match;
            }

            /*
             * No match, go on to the next specific irecv.
             */
            specific_recv = (mca_pml_base_recv_request_t *)
                ((ompi_list_item_t *)specific_recv)->ompi_list_next;

            /*
             * If that was the last specific irecv, process the
             * rest of the wild ones.
             */
            if (specific_recv == (mca_pml_base_recv_request_t *)
                    ompi_list_get_end((pml_comm->c_specific_receives)+frag_src) )
            {
                return_match = mca_ptl_base_check_wild_receives_for_match(frag_header,
                        pml_comm);

                return return_match;
            }
            /*
             * Get the sequence number for this recv, and go
             * back to the top of the loop.
             */
            specific_recv_seq = specific_recv->req_base.req_sequence;
        }
    }
}

            /* need to handle this -- mca_ptl_base_check_cantmatch_for_match();
             */
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

static void mca_ptl_base_check_cantmatch_for_match(ompi_list_t *additional_matches,
    mca_pml_ptl_comm_t *pml_comm, int frag_src)
{
    /* local parameters */
    int match_found;
    mca_ptl_sequence_t next_msg_seq_expected, frag_seq;
    mca_ptl_base_recv_frag_t *frag_desc;
    mca_pml_base_recv_request_t *matched_receive;

    /*
     * Loop over all the out of sequence messages.  No ordering is assumed
     * in the c_frags_cant_match list.
     */

    match_found = 1;
    while ((0 < ompi_list_get_size((pml_comm->c_frags_cant_match)+frag_src)) &&
            match_found) {

        /* initialize match flag for this search */
        match_found = 0;

        /* get sequence number of next message that can be processed */
        next_msg_seq_expected = *((pml_comm->c_next_msg_seq)+frag_src);

        /* search the list for a fragment from the send with sequence
         * number next_msg_seq_expected
         */
        for(frag_desc = (mca_ptl_base_recv_frag_t *) 
            ompi_list_get_first((pml_comm->c_frags_cant_match)+frag_src);
            frag_desc != (mca_ptl_base_recv_frag_t *)
            ompi_list_get_end((pml_comm->c_frags_cant_match)+frag_src);
            frag_desc = (mca_ptl_base_recv_frag_t *) 
            ompi_list_get_next(frag_desc))
        {
            /*
             * If the message has the next expected seq from that proc...
             */
            frag_seq=frag_desc->frag_base.frag_header.hdr_match.hdr_msg_seq;
            if (frag_seq == next_msg_seq_expected) {

                /* We're now expecting the next sequence number. */
                (pml_comm->c_next_msg_seq[frag_src])++;

                /* signal that match was made */
                match_found = 1;

                /*
                 * remove frag_desc from list
                 */
                ompi_list_remove_item((pml_comm->c_frags_cant_match)+frag_src,
                        (ompi_list_item_t *)frag_desc);

                /*
                 * check to see if this frag matches a posted message
                 */
                matched_receive = mca_ptl_base_check_receives_for_match(
                       &frag_desc->frag_base.frag_header.hdr_match, pml_comm);

                /* if match found, process data */
                if (matched_receive) {

                    /* associate the receive descriptor with the fragment
                     * descriptor */
                    frag_desc->frag_request=matched_receive;

                    /* add this fragment descriptor to the list of
                     * descriptors to be processed later
                     */
                    ompi_list_append(additional_matches,
                            (ompi_list_item_t *)frag_desc);

                } else {
    
                    /* if no match found, place on unexpected queue */
                    ompi_list_append( ((pml_comm->c_unexpected_frags)+frag_src),
                            (ompi_list_item_t *)frag_desc);

                }

                /* c_frags_cant_match is not an ordered list, so exit loop
                 * and re-start search for next sequence number */
                break;

            } /* end if (frag_seq == next_msg_seq_expected) */
            
        } /* end for (frag_desc) loop */
        
    } /* end while loop */

    return;
}

/**
 * RCS/CTS receive side matching
 *
 * @param frag_header list of parameters needed for matching
 *                    This list is also embeded in frag_desc,
 *                    but this allows to save a memory copy when
 *                    a match is made in this routine. (IN)
 * @param frag_desc   pointer to receive fragment which we want
 *                    to match (IN/OUT).  If a match is not made,
 *                    frag_header is copied to frag_desc.
 * @param match_made  parameter indicating if we matched frag_desc/
 *                    frag_header (OUT)
 * @return indication if match was made or not.
 *
 * This routine is used to try and match a newly arrived message fragment
 *   to pre-posted receives.  The following assumptions are made
 *   - fragments are received in order, so no explicit sequence
 *     tracking is needed.
 *   - for long messages, e.g. more than one fragment, a RTS/CTS algorithm
 *       is used.
 *   - 2nd and greater fragments include a receive descriptor pointer
 *   - this routine may be called simoultaneously by more than one thread
 *
 *  On return, if match is made:
 *       neither the fragment, nor the matched receive descriptor
 *       are on any list
 *  if match is not made:
 *       The fragment is placed on the unexpected fragment list
 */
bool mca_ptl_base_match_in_order_network_delivery(
    mca_ptl_base_match_header_t *frag_header,
    struct mca_ptl_base_recv_frag_t *frag_desc)
{
    /* local variables */
    mca_ptl_sequence_t frag_msg_seq,next_msg_seq_expected;
    ompi_communicator_t *comm_ptr;
    mca_pml_base_recv_request_t *matched_receive;
    mca_pml_ptl_comm_t *pml_comm;
    int frag_src;

    bool match_made=false;

    /* communicator pointer */
    comm_ptr=ompi_comm_lookup(frag_header->hdr_contextid);
    pml_comm=(mca_pml_ptl_comm_t *)comm_ptr->c_pml_comm;

    /* source sequence number */
    frag_msg_seq = frag_header->hdr_msg_seq;

    /* get fragment communicator source rank */
    frag_src = frag_header->hdr_src;

    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing 
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "loosing"
     * the fragment.
     */
    OMPI_THREAD_LOCK(&pml_comm->c_matching_lock);

    /* see if receive has already been posted */
    matched_receive = mca_ptl_base_check_receives_for_match(frag_header,
            pml_comm);

    /* if match found, process data */
    if (matched_receive) {
        /* set flag indicating the input fragment was matched */
        match_made=true;

        /* associate the receive descriptor with the fragment descriptor */
        frag_desc->frag_request=matched_receive;
    } else {
        /* if no match found, place on unexpected queue */
        ompi_list_append( ((pml_comm->c_unexpected_frags)+frag_src),
                (ompi_list_item_t *)frag_desc);
    }


    OMPI_THREAD_UNLOCK(&pml_comm->c_matching_lock);
    return match_made;
}
