/** @file */

/*
 * $HEADER$
 */

#include <stdio.h>

#include "lam/lfc/list.h"
#include "lam/threads/mutex.h"
#include "lam/constants.h"
#include "mpi/communicator/communicator.h"
#include "mca/mpi/pml/pml.h"
#include "mca/mpi/ptl/ptl.h"
#include "mca/mpi/ptl/base/ptl_base_comm.h"
#include "mca/mpi/ptl/base/ptl_base_recvfrag.h"
#include "mca/mpi/ptl/base/ptl_base_header.h"
#include "mca/mpi/ptl/base/ptl_base_match.h"

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
 *                    case, the associted fratment descriptors are
 *                    put on this list for further processing. (OUT)
 *
 * @return LAM error code
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
int mca_ptl_base_match(mca_ptl_base_reliable_hdr_t *frag_header,
        mca_ptl_base_recv_frag_t *frag_desc, int *match_made, 
        lam_list_t *additional_matches)
{
	/* local variables */
	mca_ptl_base_sequence_t frag_msg_seq_num,next_msg_seq_num_expected;
	lam_communicator_t *comm_ptr;
	mca_ptl_base_recv_request_t *matched_receive;
    mca_pml_comm_t *pml_comm;
	int frag_src;

    /* initialization */
    *match_made=0;

	/* communicator pointer */
	comm_ptr=lam_comm_lookup(frag_header->hdr_base.hdr_contextid);
    pml_comm=(mca_pml_comm_t *)comm_ptr->c_pml_comm;

	/* source sequence number */
	frag_msg_seq_num = frag_header->hdr_msg_seq_num;

	/* get fragment communicator source rank */
	frag_src = frag_header->hdr_frag_seq_num;

	/* get next expected message sequence number - if threaded
	 * run, lock to make sure that if another thread is processing 
	 * a frag from the same message a match is made only once.
	 * Also, this prevents other posted receives (for a pair of
	 * end points) from being processed, and potentially "loosing"
	 * the fragment.
	 */
    THREAD_LOCK((pml_comm->c_matching_lock)+frag_src);

	/* get sequence number of next message that can be processed */
	next_msg_seq_num_expected = *((pml_comm->c_next_msg_seq_num)+frag_src);

	if (frag_msg_seq_num == next_msg_seq_num_expected) {

		/*
		 * This is the sequence number we were expecting,
		 * so we can try matching it to already posted
		 * receives.
		 */

		/* We're now expecting the next sequence number. */
		(pml_comm->c_next_msg_seq_num[frag_src])++;

		/* see if receive has already been posted */
		matched_receive = mca_ptl_base_check_recieves_for_match(frag_header,
                pml_comm);

		/* if match found, process data */
		if (matched_receive) {
			/* 
			 * if threaded, ok to release lock, since the posted
			 * receive is not on any queue, so it won't be
			 * matched again, and the fragment can be processed
			 * w/o any conflict from other threads - locks will
			 * be used where concurent access needs to be managed.
			 */

            /* set flag indicating the input fragment was matched */
            *match_made=1;
            /* associate the receive descriptor with the fragment
             * descriptor */
            frag_desc->matched_recv=matched_receive;

			/*
			 * update deliverd sequence number information,
			 *   if need be.
			 */

		} else {
			/* if no match found, place on unexpected queue - need to
             * lock to prevent probe from interfering with updating
             * the list */
            THREAD_LOCK((pml_comm->unexpected_frags_lock)+frag_src);
            lam_list_append( ((pml_comm->unexpected_frags)+frag_src),
                    (lam_list_item_t *)frag_desc);
            THREAD_UNLOCK((pml_comm->unexpected_frags_lock)+frag_src);

			/* now that the fragment is on the list, ok to
			 * release match - other matches may be attempted */
            THREAD_UNLOCK((pml_comm->c_matching_lock)+frag_src);
		}

		/* 
		 * Now that new message has arrived, check to see if
		 *   any fragments on the c_frags_cant_match list
		 *   may now be used to form new matchs
		 */
		if (lam_list_get_size((pml_comm->frags_cant_match)+frag_src)) {
            /* initialize list to empty */
            lam_list_set_size(additional_matches,0);
			/* need to handle this -- lam_check_cantmatch_for_match();
             * */
		}

		/* 
		 * mark message as done, if it has completed - need to mark
		 *   it this late to avoid a race condition with another thread
		 *   waiting to complete a recv, completing, and try to free the
		 *   communicator before the current thread is done referencing
		 *   this communicator - is this true ?
		 */
    } else {
        /*
         * This message comes after the next expected, so it
         * is ahead of sequence.  Save it for later.
         */
        lam_list_append( ((pml_comm->frags_cant_match)+frag_src),
                    (lam_list_item_t *)frag_desc);

        /* now that the fragment is on the list, ok to
         * release match - other matches may be attempted */
        THREAD_UNLOCK((pml_comm->c_matching_lock)+frag_src);
    }

    return LAM_SUCCESS;
}

/**
 * Upper level routine for matching a recieved message header to posted
 * receives.
 *
 * @param frag_header Matching data from recived fragment (IN)
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
mca_ptl_base_recv_request_t *mca_ptl_base_check_recieves_for_match
  (mca_ptl_base_reliable_hdr_t *frag_header, mca_pml_comm_t *pml_comm)
{
    /* local parameters */
    mca_ptl_base_recv_request_t *return_match;
    int frag_src;

    /* initialization */
    return_match=(mca_ptl_base_recv_request_t *)NULL;

    /*
     * figure out what sort of matching logic to use, if need to
     *   look only at "specific" recieves, or "wild" receives,
     *   or if we need to traverse both sets at the same time.
     */
    frag_src = frag_header->hdr_frag_seq_num;

    if (lam_list_get_size((pml_comm->specific_receives)+frag_src) == 0 ){
        /*
         * There are only wild irecvs, so specialize the algorithm.
         */
        return_match = check_wild_receives_for_match(frag_header, pml_comm);
    } else if (lam_list_get_size(&(pml_comm->wild_receives)) == 0 ) {
        /*
         * There are only specific irecvs, so specialize the algorithm.
         */
        return_match = check_specific_receives_for_match(frag_header, 
                pml_comm);
    } else {
        /*
         * There are some of each.
         */
        return_match = check_specific_and_wild_receives_for_match(frag_header, 
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
mca_ptl_base_recv_request_t *check_wild_receives_for_match(
        mca_ptl_base_reliable_hdr_t *frag_header,
        mca_pml_comm_t *pml_comm)
{
    /* local parameters */
    mca_ptl_base_recv_request_t *return_match, *wild_recv;
    int frag_user_tag,recv_user_tag;

    /* initialization */
    return_match=(mca_ptl_base_recv_request_t *)NULL;
    frag_user_tag=frag_header->hdr_base.hdr_user_tag;

    /*
     * Loop over the wild irecvs - no need to lock, the upper level
     * locking is protecting from having other threads trying to
     * change this list.
     */
    for(wild_recv = (mca_ptl_base_recv_request_t *) 
            lam_list_get_first(&(pml_comm->wild_receives));
            wild_recv != (mca_ptl_base_recv_request_t *)
            lam_list_get_end(&(pml_comm->wild_receives));
            wild_recv = (mca_ptl_base_recv_request_t *) 
            ((lam_list_item_t *)wild_recv)->lam_list_next) {

        recv_user_tag = wild_recv->super.req_tag;
        if ( 
                /* exact tag match */
                (frag_user_tag == recv_user_tag) ||
                /* wild tag match - negative tags (except for
                 * LAM_ANY_TAG) are reserved for internal use, and will
                 * not be matched with LAM_ANY_TAG */
                ( (recv_user_tag == LAM_ANY_TAG) && (0 <= frag_user_tag) )  )

        {
            /*
             * Mark that this is the matching irecv, and go to process it.
             */
            return_match = wild_recv;

            /* remove this irecv from the postd wild ireceive list */
            lam_list_remove_item(&(pml_comm->wild_receives),
                    (lam_list_item_t *)wild_recv);

            /* found match - no need to continue */
            break;
        }
    }
    //

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
mca_ptl_base_recv_request_t *check_specific_receives_for_match(
        mca_ptl_base_reliable_hdr_t *frag_header,
        mca_pml_comm_t *pml_comm)
{
    /* local variables */
    mca_ptl_base_recv_request_t *specific_recv, *return_match;
    int frag_src,recv_user_tag,frag_user_tag;


    /* initialization */
    return_match=(mca_ptl_base_recv_request_t *)NULL;
    frag_src = frag_header->hdr_frag_seq_num;
    frag_user_tag=frag_header->hdr_base.hdr_user_tag;

    /*
     * Loop over the specific irecvs.
     */
    for(specific_recv = (mca_ptl_base_recv_request_t *) 
            lam_list_get_first((pml_comm->specific_receives)+frag_src);
            specific_recv != (mca_ptl_base_recv_request_t *)
            lam_list_get_end((pml_comm->specific_receives)+frag_src);
            specific_recv = (mca_ptl_base_recv_request_t *) 
            ((lam_list_item_t *)specific_recv)->lam_list_next) {
        /*
         * Check for a match
         */
        recv_user_tag = specific_recv->super.req_tag;
        if ( (frag_user_tag == recv_user_tag) ||
             ( (recv_user_tag == LAM_ANY_TAG) && (0 <= frag_user_tag) ) ) {

            /*
             * Match made
             */
            return_match = specific_recv;

            /* remove descriptor from posted specific ireceive list */
            lam_list_remove_item((pml_comm->specific_receives)+frag_src,
                    (lam_list_item_t *)specific_recv);

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
mca_ptl_base_recv_request_t *check_specific_and_wild_receives_for_match(
        mca_ptl_base_reliable_hdr_t *frag_header,
        mca_pml_comm_t *pml_comm)
{
    /* local variables */
    mca_ptl_base_recv_request_t *specific_recv, *wild_recv, *return_match;
    mca_ptl_base_sequence_t wild_recv_seq_num, specific_recv_seq_num;
    int frag_src,frag_user_tag, wild_recv_tag, specific_recv_tag;

    /* initialization */
    return_match=(mca_ptl_base_recv_request_t *)NULL;
    frag_src = frag_header->hdr_frag_seq_num;
    frag_user_tag=frag_header->hdr_base.hdr_user_tag;

    /*
     * We know that when this is called, both specific and wild irecvs
     *  have been posted.
     */
    specific_recv = (mca_ptl_base_recv_request_t *) 
            lam_list_get_first((pml_comm->specific_receives)+frag_src);
    wild_recv = (mca_ptl_base_recv_request_t *) 
            lam_list_get_first(&(pml_comm->wild_receives));

    specific_recv_seq_num = specific_recv->req_sequence;
    wild_recv_seq_num = wild_recv->req_sequence;

    while (true) {
        if (wild_recv_seq_num < specific_recv_seq_num) {
            /*
             * wild recv is earlier than the specific one.
             */
            /*
             * try and match
             */
            wild_recv_tag = wild_recv->super.req_tag;
            if ( (frag_user_tag == wild_recv_tag) ||
                 ( (wild_recv_tag == LAM_ANY_TAG) && (0 <= frag_user_tag) ) ) {
                    
                /*
                 * Match made
                 */
                return_match=wild_recv;

                /* remove this recv from the wild receive queue */
                lam_list_remove_item(&(pml_comm->wild_receives),
                        (lam_list_item_t *)wild_recv);

                return return_match;
            }

            /*
             * No match, go to the next.
             */
            wild_recv=(mca_ptl_base_recv_request_t *)
                ((lam_list_item_t *)wild_recv)->lam_list_next;

            /*
             * If that was the last wild one, just look at the
             * rest of the specific ones.
             */
            if (wild_recv == (mca_ptl_base_recv_request_t *)
                    lam_list_get_end(&(pml_comm->wild_receives)) ) 
            { 
                return_match = check_specific_receives_for_match(frag_header,
                        pml_comm);

                return return_match;
            }

            /*
             * Get the sequence number for this recv, and go
             * back to the top of the loop.
             */
            wild_recv_seq_num = wild_recv->req_sequence;

        } else {
            /*
             * specific recv is earlier than the wild one.
             */
            specific_recv_tag=specific_recv->super.req_tag;
            if ( (frag_user_tag == specific_recv_tag) ||
                 ( (specific_recv_tag == LAM_ANY_TAG) && (0<=frag_user_tag)) ) 
            {

                /*
                 * Match made
                 */
                return_match = specific_recv;

                /* remove descriptor from specific receive list */
                lam_list_remove_item((pml_comm->specific_receives)+frag_src,
                    (lam_list_item_t *)specific_recv);

                return return_match;
            }

            /*
             * No match, go on to the next specific irecv.
             */
            specific_recv = (mca_ptl_base_recv_request_t *)
                ((lam_list_item_t *)specific_recv)->lam_list_next;

            /*
             * If that was the last specific irecv, process the
             * rest of the wild ones.
             */
            if (specific_recv == (mca_ptl_base_recv_request_t *)
                    lam_list_get_end((pml_comm->specific_receives)+frag_src) )
            {
                return_match = check_wild_receives_for_match(frag_header,
                        pml_comm);

                return return_match;
            }
            /*
             * Get the sequence number for this recv, and go
             * back to the top of the loop.
             */
            specific_recv_seq_num = specific_recv->req_sequence;
        }
    }
}
