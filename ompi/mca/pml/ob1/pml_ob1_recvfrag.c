/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2006-2008 University of Houston.  All rights reserved.
 * Copyright (c) 2009-2010 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Sandia National Laboratories
 *                         All rights reserved.
 * Copyright (c) 2020      Google, LLC. All rights reserved.
 * Copyright (c) 2020-2021 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
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
#include "opal/mca/threads/mutex.h"
#include "opal/prefetch.h"

#include "ompi/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/peruse/peruse-internal.h"
#include "ompi/runtime/ompi_spc.h"

#include "pml_ob1.h"
#include "pml_ob1_comm.h"
#include "pml_ob1_recvfrag.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_hdr.h"
#include "pml_ob1_accelerator.h"

OBJ_CLASS_INSTANCE( mca_pml_ob1_buffer_t,
                    opal_free_list_item_t,
                    NULL,
                    NULL );

OBJ_CLASS_INSTANCE( mca_pml_ob1_recv_frag_t,
                    opal_list_item_t,
                    NULL,
                    NULL );

/**
 * Static functions.
 */

/**
 * Append an unexpected descriptor to a queue. This function will allocate and
 * initialize the fragment (if necessary) and then will add it to the specified
 * queue. The allocated fragment is not returned to the caller.
 */

static void
append_frag_to_list(opal_list_t *queue, mca_btl_base_module_t *btl,
                    const mca_pml_ob1_match_hdr_t *hdr, const mca_btl_base_segment_t *segments,
                    size_t num_segments, mca_pml_ob1_recv_frag_t* frag)
{
    if(NULL == frag) {
        MCA_PML_OB1_RECV_FRAG_ALLOC(frag);
        MCA_PML_OB1_RECV_FRAG_INIT(frag, hdr, segments, num_segments, btl);
    }
    opal_list_append(queue, (opal_list_item_t*)frag);
}

#if MCA_PML_OB1_CUSTOM_MATCH

static void
append_frag_to_umq(custom_match_umq *queue, mca_btl_base_module_t *btl,
                   const mca_pml_ob1_match_hdr_t *hdr, const mca_btl_base_segment_t *segments,
                   size_t num_segments, mca_pml_ob1_recv_frag_t* frag)
{
  if(NULL == frag) {
    MCA_PML_OB1_RECV_FRAG_ALLOC(frag);
    MCA_PML_OB1_RECV_FRAG_INIT(frag, hdr, segments, num_segments, btl);
  }
  custom_match_umq_append(queue, hdr->hdr_tag, hdr->hdr_src, frag);
}

#endif


/**
 * Append an unexpected descriptor to an ordered queue.
 *
 * use the opal_list_item_t to maintain themselves on an ordered list
 * according to their hdr_seq. Special care has been taken to cope with
 * overflowing the uint16_t we use for the hdr_seq. The current algorithm
 * works as long as there are no two elements with the same hdr_seq in the
 * list in same time (aka. no more than 2^16-1 left out-of-sequence
 * messages. On the vertical layer, messages with contiguous sequence
 * number organize themselves in a way to minimize the search space.
 */
void ompi_pml_ob1_append_frag_to_ordered_list (mca_pml_ob1_recv_frag_t **queue,
                                  mca_pml_ob1_recv_frag_t *frag,
                                  uint16_t seq)
{
    mca_pml_ob1_recv_frag_t  *prior, *next;
    const mca_pml_ob1_match_hdr_t *hdr = &frag->hdr.hdr_match;

    frag->super.super.opal_list_next = (opal_list_item_t*)frag;
    frag->super.super.opal_list_prev = (opal_list_item_t*)frag;
    frag->range = NULL;

    if (NULL == *queue) {  /* no pending fragments yet */
        *queue = frag;
        return;
    }

    prior = *queue;
    assert(hdr->hdr_seq != prior->hdr.hdr_match.hdr_seq);

    /* The hdr_seq being 16 bits long it can rollover rather quickly. We need to
     * account for this rollover or the matching will fail.
     * Extract the items from the list to order them safely */
    if( hdr->hdr_seq < prior->hdr.hdr_match.hdr_seq ) {
        uint16_t d1, d2 = prior->hdr.hdr_match.hdr_seq - hdr->hdr_seq;
        do {
            d1 = d2;
            prior = (mca_pml_ob1_recv_frag_t*)(prior->super.super.opal_list_prev);
            d2 = prior->hdr.hdr_match.hdr_seq - hdr->hdr_seq;
        } while( (hdr->hdr_seq < prior->hdr.hdr_match.hdr_seq) &&
                 (d1 > d2) && (prior != *queue) );
    } else {
        uint16_t prior_seq = prior->hdr.hdr_match.hdr_seq,
        next_seq = ((mca_pml_ob1_recv_frag_t*)(prior->super.super.opal_list_next))->hdr.hdr_match.hdr_seq;
        /* prevent rollover */
        while( (hdr->hdr_seq > prior_seq) && (hdr->hdr_seq > next_seq) && (prior_seq < next_seq) ) {
            prior_seq = next_seq;
            prior = (mca_pml_ob1_recv_frag_t*)(prior->super.super.opal_list_next);
            next_seq = ((mca_pml_ob1_recv_frag_t*)(prior->super.super.opal_list_next))->hdr.hdr_match.hdr_seq;
        }
    }

    /* prior is the fragment with a closest hdr_seq lesser than the current hdr_seq */
    mca_pml_ob1_recv_frag_t* parent = prior;

    /* Is this fragment the next in range ? */
    if( NULL == parent->range ) {
        if( (parent->hdr.hdr_match.hdr_seq + 1) == hdr->hdr_seq ) {
            parent->range = (mca_pml_ob1_recv_frag_t*)frag;
            goto merge_ranges;
        }
        /* all other cases fallback and add the frag after the parent */
    } else {
        /* can we add the frag to the range of the previous fragment ? */
        mca_pml_ob1_recv_frag_t* largest = (mca_pml_ob1_recv_frag_t*)parent->range->super.super.opal_list_prev;
        if( (largest->hdr.hdr_match.hdr_seq + 1) == hdr->hdr_seq ) {
            /* the frag belongs to this range */
            frag->super.super.opal_list_prev = (opal_list_item_t*)largest;
            frag->super.super.opal_list_next = largest->super.super.opal_list_next;
            frag->super.super.opal_list_prev->opal_list_next = (opal_list_item_t*)frag;
            frag->super.super.opal_list_next->opal_list_prev = (opal_list_item_t*)frag;
            goto merge_ranges;
        }
        /* all other cases fallback and add the frag after the parent */
    }

    frag->super.super.opal_list_prev = (opal_list_item_t*)prior;
    frag->super.super.opal_list_next = (opal_list_item_t*)prior->super.super.opal_list_next;
    frag->super.super.opal_list_prev->opal_list_next = (opal_list_item_t*)frag;
    frag->super.super.opal_list_next->opal_list_prev = (opal_list_item_t*)frag;
    parent = frag;  /* the frag is not part of a range yet */

    /* if the newly added element is closer to the next expected sequence mark it so */
    if( parent->hdr.hdr_match.hdr_seq >= seq )
        if( abs(parent->hdr.hdr_match.hdr_seq - seq) < abs((*queue)->hdr.hdr_match.hdr_seq - seq))
            *queue = parent;

 merge_ranges:
    /* is the next hdr_seq the increasing next one ? */
    next = (mca_pml_ob1_recv_frag_t*)parent->super.super.opal_list_next;
    uint16_t upper = parent->hdr.hdr_match.hdr_seq;
    if( NULL != parent->range ) {
        upper = ((mca_pml_ob1_recv_frag_t*)parent->range->super.super.opal_list_prev)->hdr.hdr_match.hdr_seq;
    }
    if( (upper + 1) == next->hdr.hdr_match.hdr_seq ) {
        /* remove next from the horizontal chain */
        next->super.super.opal_list_next->opal_list_prev = (opal_list_item_t*)parent;
        parent->super.super.opal_list_next = next->super.super.opal_list_next;
        /* merge next with it's own range */
        if( NULL != next->range ) {
            next->super.super.opal_list_next = (opal_list_item_t*)next->range;
            next->super.super.opal_list_prev = next->range->super.super.opal_list_prev;
            next->super.super.opal_list_next->opal_list_prev = (opal_list_item_t*)next;
            next->super.super.opal_list_prev->opal_list_next = (opal_list_item_t*)next;
            next->range = NULL;
        } else {
            next->super.super.opal_list_prev = (opal_list_item_t*)next;
            next->super.super.opal_list_next = (opal_list_item_t*)next;
        }
        if( NULL == parent->range ) {
            parent->range = next;
        } else {
            /* we have access to parent->range so make frag be it's predecessor */
            frag = (mca_pml_ob1_recv_frag_t*)parent->range->super.super.opal_list_prev;
            /* merge the 2 rings such that frag is right before next */
            frag->super.super.opal_list_next = (opal_list_item_t*)next;
            parent->range->super.super.opal_list_prev = next->super.super.opal_list_prev;
            next->super.super.opal_list_prev->opal_list_next = (opal_list_item_t*)parent->range;
            next->super.super.opal_list_prev = (opal_list_item_t*)frag;
        }
        if( next == *queue )
            *queue = parent;
    }
}

/*
 * remove the head of ordered list and restructure the list.
 */
static mca_pml_ob1_recv_frag_t*
remove_head_from_ordered_list(mca_pml_ob1_recv_frag_t** queue)
{
    mca_pml_ob1_recv_frag_t* frag = *queue;
    /* queue is empty, nothing to see. */
    if( NULL == *queue )
        return NULL;
    if( NULL == frag->range ) {
        /* head has no range, */
        if( frag->super.super.opal_list_next == (opal_list_item_t*)frag ) {
            /* head points to itself means it is the only
             * one in this queue. We set the new head to NULL */
            *queue = NULL;
        } else {
            /* make the next one a new head. */
            *queue = (mca_pml_ob1_recv_frag_t*)frag->super.super.opal_list_next;
            frag->super.super.opal_list_next->opal_list_prev = frag->super.super.opal_list_prev;
            frag->super.super.opal_list_prev->opal_list_next = frag->super.super.opal_list_next;
        }
    } else {
        /* head has range */
        mca_pml_ob1_recv_frag_t* range = frag->range;
        frag->range = NULL;
        *queue = (mca_pml_ob1_recv_frag_t*)range;
        if( range->super.super.opal_list_next == (opal_list_item_t*)range ) {
            /* the range has no next element */
            assert( range->super.super.opal_list_prev == (opal_list_item_t*)range );
            range->range = NULL;
        } else {
            range->range = (mca_pml_ob1_recv_frag_t*)range->super.super.opal_list_next;
            /* remove the range from the vertical chain */
            range->super.super.opal_list_next->opal_list_prev = range->super.super.opal_list_prev;
            range->super.super.opal_list_prev->opal_list_next = range->super.super.opal_list_next;
        }
        /* replace frag with range in the horizontal range if not the only element */
        if( frag->super.super.opal_list_next == (opal_list_item_t*)frag ) {
            range->super.super.opal_list_next = (opal_list_item_t*)range;
            range->super.super.opal_list_prev = (opal_list_item_t*)range;
        } else {
            range->super.super.opal_list_next = frag->super.super.opal_list_next;
            range->super.super.opal_list_prev = frag->super.super.opal_list_prev;
            range->super.super.opal_list_next->opal_list_prev = (opal_list_item_t*)range;
            range->super.super.opal_list_prev->opal_list_next = (opal_list_item_t*)range;
        }
    }
    frag->super.super.opal_list_next = NULL;
    frag->super.super.opal_list_prev = NULL;
    return frag;
}

/**
 * Match incoming recv_frags against posted receives.
 * Supports out of order delivery.
 *
 * @param hdr (IN)                  Header of received recv_frag.
 * @param segments (IN)             Received recv_frag descriptor.
 * @param num_segments (IN)         Flag indicating whether a match was made.
 * @param type (IN)                 Type of the message header.
 * @return                          OMPI_SUCCESS or error status on failure.
 */
static int mca_pml_ob1_recv_frag_match (mca_btl_base_module_t *btl,
                                        const mca_pml_ob1_match_hdr_t *hdr,
                                        const mca_btl_base_segment_t *segments,
                                        size_t num_segments,
                                        int type);

/**
 * Match incoming frags against posted receives. If frag is not NULL then we assume
 * it is already local and that it can be released upon completion.
 * Supports out of order delivery.
 *
 * @param comm_ptr (IN)             Communicator where the message has been received
 * @param proc (IN)                 Proc for which we have received the message.
 * @param hdr (IN)                  Header of received recv_frag.
 * @param segments (IN)             Received recv_frag descriptor.
 * @param num_segments (IN)         Flag indicating whether a match was made.
 * @param type (IN)                 Type of the message header.
 * @return                          OMPI_SUCCESS or error status on failure.
 */
static int
mca_pml_ob1_recv_frag_match_proc (mca_btl_base_module_t *btl,
                                  ompi_communicator_t *comm_ptr,
                                  mca_pml_ob1_comm_proc_t *proc,
                                  const mca_pml_ob1_match_hdr_t *hdr,
                                  const mca_btl_base_segment_t *segments,
                                  size_t num_segments,
                                  int type,
                                  mca_pml_ob1_recv_frag_t *frag);

static mca_pml_ob1_recv_request_t *match_one (mca_btl_base_module_t *btl,
                                              const mca_pml_ob1_match_hdr_t *hdr,
                                              const mca_btl_base_segment_t *segments,
                                              size_t num_segments, ompi_communicator_t *comm_ptr,
                                              mca_pml_ob1_comm_proc_t *proc,
                                              mca_pml_ob1_recv_frag_t *frag);

#if OPAL_ENABLE_FT_MPI
static inline int pml_ob1_frag_is_revoked(ompi_communicator_t* ompi_comm, mca_pml_ob1_recv_frag_t* frag) {
    mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)frag->segments->seg_addr.pval;
    return ((ompi_comm_is_revoked(ompi_comm) && !ompi_request_tag_is_ft(hdr->hdr_match.hdr_tag))
         || (ompi_comm_coll_revoked(ompi_comm) && ompi_request_tag_is_collective(hdr->hdr_match.hdr_tag)));
}

int mca_pml_ob1_revoke_comm( struct ompi_communicator_t* ompi_comm, bool coll_only )
{
    mca_pml_ob1_comm_t* comm = ompi_comm->c_pml_comm;
    mca_pml_ob1_comm_proc_t* proc;
    size_t i;
    opal_list_t nack_list;
    opal_list_item_t *it;

    /* For intercomm, also work with the local_comm */
    if( OMPI_COMM_IS_INTER(ompi_comm) ) {
        mca_pml_ob1_revoke_comm(ompi_comm->c_local_comm, coll_only);
    }

    OBJ_CONSTRUCT(&nack_list, opal_list_t);

    OPAL_THREAD_LOCK(&comm->matching_lock);
    /* these assignments need to be here because we need the matching_lock */
    ompi_comm->coll_revoked = true;
    if( !coll_only ) ompi_comm->comm_revoked = true;

#if OPAL_ENABLE_DEBUG
    int verbose = opal_output_get_verbosity(ompi_ftmpi_output_handle);
    if( verbose > 15 ) {
        mca_pml_ob1_dump(ompi_comm, verbose);
    }
#endif /* OPAL_ENABLE_DEBUG */

    /* loop over all procs in that comm */
    for (i = 0; i < comm->num_procs; i++) {
        proc = comm->procs[i];
        /* note this is not an ompi_proc, but a ob1_comm_proc, thus we don't
         * use ompi_proc_is_sentinel to verify if initialized. */
        if( NULL == proc ) continue;
        /* remove the frag from the unexpected list, add to the nack list
         * so that we can send the nack as needed to remote cancel the send
         * from outside the match lock.
         */
        opal_list_t* frags_list = &proc->unexpected_frags;
        for( it = opal_list_get_first(frags_list);
             it != opal_list_get_end(frags_list);
             it = opal_list_get_next(it) ) {
            mca_pml_ob1_recv_frag_t* frag = (mca_pml_ob1_recv_frag_t*)it;
            if( pml_ob1_frag_is_revoked(ompi_comm, frag) ) {
                it = opal_list_remove_item( frags_list, it );
                opal_list_append(&nack_list, &frag->super.super);
            }
        }
        /* same for the cantmatch queue/heap; this list is more complicated
         * Keep it simple: we pop all of the complex list, put the bad items
         * in the nack_list, and keep the good items in the keep_list;
         * then we reinsert the good items in the cantmatch heaplist */
        mca_pml_ob1_recv_frag_t* frag;
        opal_list_t keep_list;
        OBJ_CONSTRUCT(&keep_list, opal_list_t);
        while(NULL != (frag = remove_head_from_ordered_list(&proc->frags_cant_match))) {
            if( pml_ob1_frag_is_revoked(ompi_comm, frag) ) {
                opal_list_append(&nack_list, &frag->super.super);
            }
            else {
                opal_list_append(&keep_list, &frag->super.super);
            }
        }
        while( NULL != (it = opal_list_remove_first(&keep_list)) ) {
            ompi_pml_ob1_append_frag_to_ordered_list(&proc->frags_cant_match, (mca_pml_ob1_recv_frag_t*)it, proc->expected_sequence);
        }
        OBJ_DESTRUCT(&keep_list);
    }

#if OPAL_ENABLE_DEBUG
    if( opal_list_get_size(&nack_list) ) {
        OPAL_OUTPUT_VERBOSE((15, ompi_ftmpi_output_handle,
                             "ob1_revoke_comm: purging unexpected and cantmatch frags for in comm %s (%s): nacking %zu frags",
                             ompi_comm_print_cid(ompi_comm), coll_only ? "collective frags only" : "all revoked",
                             opal_list_get_size(&nack_list)));
        if( verbose > 15) mca_pml_ob1_dump(ompi_comm, verbose);
    }
#endif
    OPAL_THREAD_UNLOCK(&comm->matching_lock);
    while( NULL != (it = opal_list_remove_first(&nack_list)) ) {
        mca_pml_ob1_recv_frag_t* frag = (mca_pml_ob1_recv_frag_t*)it;
        mca_pml_ob1_hdr_t* hdr = (mca_pml_ob1_hdr_t*)frag->segments->seg_addr.pval;

        if( MCA_PML_OB1_HDR_TYPE_MATCH != hdr->hdr_common.hdr_type ) {
            assert( MCA_PML_OB1_HDR_TYPE_RGET == hdr->hdr_common.hdr_type ||
                    MCA_PML_OB1_HDR_TYPE_RNDV == hdr->hdr_common.hdr_type );
            OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle,
                                 "ob1_revoke_comm: sending NACK to %d for seq %d", hdr->hdr_rndv.hdr_match.hdr_src, hdr->hdr_rndv.hdr_match.hdr_seq));
            /* Send a ACK with a NULL request to signify revocation */
            proc = mca_pml_ob1_peer_lookup(ompi_comm, hdr->hdr_rndv.hdr_match.hdr_src);
            mca_pml_ob1_recv_request_ack_send(NULL, proc->ompi_proc, hdr->hdr_rndv.hdr_src_req.lval, NULL, 0, 0, false);
        }
        else {
            /* if it's a TYPE_MATCH, the sender is not expecting anything
             * from us. So we are done. */
            OPAL_OUTPUT_VERBOSE((15, ompi_ftmpi_output_handle,
                                 "ob1_revoke_comm: dropping silently frag from %d for seq %d", hdr->hdr_rndv.hdr_match.hdr_src, hdr->hdr_rndv.hdr_match.hdr_seq));
        }
        MCA_PML_OB1_RECV_FRAG_RETURN(frag);
    }
    OBJ_DESTRUCT(&nack_list);
    return OMPI_SUCCESS;
}
#endif /*OPAL_ENABLE_FT_MPI*/

mca_pml_ob1_recv_frag_t *ompi_pml_ob1_check_cantmatch_for_match (mca_pml_ob1_comm_proc_t *proc)
{
    mca_pml_ob1_recv_frag_t *frag = proc->frags_cant_match;

    if( (NULL != frag) && (frag->hdr.hdr_match.hdr_seq == proc->expected_sequence) ) {
        return remove_head_from_ordered_list(&proc->frags_cant_match);
    }
    return NULL;
}

void mca_pml_ob1_recv_frag_callback_match (mca_btl_base_module_t *btl,
                                           const mca_btl_base_receive_descriptor_t *descriptor)
{
    const mca_btl_base_segment_t *segments = descriptor->des_segments;
    const mca_pml_ob1_match_hdr_t *hdr = (const mca_pml_ob1_match_hdr_t *) segments->seg_addr.pval;
    ompi_communicator_t *comm_ptr;
    mca_pml_ob1_recv_request_t *match = NULL;
    mca_pml_ob1_comm_t *comm;
    mca_pml_ob1_comm_proc_t *proc;
    size_t num_segments = descriptor->des_segment_count;
    size_t bytes_received = 0;

    assert(num_segments <= MCA_BTL_DES_MAX_SEGMENTS);

    if (OPAL_UNLIKELY(segments->seg_len < OMPI_PML_OB1_MATCH_HDR_LEN)) {
        return;
    }
    ob1_hdr_ntoh(((mca_pml_ob1_hdr_t*) hdr), MCA_PML_OB1_HDR_TYPE_MATCH);

    /* communicator pointer */
    comm_ptr = ompi_comm_lookup(hdr->hdr_ctx);
    if(OPAL_UNLIKELY(NULL == comm_ptr)) {
        /* This is a special case. A message for a not yet existing
         * communicator can happens. Instead of doing a matching we
         * will temporarily add it the a pending queue in the PML.
         * Later on, when the communicator is completely instantiated,
         * this pending queue will be searched and all matching fragments
         * moved to the right communicator.
         */
        append_frag_to_list( &mca_pml_ob1.non_existing_communicator_pending, btl,
                             hdr, segments, num_segments, NULL );
        return;
    }
    comm = (mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;

    /* source sequence number */
    proc = mca_pml_ob1_peer_lookup (comm_ptr, hdr->hdr_src);

    /* We generate the MSG_ARRIVED event as soon as the PML is aware
     * of a matching fragment arrival. Independing if it is received
     * on the correct order or not. This will allow the tools to
     * figure out if the messages are not received in the correct
     * order (if multiple network interfaces).
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_ARRIVED, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "losing"
     * the fragment.
     */
    OB1_MATCHING_LOCK(&comm->matching_lock);

#if OPAL_ENABLE_FT_MPI
    if( OPAL_UNLIKELY((ompi_comm_is_revoked(comm_ptr) && !ompi_request_tag_is_ft(hdr->hdr_tag)) ||
                      (ompi_comm_coll_revoked(comm_ptr) && ompi_request_tag_is_collective(hdr->hdr_tag))) ) {
        /* if it's a TYPE_MATCH, the sender is not expecting anything from us
         * so we are done. */
        OPAL_THREAD_UNLOCK(&comm->matching_lock);
        OPAL_OUTPUT_VERBOSE((15, ompi_ftmpi_output_handle,
            "ob1_revoke_comm: dropping silently frag from %d", hdr->hdr_src));
        return;
    }
#endif

    if (!OMPI_COMM_CHECK_ASSERT_ALLOW_OVERTAKE(comm_ptr) || 0 > hdr->hdr_tag) {
        /* get sequence number of next message that can be processed.
         * If this frag is out of sequence, queue it up in the list
         * now as we still have the lock.
         */
        if(OPAL_UNLIKELY(((uint16_t) hdr->hdr_seq) != ((uint16_t) proc->expected_sequence))) {
            mca_pml_ob1_recv_frag_t* frag;
            MCA_PML_OB1_RECV_FRAG_ALLOC(frag);
            MCA_PML_OB1_RECV_FRAG_INIT(frag, hdr, segments, num_segments, btl);
            ompi_pml_ob1_append_frag_to_ordered_list(&proc->frags_cant_match, frag, proc->expected_sequence);
            SPC_RECORD(OMPI_SPC_OUT_OF_SEQUENCE, 1);
            OB1_MATCHING_UNLOCK(&comm->matching_lock);
            return;
        }

        /* We're now expecting the next sequence number. */
        proc->expected_sequence++;
    }

    /* We generate the SEARCH_POSTED_QUEUE only when the message is
     * received in the correct sequence. Otherwise, we delay the event
     * generation until we reach the correct sequence number.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_BEGIN, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    match = match_one(btl, hdr, segments, num_segments, comm_ptr, proc, NULL);

    /* The match is over. We generate the SEARCH_POSTED_Q_END here,
     * before going into check_cantmatch_for_match so we can make
     * a difference for the searching time for all messages.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_END, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    /* release matching lock before processing fragment */
    OB1_MATCHING_UNLOCK(&comm->matching_lock);

    if(OPAL_LIKELY(match)) {
        bytes_received = segments->seg_len - OMPI_PML_OB1_MATCH_HDR_LEN;
        /* We don't need to know the total amount of bytes we just received,
         * but we need to know if there is any data in this message. The
         * simplest way is to get the extra length from the first segment,
         * and then add the number of remaining segments.
         */
        match->req_recv.req_bytes_packed = bytes_received + (num_segments-1);

        MCA_PML_OB1_RECV_REQUEST_MATCHED(match, hdr);
        if(match->req_bytes_expected > 0) {
            struct iovec iov[MCA_BTL_DES_MAX_SEGMENTS];
            uint32_t iov_count = 1;

            /*
             *  Make user buffer accessible(defined) before unpacking.
             */
            MEMCHECKER(
                       memchecker_call(&opal_memchecker_base_mem_defined,
                                       match->req_recv.req_base.req_addr,
                                       match->req_recv.req_base.req_count,
                                       match->req_recv.req_base.req_datatype);
                       );

            iov[0].iov_len = bytes_received;
            iov[0].iov_base = (IOVBASE_TYPE*)((unsigned char*)segments->seg_addr.pval +
                                              OMPI_PML_OB1_MATCH_HDR_LEN);
            while (iov_count < num_segments) {
                bytes_received += segments[iov_count].seg_len;
                iov[iov_count].iov_len = segments[iov_count].seg_len;
                iov[iov_count].iov_base = (IOVBASE_TYPE*)((unsigned char*)segments[iov_count].seg_addr.pval);
                iov_count++;
            }
            opal_convertor_unpack( &match->req_recv.req_base.req_convertor,
                                   iov,
                                   &iov_count,
                                   &bytes_received );
            match->req_bytes_received = bytes_received;
            SPC_USER_OR_MPI(match->req_recv.req_base.req_ompi.req_status.MPI_TAG, (ompi_spc_value_t)bytes_received,
                            OMPI_SPC_BYTES_RECEIVED_USER, OMPI_SPC_BYTES_RECEIVED_MPI);
            /*
             *  Unpacking finished, make the user buffer unaccessible again.
             */
            MEMCHECKER(
                       memchecker_call(&opal_memchecker_base_mem_noaccess,
                                       match->req_recv.req_base.req_addr,
                                       match->req_recv.req_base.req_count,
                                       match->req_recv.req_base.req_datatype);
                       );
        }

        /* no need to check if complete we know we are. */
        /*  don't need a rmb as that is for checking */
        recv_request_pml_complete(match);
    }

    /* We matched the frag, Now see if we already have the next sequence in
     * our OOS list. If yes, try to match it.
     *
     * NOTE:
     * To optimize the number of lock used, mca_pml_ob1_recv_frag_match_proc()
     * MUST be called with communicator lock and will RELEASE the lock. This is
     * not ideal but it is better for the performance.
     */
    if(NULL != proc->frags_cant_match) {
        mca_pml_ob1_recv_frag_t* frag;

        OB1_MATCHING_LOCK(&comm->matching_lock);
        if((frag = ompi_pml_ob1_check_cantmatch_for_match(proc))) {
            /* mca_pml_ob1_recv_frag_match_proc() will release the lock. */
            mca_pml_ob1_recv_frag_match_proc(frag->btl, comm_ptr, proc,
                                             &frag->hdr.hdr_match,
                                             frag->segments, frag->num_segments,
                                             frag->hdr.hdr_match.hdr_common.hdr_type, frag);
        } else {
            OB1_MATCHING_UNLOCK(&comm->matching_lock);
        }
    }
}

/**
 * Merge all out of sequence fragments into the matching queue, as if they were received now.
 */
int mca_pml_ob1_merge_cant_match( ompi_communicator_t * ompi_comm )
{
    mca_pml_ob1_comm_t * pml_comm = (mca_pml_ob1_comm_t *)ompi_comm->c_pml_comm;
    mca_pml_ob1_recv_frag_t *frag, *frags_cant_match;
    mca_pml_ob1_comm_proc_t* proc;
    int cnt = 0;

    OB1_MATCHING_LOCK(&pml_comm->matching_lock);
    for (uint32_t i = 0; i < pml_comm->num_procs; i++) {
        if ((NULL == (proc = pml_comm->procs[i])) || (NULL != proc->frags_cant_match)) {
            continue;
        }

        /* Acquire all cant_match frags from the peer */
        frags_cant_match = proc->frags_cant_match;
        proc->frags_cant_match = NULL;
        while(NULL != (frag = remove_head_from_ordered_list(&frags_cant_match))) {
            /* mca_pml_ob1_recv_frag_match_proc() will release the lock. */
            mca_pml_ob1_recv_frag_match_proc(frag->btl, ompi_comm, proc,
                                             &frag->hdr.hdr_match,
                                             frag->segments, frag->num_segments,
                                             frag->hdr.hdr_match.hdr_common.hdr_type, frag);
            OB1_MATCHING_LOCK(&pml_comm->matching_lock);
            cnt++;
        }
    }
    OB1_MATCHING_UNLOCK(&pml_comm->matching_lock);
    return cnt;
}

void mca_pml_ob1_recv_frag_callback_rndv (mca_btl_base_module_t *btl,
                                          const mca_btl_base_receive_descriptor_t *descriptor)
{
    const mca_btl_base_segment_t *segments = descriptor->des_segments;
    const mca_pml_ob1_hdr_t *hdr = (mca_pml_ob1_hdr_t *) segments->seg_addr.pval;

    if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
        return;
    }
    ob1_hdr_ntoh((mca_pml_ob1_hdr_t*)hdr, MCA_PML_OB1_HDR_TYPE_RNDV);
    mca_pml_ob1_recv_frag_match(btl, &hdr->hdr_match, segments,
                                descriptor->des_segment_count, MCA_PML_OB1_HDR_TYPE_RNDV);
}

void mca_pml_ob1_recv_frag_callback_rget (mca_btl_base_module_t *btl,
                                          const mca_btl_base_receive_descriptor_t *descriptor)
{
    const mca_btl_base_segment_t *segments = descriptor->des_segments;
    const mca_pml_ob1_hdr_t *hdr = (mca_pml_ob1_hdr_t *) segments->seg_addr.pval;

    if( OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t)) ) {
        return;
    }
    ob1_hdr_ntoh((mca_pml_ob1_hdr_t*)hdr, MCA_PML_OB1_HDR_TYPE_RGET);
    mca_pml_ob1_recv_frag_match(btl, &hdr->hdr_match, segments,
                                descriptor->des_segment_count, MCA_PML_OB1_HDR_TYPE_RGET);
}

void mca_pml_ob1_recv_frag_callback_ack (mca_btl_base_module_t *btl,
                                         const mca_btl_base_receive_descriptor_t *descriptor)
{
    const mca_btl_base_segment_t *segments = descriptor->des_segments;
    const mca_pml_ob1_hdr_t *hdr = (mca_pml_ob1_hdr_t *) segments->seg_addr.pval;
    mca_pml_ob1_send_request_t* sendreq;
    size_t size;

    if (OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t))) {
         return;
    }

    ob1_hdr_ntoh((mca_pml_ob1_hdr_t*)hdr, MCA_PML_OB1_HDR_TYPE_ACK);
    sendreq = (mca_pml_ob1_send_request_t *) hdr->hdr_ack.hdr_src_req.pval;
    sendreq->req_recv = hdr->hdr_ack.hdr_dst_req;

#if OPAL_ENABLE_FT_MPI
    /* if the req_recv is NULL, the comm has been revoked at the receiver */
    if( OPAL_UNLIKELY(NULL == sendreq->req_recv.pval) ) {
        OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle, "Recvfrag: Received a NACK to the RDV/RGET match to %d for seq %" PRIu64 " on comm %s\n", sendreq->req_send.req_base.req_peer, sendreq->req_send.req_base.req_sequence, ompi_comm_print_cid(sendreq->req_send.req_base.req_comm)));
        if (NULL != sendreq->rdma_frag) {
            MCA_PML_OB1_RDMA_FRAG_RETURN(sendreq->rdma_frag);
            sendreq->rdma_frag = NULL;
        }
        send_request_pml_complete( sendreq );
        return;
    }
#endif /*OPAL_ENABLE_FT_MPI*/

    /* if the request should be delivered entirely by copy in/out
     * then throttle sends */
    if(hdr->hdr_common.hdr_flags & MCA_PML_OB1_HDR_FLAGS_NORDMA) {
        if (NULL != sendreq->rdma_frag) {
            MCA_PML_OB1_RDMA_FRAG_RETURN(sendreq->rdma_frag);
            sendreq->rdma_frag = NULL;
        }

        sendreq->req_throttle_sends = true;
    }

    if (hdr->hdr_ack.hdr_send_size) {
        size = hdr->hdr_ack.hdr_send_size;
    } else {
        size = sendreq->req_send.req_bytes_packed - hdr->hdr_ack.hdr_send_offset;
    }

    mca_pml_ob1_send_request_copy_in_out(sendreq, hdr->hdr_ack.hdr_send_offset, size);

    if (sendreq->req_state != 0) {
        /* Typical receipt of an ACK message causes req_state to be
         * decremented. However, a send request that started as an
         * RGET request can become a RNDV. For example, when the
         * receiver determines that its receive buffer is not
         * contiguous and therefore cannot support the RGET
         * protocol. A send request that started with the RGET
         * protocol has req_state == 0 and as such should not be
         * decremented.
         */
        OPAL_THREAD_ADD_FETCH32(&sendreq->req_state, -1);
    }

    if ((sendreq->req_send.req_base.req_convertor.flags & CONVERTOR_ACCELERATOR) &&
        (btl->btl_flags & MCA_BTL_FLAGS_ACCELERATOR_COPY_ASYNC_SEND)) {
        /* The user's buffer is GPU and this BTL can support asynchronous copies,
         * so adjust the convertor accordingly.  All the subsequent fragments will
         * use the asynchronous copy. */
        opal_accelerator_stream_t *stream = mca_pml_ob1_get_dtoh_stream();
        sendreq->req_send.req_base.req_convertor.flags |= CONVERTOR_ACCELERATOR_ASYNC;
        sendreq->req_send.req_base.req_convertor.stream = stream;
    }

    if (send_request_pml_complete_check(sendreq) == false)
        mca_pml_ob1_send_request_schedule(sendreq);
}

void mca_pml_ob1_recv_frag_callback_frag (mca_btl_base_module_t *btl,
                                          const mca_btl_base_receive_descriptor_t *descriptor)
{
    const mca_btl_base_segment_t *segments = descriptor->des_segments;
    const mca_pml_ob1_hdr_t *hdr = (mca_pml_ob1_hdr_t *) segments->seg_addr.pval;
    mca_pml_ob1_recv_request_t* recvreq;

    if (OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t))) {
        return;
    }

    ob1_hdr_ntoh((mca_pml_ob1_hdr_t*)hdr, MCA_PML_OB1_HDR_TYPE_FRAG);
    recvreq = (mca_pml_ob1_recv_request_t*)hdr->hdr_frag.hdr_dst_req.pval;
    /* If data is destined for GPU buffer and convertor was set up for asynchronous
     * copies, then start the copy and return.  The copy completion will trigger
     * the next phase. */
    if (recvreq->req_recv.req_base.req_convertor.flags & CONVERTOR_ACCELERATOR_ASYNC) {
        assert(btl->btl_flags & MCA_BTL_FLAGS_ACCELERATOR_COPY_ASYNC_RECV);

        /* This will trigger the opal_convertor_pack to start asynchronous copy. */
        mca_pml_ob1_recv_request_frag_copy_start(recvreq, btl, segments, descriptor->des_segment_count, NULL);

        /* Let BTL know that it CANNOT free the frag */
        //TODO: GB: descriptor->des_flags |= MCA_BTL_DES_FLAGS_CUDA_COPY_ASYNC;

        return;
    }

    mca_pml_ob1_recv_request_progress_frag(recvreq,btl,segments,descriptor->des_segment_count);
}


void mca_pml_ob1_recv_frag_callback_put (mca_btl_base_module_t *btl,
                                         const mca_btl_base_receive_descriptor_t *descriptor)
{
    const mca_btl_base_segment_t *segments = descriptor->des_segments;
    const mca_pml_ob1_hdr_t *hdr = (mca_pml_ob1_hdr_t *) segments->seg_addr.pval;
    mca_pml_ob1_send_request_t *sendreq;

    if (OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_common_hdr_t))) {
        return;
    }

    ob1_hdr_ntoh ((mca_pml_ob1_hdr_t*)hdr, MCA_PML_OB1_HDR_TYPE_PUT);
    sendreq = (mca_pml_ob1_send_request_t *) hdr->hdr_rdma.hdr_req.pval;
    mca_pml_ob1_send_request_put (sendreq, btl, &hdr->hdr_rdma);
}

void mca_pml_ob1_recv_frag_callback_fin(mca_btl_base_module_t *btl,
                                        const mca_btl_base_receive_descriptor_t *descriptor)
{
    const mca_btl_base_segment_t *segments = descriptor->des_segments;
    const mca_pml_ob1_fin_hdr_t *hdr = (mca_pml_ob1_fin_hdr_t *) segments->seg_addr.pval;
    mca_pml_ob1_rdma_frag_t *frag;

    if (OPAL_UNLIKELY(segments->seg_len < sizeof(mca_pml_ob1_fin_hdr_t))) {
        return;
    }

    ob1_hdr_ntoh ((union mca_pml_ob1_hdr_t *) hdr, MCA_PML_OB1_HDR_TYPE_FIN);
    frag = (mca_pml_ob1_rdma_frag_t *) hdr->hdr_frag.pval;
    frag->cbfunc (frag, hdr->hdr_size);
}



#define PML_MAX_SEQ ~((mca_pml_sequence_t)0);

static inline mca_pml_ob1_recv_request_t* get_posted_recv(opal_list_t *queue)
{
    if(opal_list_get_size(queue) == 0)
        return NULL;

    return (mca_pml_ob1_recv_request_t*)opal_list_get_first(queue);
}

static inline mca_pml_ob1_recv_request_t* get_next_posted_recv(
        opal_list_t *queue,
        mca_pml_ob1_recv_request_t* req)
{
    opal_list_item_t *i = opal_list_get_next((opal_list_item_t*)req);

    if(opal_list_get_end(queue) == i)
        return NULL;

    return (mca_pml_ob1_recv_request_t*)i;
}

static mca_pml_ob1_recv_request_t *match_incomming(const mca_pml_ob1_match_hdr_t *hdr,
                                                   mca_pml_ob1_comm_t *comm,
                                                   mca_pml_ob1_comm_proc_t *proc)
{
#if !MCA_PML_OB1_CUSTOM_MATCH
    mca_pml_ob1_recv_request_t *specific_recv, *wild_recv;
    mca_pml_sequence_t wild_recv_seq, specific_recv_seq;
    int tag = hdr->hdr_tag;

    specific_recv = get_posted_recv(&proc->specific_receives);
    wild_recv = get_posted_recv(&comm->wild_receives);

    wild_recv_seq = wild_recv ?
        wild_recv->req_recv.req_base.req_sequence : PML_MAX_SEQ;
    specific_recv_seq = specific_recv ?
        specific_recv->req_recv.req_base.req_sequence : PML_MAX_SEQ;

    /* they are equal only if both are PML_MAX_SEQ */
    while(wild_recv_seq != specific_recv_seq) {
        mca_pml_ob1_recv_request_t **match;
        opal_list_t *queue;
        int req_tag;
        mca_pml_sequence_t *seq;

        if (OPAL_UNLIKELY(wild_recv_seq < specific_recv_seq)) {
            match = &wild_recv;
            queue = &comm->wild_receives;
            seq = &wild_recv_seq;
        } else {
            match = &specific_recv;
            queue = &proc->specific_receives;
            seq = &specific_recv_seq;
        }

        req_tag = (*match)->req_recv.req_base.req_tag;
        if(req_tag == tag || (req_tag == OMPI_ANY_TAG && tag >= 0)) {
            opal_list_remove_item(queue, (opal_list_item_t*)(*match));
            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q,
                    &((*match)->req_recv.req_base), PERUSE_RECV);
            return *match;
        }

        *match = get_next_posted_recv(queue, *match);
        *seq = (*match) ? (*match)->req_recv.req_base.req_sequence : PML_MAX_SEQ;
    }

    return NULL;
#else
    return custom_match_prq_find_dequeue_verify(comm->prq, hdr->hdr_tag, hdr->hdr_src);
#endif
}

#if !MCA_PML_OB1_CUSTOM_MATCH
static mca_pml_ob1_recv_request_t *match_incomming_no_any_source (const mca_pml_ob1_match_hdr_t *hdr,
                                                                  mca_pml_ob1_comm_t *comm,
                                                                  mca_pml_ob1_comm_proc_t *proc)
{
    mca_pml_ob1_recv_request_t *recv_req;
    int tag = hdr->hdr_tag;

    OPAL_LIST_FOREACH(recv_req, &proc->specific_receives, mca_pml_ob1_recv_request_t) {
        int req_tag = recv_req->req_recv.req_base.req_tag;

        if (req_tag == tag || (req_tag == OMPI_ANY_TAG && tag >= 0)) {
            opal_list_remove_item (&proc->specific_receives, (opal_list_item_t *) recv_req);
            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_REQ_REMOVE_FROM_POSTED_Q,
                    &(recv_req->req_recv.req_base), PERUSE_RECV);
            return recv_req;
        }
    }

    return NULL;
}
#endif

static mca_pml_ob1_recv_request_t *match_one (mca_btl_base_module_t *btl,
                                              const mca_pml_ob1_match_hdr_t *hdr,
                                              const mca_btl_base_segment_t *segments,
                                              size_t num_segments, ompi_communicator_t *comm_ptr,
                                              mca_pml_ob1_comm_proc_t *proc,
                                              mca_pml_ob1_recv_frag_t* frag)
{
#if SPC_ENABLE == 1
    opal_timer_t timer = 0;
#endif
    SPC_TIMER_START(OMPI_SPC_MATCH_TIME, &timer);

    mca_pml_ob1_recv_request_t *match;
    mca_pml_ob1_comm_t *comm = (mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;

    do {
#if MCA_PML_OB1_CUSTOM_MATCH
        match = match_incomming(hdr, comm, proc);
#else
        if (!OMPI_COMM_CHECK_ASSERT_NO_ANY_SOURCE (comm_ptr)) {
            match = match_incomming(hdr, comm, proc);
        } else {
            match = match_incomming_no_any_source (hdr, comm, proc);
        }
#endif

        /* if match found, process data */
        if(OPAL_LIKELY(NULL != match)) {
            match->req_recv.req_base.req_proc = proc->ompi_proc;

            if(OPAL_UNLIKELY(MCA_PML_REQUEST_PROBE == match->req_recv.req_base.req_type)) {
                /* complete the probe */
                mca_pml_ob1_recv_request_matched_probe(match, btl, segments,
                                                       num_segments);
                /* attempt to match actual request */
                continue;
            } else if (MCA_PML_REQUEST_MPROBE == match->req_recv.req_base.req_type) {
                /* create a receive frag and associate it with the
                   request, which is then completed so that it can be
                   restarted later during mrecv */
                mca_pml_ob1_recv_frag_t *tmp;
                if(NULL == frag) {
                    MCA_PML_OB1_RECV_FRAG_ALLOC(tmp);
                    MCA_PML_OB1_RECV_FRAG_INIT(tmp, hdr, segments, num_segments, btl);
                } else {
                    tmp = frag;
                }

                match->req_recv.req_base.req_addr = tmp;
                mca_pml_ob1_recv_request_matched_probe(match, btl, segments,
                                                       num_segments);
                /* this frag is already processed, so we want to break out
                   of the loop and not end up back on the unexpected queue. */
                SPC_TIMER_STOP(OMPI_SPC_MATCH_TIME, &timer);
                return NULL;
            }

            PERUSE_TRACE_COMM_EVENT(PERUSE_COMM_MSG_MATCH_POSTED_REQ,
                                    &(match->req_recv.req_base), PERUSE_RECV);
            SPC_TIMER_STOP(OMPI_SPC_MATCH_TIME, &timer);
            return match;
        }

        /* if no match found, place on unexpected queue */
#if MCA_PML_OB1_CUSTOM_MATCH
        append_frag_to_umq(comm->umq, btl, hdr, segments,
                            num_segments, frag);
#else
        append_frag_to_list(&proc->unexpected_frags, btl, hdr, segments,
                            num_segments, frag);
#endif
        SPC_RECORD(OMPI_SPC_UNEXPECTED, 1);
        SPC_RECORD(OMPI_SPC_UNEXPECTED_IN_QUEUE, 1);
        SPC_UPDATE_WATERMARK(OMPI_SPC_MAX_UNEXPECTED_IN_QUEUE, OMPI_SPC_UNEXPECTED_IN_QUEUE);
        PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_INSERT_IN_UNEX_Q, comm_ptr,
                               hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);
        SPC_TIMER_STOP(OMPI_SPC_MATCH_TIME, &timer);
        return NULL;
    } while(true);
}

/**
 * RCS/CTS receive side matching
 *
 * @param hdr list of parameters needed for matching
 *                    This list is also embedded in frag,
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
static int mca_pml_ob1_recv_frag_match (mca_btl_base_module_t *btl,
                                        const mca_pml_ob1_match_hdr_t *hdr,
                                        const mca_btl_base_segment_t *segments,
                                        size_t num_segments,
                                        int type)
{
    /* local variables */
    uint16_t frag_msg_seq;
    uint16_t next_msg_seq_expected;
    ompi_communicator_t *comm_ptr;
    mca_pml_ob1_comm_t *comm;
    mca_pml_ob1_comm_proc_t *proc;

    /* communicator pointer */
    comm_ptr = ompi_comm_lookup(hdr->hdr_ctx);
    if(OPAL_UNLIKELY(NULL == comm_ptr)) {
        /* This is a special case. A message for a not yet existing
         * communicator can happens. Instead of doing a matching we
         * will temporarily add it the a pending queue in the PML.
         * Later on, when the communicator is completely instantiated,
         * this pending queue will be searched and all matching fragments
         * moved to the right communicator.
         */
        append_frag_to_list( &mca_pml_ob1.non_existing_communicator_pending, btl,
                             hdr, segments, num_segments, NULL );
        return OMPI_SUCCESS;
    }
    comm = (mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;

    /* source sequence number */
    proc = mca_pml_ob1_peer_lookup (comm_ptr, hdr->hdr_src);

    /* We generate the MSG_ARRIVED event as soon as the PML is aware
     * of a matching fragment arrival. Independing if it is received
     * on the correct order or not. This will allow the tools to
     * figure out if the messages are not received in the correct
     * order (if multiple network interfaces).
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_MSG_ARRIVED, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    /* get next expected message sequence number - if threaded
     * run, lock to make sure that if another thread is processing
     * a frag from the same message a match is made only once.
     * Also, this prevents other posted receives (for a pair of
     * end points) from being processed, and potentially "losing"
     * the fragment.
     */
    OB1_MATCHING_LOCK(&comm->matching_lock);

#if OPAL_ENABLE_FT_MPI
    if( OPAL_UNLIKELY((ompi_comm_is_revoked(comm_ptr) && !ompi_request_tag_is_ft(hdr->hdr_tag) )) ||
                      (ompi_comm_coll_revoked(comm_ptr) && ompi_request_tag_is_collective(hdr->hdr_tag)) ) {
        OPAL_THREAD_UNLOCK(&comm->matching_lock);
        if( MCA_PML_OB1_HDR_TYPE_MATCH != hdr->hdr_common.hdr_type ) {
            assert( MCA_PML_OB1_HDR_TYPE_RGET == hdr->hdr_common.hdr_type ||
                    MCA_PML_OB1_HDR_TYPE_RNDV == hdr->hdr_common.hdr_type );
            /* Send a ACK with a NULL request to signify revocation */
            mca_pml_ob1_rendezvous_hdr_t* hdr_rndv = (mca_pml_ob1_rendezvous_hdr_t*) hdr;
            mca_pml_ob1_recv_request_ack_send(NULL, proc->ompi_proc, hdr_rndv->hdr_src_req.lval, NULL, 0, 0, false);
            OPAL_OUTPUT_VERBOSE((2, ompi_ftmpi_output_handle, "Recvfrag: comm %d is revoked or collectives force errors, sending a NACK to the RDV/RGET match from %d for seq %d\n", hdr->hdr_ctx, hdr->hdr_src, hdr->hdr_seq));
        }
        else {
            OPAL_OUTPUT_VERBOSE((15, ompi_ftmpi_output_handle,
                "ob1_revoke_comm: dropping silently frag from %d", hdr->hdr_src));
        }
        return OMPI_SUCCESS;
    }
#endif

    /* get sequence number of next message that can be processed */
    frag_msg_seq = hdr->hdr_seq;
    next_msg_seq_expected = (uint16_t)proc->expected_sequence;

    if (!OMPI_COMM_CHECK_ASSERT_ALLOW_OVERTAKE(comm_ptr) || 0 > hdr->hdr_tag) {
        /* If the sequence number is wrong, queue it up for later. */
        if(OPAL_UNLIKELY(frag_msg_seq != next_msg_seq_expected)) {
            mca_pml_ob1_recv_frag_t* frag;
            MCA_PML_OB1_RECV_FRAG_ALLOC(frag);
            MCA_PML_OB1_RECV_FRAG_INIT(frag, hdr, segments, num_segments, btl);
            ompi_pml_ob1_append_frag_to_ordered_list(&proc->frags_cant_match, frag, next_msg_seq_expected);

            SPC_RECORD(OMPI_SPC_OUT_OF_SEQUENCE, 1);
            SPC_RECORD(OMPI_SPC_OOS_IN_QUEUE, 1);
            SPC_UPDATE_WATERMARK(OMPI_SPC_MAX_OOS_IN_QUEUE, OMPI_SPC_OOS_IN_QUEUE);

            OB1_MATCHING_UNLOCK(&comm->matching_lock);
            return OMPI_SUCCESS;
        }
    }

    /* mca_pml_ob1_recv_frag_match_proc() will release the lock. */
    return mca_pml_ob1_recv_frag_match_proc(btl, comm_ptr, proc, hdr,
                                            segments, num_segments,
                                            type, NULL);
}


/* mca_pml_ob1_recv_frag_match_proc() will match the given frag and
 * then try to match the next frag in sequence by looking into arrived
 * out of order frags in frags_cant_match list until it can't find one.
 *
 * ATTENTION: THIS FUNCTION MUST BE CALLED WITH COMMUNICATOR LOCK HELD.
 * THE LOCK WILL BE RELEASED UPON RETURN. USE WITH CARE. */
static int
mca_pml_ob1_recv_frag_match_proc (mca_btl_base_module_t *btl,
                                  ompi_communicator_t* comm_ptr,
                                  mca_pml_ob1_comm_proc_t *proc,
                                  const mca_pml_ob1_match_hdr_t *hdr,
                                  const mca_btl_base_segment_t *segments,
                                  size_t num_segments,
                                  int type,
                                  mca_pml_ob1_recv_frag_t *frag)
{
    /* local variables */
    mca_pml_ob1_comm_t *comm = (mca_pml_ob1_comm_t *)comm_ptr->c_pml_comm;
    mca_pml_ob1_recv_request_t *match = NULL;

    /* If we are here, this is the sequence number we were expecting,
     * so we can try matching it to already posted receives.
     */

 match_this_frag:
    /* We're now expecting the next sequence number. */
    /* NOTE: We should have checked for ALLOW_OVERTAKE comm flag here
     * but adding a branch in this critical path is not ideal for performance.
     * We decided to let it run the sequence number even we are not doing
     * anything with it. */
    proc->expected_sequence++;

    /* We generate the SEARCH_POSTED_QUEUE only when the message is
     * received in the correct sequence. Otherwise, we delay the event
     * generation until we reach the correct sequence number.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_BEGIN, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    match = match_one(btl, hdr, segments, num_segments, comm_ptr, proc, frag);

    /* The match is over. We generate the SEARCH_POSTED_Q_END here,
     * before going into check_cantmatch_for_match we can make a
     * difference for the searching time for all messages.
     */
    PERUSE_TRACE_MSG_EVENT(PERUSE_COMM_SEARCH_POSTED_Q_END, comm_ptr,
                           hdr->hdr_src, hdr->hdr_tag, PERUSE_RECV);

    /* release matching lock before processing fragment */
    OB1_MATCHING_UNLOCK(&comm->matching_lock);

    if(OPAL_LIKELY(match)) {
        switch(type) {
        case MCA_PML_OB1_HDR_TYPE_MATCH:
            mca_pml_ob1_recv_request_progress_match(match, btl, segments, num_segments);
            break;
        case MCA_PML_OB1_HDR_TYPE_RNDV:
            mca_pml_ob1_recv_request_progress_rndv(match, btl, segments, num_segments);
            break;
        case MCA_PML_OB1_HDR_TYPE_RGET:
            mca_pml_ob1_recv_request_progress_rget(match, btl, segments, num_segments);
            break;
        }

        if(OPAL_UNLIKELY(frag))
            MCA_PML_OB1_RECV_FRAG_RETURN(frag);
    }

    /*
     * Now that new message has arrived, check to see if
     * any fragments on the frags_cant_match list
     * may now be used to form new matches
     */
    if(OPAL_UNLIKELY(NULL != proc->frags_cant_match)) {
        OB1_MATCHING_LOCK(&comm->matching_lock);
        if((frag = ompi_pml_ob1_check_cantmatch_for_match(proc))) {
            hdr = &frag->hdr.hdr_match;
            segments = frag->segments;
            num_segments = frag->num_segments;
            btl = frag->btl;
            type = hdr->hdr_common.hdr_type;
            goto match_this_frag;
        }
        OB1_MATCHING_UNLOCK(&comm->matching_lock);
    }

    return OMPI_SUCCESS;
}

void mca_pml_ob1_handle_cid (ompi_communicator_t *comm, int src, mca_pml_ob1_cid_hdr_t *hdr_cid)
{
    mca_pml_ob1_comm_proc_t *ob1_proc = mca_pml_ob1_peer_lookup (comm, src);
    bool had_comm_index = (-1 != ob1_proc->comm_index);

    if (!had_comm_index) {
        /* avoid sending too many extra packets. if this doesn't work well then a flag can be added to
         * the proc to indicate that this packet has been sent */
        ob1_proc->comm_index = hdr_cid->hdr_src_comm_index;

        /*
         * if the proc to send to is myself,  no need to do the send
         */
        if(ob1_proc->ompi_proc != ompi_proc_local()) {
            (void) mca_pml_ob1_send_cid (ob1_proc->ompi_proc, comm);
        }
    }
}

void mca_pml_ob1_recv_frag_callback_cid (mca_btl_base_module_t* btl,
                                         const mca_btl_base_receive_descriptor_t* des)
{
    mca_btl_base_segment_t segments[MCA_BTL_DES_MAX_SEGMENTS];
    mca_pml_ob1_hdr_t *hdr = (mca_pml_ob1_hdr_t *) des->des_segments[0].seg_addr.pval;
    mca_pml_ob1_match_hdr_t *hdr_match = &hdr->hdr_ext_match.hdr_match;
    size_t num_segments = des->des_segment_count;
    ompi_communicator_t *comm;

    memcpy (segments, des->des_segments, num_segments * sizeof (segments[0]));
    assert (segments->seg_len >= sizeof (hdr->hdr_cid));

    ob1_hdr_ntoh (hdr, hdr->hdr_common.hdr_type);

    /* NTH: this should be ok as as all BTLs create a dummy segment */
    segments->seg_len -= offsetof (mca_pml_ob1_ext_match_hdr_t, hdr_match);
    segments->seg_addr.pval = (void *) hdr_match;

    /* find the communicator with this extended CID */
    comm = ompi_comm_lookup_cid (hdr->hdr_cid.hdr_cid);
    if (OPAL_UNLIKELY(NULL == comm)) {
        if (segments->seg_len > 0) {
            /* This is a special case. A message for a not yet existing
             * communicator can happens. Instead of doing a matching we
             * will temporarily add it the a pending queue in the PML.
             * Later on, when the communicator is completely instantiated,
             * this pending queue will be searched and all matching fragments
             * moved to the right communicator.
             */
            append_frag_to_list (&mca_pml_ob1.non_existing_communicator_pending,
                                 btl, (const mca_pml_ob1_match_hdr_t *)hdr, des->des_segments,
                                 num_segments, NULL);
        }

        /* nothing more to do */
        return;
    }

    mca_pml_ob1_handle_cid (comm, hdr->hdr_cid.hdr_src, &hdr->hdr_cid);
    hdr_match->hdr_ctx = comm->c_index;

    if (segments->seg_len == 0) {
        /* just a response */
        return;
    }

    mca_pml_ob1_recv_frag_match (btl, hdr_match, segments, des->des_segment_count,
                                 hdr_match->hdr_common.hdr_type);
}
