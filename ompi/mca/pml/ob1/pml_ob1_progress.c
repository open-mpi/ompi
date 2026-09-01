/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "pml_ob1.h"
#include "pml_ob1_sendreq.h"
#include "pml_ob1_accelerator.h"
#include "ompi/mca/bml/base/base.h"
#include "pml_ob1_recvreq.h"
#include "pml_ob1_recvfrag.h"
#include "opal/runtime/opal_params.h"

/**
 * Return the number of completed events allowing the upper level
 * to know when no pending events are expected so that it can
 * unregister the progress function.
 */
static inline int mca_pml_ob1_process_pending_accelerator_async_copies(void)
{
    mca_btl_base_descriptor_t *frag;
    int progress, count = 0;

    do {
        progress = mca_pml_ob1_progress_one_htod_event(&frag);
        if (1 == progress) {
            /* Call the finish function to make progress. */
            mca_pml_ob1_recv_request_frag_copy_finished(NULL, NULL, frag, 0);
            count++;
        }
    } while (progress > 0);
    /* Consider progressing dtoh events here in future */

    return count;
}

static opal_atomic_int32_t mca_pml_ob1_progress_needed = 0;

void mca_pml_ob1_enable_progress(int32_t count)
{
    int32_t needed = OPAL_ATOMIC_ADD_FETCH32(&mca_pml_ob1_progress_needed, count);

    if( 0 < count ) {
        if( count == needed ) {  /* it was zero: ours to turn on */
            opal_progress_register(mca_pml_ob1_progress);
        }
    } else if( 0 == needed ) {
        opal_progress_unregister(mca_pml_ob1_progress);
        /* The count reaching zero and polling going away are two steps,
         * and a park landing between them takes the first branch above,
         * sees a count it did not raise from zero, and leaves polling to
         * somebody else -- who is this thread, on its way to taking it
         * away. Nothing would repair that later, since every park after
         * it draws the same conclusion for the same reason, so parked
         * work would never be re-driven again. Hence the second look: a
         * debt that is back is one nothing else will come back for.
         * Registering twice is free, opal_progress_register() returning
         * early for a callback already in the array.
         *
         * The mirror of it survives -- our register above landing after
         * another thread's unregister, leaving the callback polling for
         * a debt of zero -- and is left alone deliberately. It costs one
         * needless callback, not forward progress, and the next debt
         * taken and paid puts it right. */
        if( 0 < mca_pml_ob1_progress_needed ) {
            opal_progress_register(mca_pml_ob1_progress);
        }
    }
}

int mca_pml_ob1_progress(void)
{
    int i, queue_length = opal_list_get_size(&mca_pml_ob1.send_pending);
    int j, completed_requests = 0;
    bool send_succeeded;

    completed_requests += mca_pml_ob1_process_pending_accelerator_async_copies();

    /* Work parked on a peer that could not be reached yet: sends that never
     * started, and fragments from a peer we cannot convert from. Neither
     * has anything else to bring it back -- no completion is outstanding,
     * because nothing was ever sent -- so each park takes a count here
     * (mca_pml_ob1_enable_progress()) and gives it back by being counted
     * as done below. Asking about a peer is also what fetches its data
     * where peers are fetched one at a time, so this is the retry as well
     * as the drain. */
    completed_requests += mca_pml_ob1_drain_staged_sends();
    completed_requests += mca_pml_ob1_drain_unseeded_frags();

    /* Drain the FIN/ACK control-packet retry queue. It is otherwise drained
     * only as a side effect of BTL completion callbacks (see
     * MCA_PML_OB1_PROGRESS_PENDING). If the BTL goes idle while packets are
     * still queued -- e.g. the tail of an incast where btl_sendi() repeatedly
     * returned OPAL_ERR_OUT_OF_RESOURCE -- no further completion fires, the
     * queue is never revisited, and every peer waiting on those FINs hangs
     * forever. Retrying it here, driven by mca_pml_ob1_progress_needed (which
     * mca_pml_ob1_add_to_pending() bumps via mca_pml_ob1_enable_progress()),
     * guarantees the queue makes progress even with no BTL traffic in flight. */
    if( opal_list_get_size(&mca_pml_ob1.pckt_pending) ) {
        int pckt_before = (int) opal_list_get_size(&mca_pml_ob1.pckt_pending);
        mca_pml_ob1_process_pending_packets(NULL);
        completed_requests += pckt_before - (int) opal_list_get_size(&mca_pml_ob1.pckt_pending);
    }

    for( i = 0; i < queue_length; i++ ) {
        mca_pml_ob1_send_pending_t pending_type = MCA_PML_OB1_SEND_PENDING_NONE;
        mca_pml_ob1_send_request_t* sendreq;
        mca_bml_base_endpoint_t* endpoint;

        sendreq = get_request_from_send_pending(&pending_type);
        if(OPAL_UNLIKELY(NULL == sendreq))
            break;

        switch(pending_type) {
        case MCA_PML_OB1_SEND_PENDING_NONE:
            assert(0);
            return 0;
        case MCA_PML_OB1_SEND_PENDING_SCHEDULE:
            if( mca_pml_ob1_send_request_schedule_exclusive(sendreq) ==
                OMPI_ERR_OUT_OF_RESOURCE ) {
                return 0;
            }
            completed_requests++;
            break;
        case MCA_PML_OB1_SEND_PENDING_START:
            MCA_PML_OB1_SEND_REQUEST_RESET(sendreq);
            endpoint = sendreq->req_endpoint;
            send_succeeded = false;
            for(j = 0; j < (int)mca_bml_base_btl_array_get_size(&endpoint->btl_eager); j++) {
                mca_bml_base_btl_t* bml_btl;
                int rc;

                /* select a btl */
                bml_btl = mca_bml_base_btl_array_get_next(&endpoint->btl_eager);
                rc = mca_pml_ob1_send_request_start_btl(sendreq, bml_btl);
                if( OPAL_LIKELY(OMPI_SUCCESS == rc) ) {
                    send_succeeded = true;
                    completed_requests++;
                    break;
                }
            }
            if( false == send_succeeded ) {
                add_request_to_send_pending(sendreq, MCA_PML_OB1_SEND_PENDING_START, true);
            }
        }
    }

    if( 0 != completed_requests ) {
        mca_pml_ob1_enable_progress(-completed_requests);
    }

    return completed_requests;
}
