/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA CORPORATION. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef MCA_COLL_ADAPT_ALGORITHMS_H
#define MCA_COLL_ADAPT_ALGORITHMS_H

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_topo.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"
#include <math.h>

/* Send-side state for one peer: a child in bcast, the parent in reduce. */
typedef struct ompi_coll_adapt_peer_send_s {
    /* Sends posted to this peer that have not completed yet */
    int ongoing;
    /* Standard sends owed before the next synchronous one; starts at zero so
     * the first send to a peer is synchronous */
    int until_sync;
} ompi_coll_adapt_peer_send_t;

/*
 * Pick the send mode for the next segment going to a peer.
 *
 * An eager standard send completes once the BTL takes the data, so credits
 * alone bound only local buffer use and let a fast sender turn the pipeline
 * into unexpected messages.  A synchronous send completes on peer match, so
 * one per credit window bounds how far ahead of the peer we run.
 *
 * The interval must not be negative; zero makes every send synchronous.  A
 * lost update to the countdown only shifts a checkpoint by a segment, so it
 * needs no atomics.
 */
static inline mca_pml_base_send_mode_t
ompi_coll_adapt_peer_send_mode(ompi_coll_adapt_peer_send_t *peer, int interval)
{
    if (0 == peer->until_sync) {
        peer->until_sync = interval;
        return MCA_PML_BASE_SEND_SYNCHRONOUS;
    }
    --(peer->until_sync);
    return MCA_PML_BASE_SEND_STANDARD;
}

/*
 * Install the completion callback of a request that has not been started.
 *
 * Nothing can complete an inactive request, so a plain store is enough and
 * the callback is certain to run once.  ompi_request_set_callback() is not
 * usable here: it runs the callback when the request is already complete,
 * and an inactive persistent request reports itself complete, so it would
 * call back before the operation had begun.
 */
static inline void ompi_coll_adapt_request_set_callback(ompi_request_t *request,
                                                        ompi_request_complete_fn_t cb,
                                                        void *cb_data)
{
    assert(OMPI_REQUEST_INACTIVE == request->req_state);
    request->req_complete_cb_data = cb_data;
    request->req_complete_cb = cb;
}

/*
 * Post a point-to-point operation with its completion callback already in
 * place.
 *
 * The callback must be installed while the request is inactive: a started
 * request can complete before the installation is visible, and the callback
 * then never runs at all.
 *
 * The request stays persistent, so the callback owns it and must free it.
 */
static inline int ompi_coll_adapt_isend_with_cb(const void *buf, size_t count,
                                                ompi_datatype_t *datatype, int dst, int tag,
                                                mca_pml_base_send_mode_t sendmode,
                                                ompi_communicator_t *comm,
                                                ompi_request_complete_fn_t cb, void *cb_data,
                                                ompi_request_t **request)
{
    int err = MCA_PML_CALL(isend_init(buf, count, datatype, dst, tag, sendmode,
                                      comm, request));
    if (OMPI_SUCCESS != err) {
        return err;
    }
    ompi_coll_adapt_request_set_callback(*request, cb, cb_data);

    err = MCA_PML_CALL(start(1, request));
    if (OMPI_SUCCESS != err) {
        (void) (*request)->req_free(request);
    }
    return err;
}

static inline int ompi_coll_adapt_irecv_with_cb(void *buf, size_t count,
                                                ompi_datatype_t *datatype, int src, int tag,
                                                ompi_communicator_t *comm,
                                                ompi_request_complete_fn_t cb, void *cb_data,
                                                ompi_request_t **request)
{
    int err = MCA_PML_CALL(irecv_init(buf, count, datatype, src, tag, comm, request));
    if (OMPI_SUCCESS != err) {
        return err;
    }
    ompi_coll_adapt_request_set_callback(*request, cb, cb_data);

    err = MCA_PML_CALL(start(1, request));
    if (OMPI_SUCCESS != err) {
        (void) (*request)->req_free(request);
    }
    return err;
}

/* Bcast */
int ompi_coll_adapt_ibcast_register(void);
int ompi_coll_adapt_ibcast_fini(void);
int ompi_coll_adapt_bcast(BCAST_ARGS);
int ompi_coll_adapt_ibcast(IBCAST_ARGS);

/* Reduce */
int ompi_coll_adapt_ireduce_register(void);
int ompi_coll_adapt_ireduce_fini(void);
int ompi_coll_adapt_reduce(REDUCE_ARGS);
int ompi_coll_adapt_ireduce(IREDUCE_ARGS);

#endif /* MCA_COLL_ADAPT_ALGORITHMS_H */

