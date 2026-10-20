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
 * The callback has to be installed while the request is still inactive.  A
 * started request can complete on another thread before the installation is
 * visible to it, and the callback is then never run at all: the completion
 * found nothing to call, and the installation found nothing to complete.
 * Creating the request inactive and starting it afterwards removes the
 * window rather than racing in it.
 *
 * The request is left persistent, so the callback owns it and must free it,
 * which is what the ADAPT callbacks already do.
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

