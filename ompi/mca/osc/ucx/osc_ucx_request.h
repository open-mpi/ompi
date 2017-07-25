/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (C) Mellanox Technologies Ltd. 2001-2017. ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_OSC_UCX_REQUEST_H
#define OMPI_OSC_UCX_REQUEST_H

#include "ompi/request/request.h"

typedef struct ompi_osc_ucx_request {
    ompi_request_t super;
} ompi_osc_ucx_request_t;

OBJ_CLASS_DECLARATION(ompi_osc_ucx_request_t);

typedef struct ompi_osc_ucx_internal_request {
    ompi_osc_ucx_request_t *external_req;
} ompi_osc_ucx_internal_request_t;

#define OMPI_OSC_UCX_REQUEST_ALLOC(win, req)                            \
    do {                                                                \
        opal_free_list_item_t *item;                                    \
        do {                                                            \
            item = opal_free_list_get(&mca_osc_ucx_component.requests); \
            if (item == NULL) {                                         \
                if (mca_osc_ucx_component.ucp_worker != NULL &&         \
                    mca_osc_ucx_component.num_incomplete_req_ops > 0) { \
                    ucp_worker_progress(mca_osc_ucx_component.ucp_worker); \
                }                                                       \
            }                                                           \
        } while (item == NULL);                                         \
        req = (ompi_osc_ucx_request_t*) item;                           \
        OMPI_REQUEST_INIT(&req->super, false);                          \
        req->super.req_mpi_object.win = win;                            \
        req->super.req_complete = false;                                \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                     \
        req->super.req_status.MPI_ERROR = MPI_SUCCESS;                  \
    } while (0)

#define OMPI_OSC_UCX_REQUEST_RETURN(req)                                \
    do {                                                                \
        OMPI_REQUEST_FINI(&request->super);                             \
        opal_free_list_return (&mca_osc_ucx_component.requests,         \
                               (opal_free_list_item_t*) req);           \
    } while (0)

#endif /* OMPI_OSC_UCX_REQUEST_H */
