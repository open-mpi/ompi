/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OMPI_OSC_RDMA_REQUEST_H
#define OMPI_OSC_RDMA_REQUEST_H

#include "osc_rdma.h"

#include "ompi/request/request.h"
#include "opal/util/output.h"

struct ompi_osc_rdma_request_t {
    ompi_request_t super;

    int type;
    void *origin_addr;
    int origin_count;
    struct ompi_datatype_t *origin_dt;
    ompi_osc_rdma_module_t* module;
    int outstanding_requests;
    bool internal;
};
typedef struct ompi_osc_rdma_request_t ompi_osc_rdma_request_t;
OBJ_CLASS_DECLARATION(ompi_osc_rdma_request_t);

/* REQUEST_ALLOC is only called from "top-level" functions (rdma_rput,
   rdma_rget, etc.), so it's ok to spin here... */ 
#define OMPI_OSC_RDMA_REQUEST_ALLOC(win, req)                           \
    do {                                                                \
        ompi_free_list_item_t *item;                                    \
        do {                                                            \
            OMPI_FREE_LIST_GET_MT(&mca_osc_rdma_component.requests, item); \
            if (NULL == item) {                                         \
                opal_progress();                                        \
            }                                                           \
        } while (NULL == item);                                         \
        req = (ompi_osc_rdma_request_t*) item;                          \
        OMPI_REQUEST_INIT(&req->super, false);                          \
        req->super.req_mpi_object.win = win;                            \
        req->super.req_complete = false;                                \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                     \
        req->module = GET_MODULE(win);                                  \
        req->internal = false;                                          \
    } while (0)

#define OMPI_OSC_RDMA_REQUEST_RETURN(req)                               \
    do {                                                                \
      OMPI_REQUEST_FINI(&(req)->super);					\
      OMPI_FREE_LIST_RETURN_MT(&mca_osc_rdma_component.requests,	\
			       (ompi_free_list_item_t *) (req));	\
    } while (0)

static inline void ompi_osc_rdma_request_complete (ompi_osc_rdma_request_t *request, int mpi_error)
{
    if (!request->internal) {
        request->super.req_status.MPI_ERROR = mpi_error;

        /* mark the request complete at the mpi level */
        ompi_request_complete (&request->super, true);
    } else {
        OMPI_OSC_RDMA_REQUEST_RETURN (request);
    }
}

#endif /* OMPI_OSC_RDMA_REQUEST_H */
