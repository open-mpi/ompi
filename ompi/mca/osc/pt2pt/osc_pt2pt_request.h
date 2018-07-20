/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2014-2016 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_OSC_PT2PT_REQUEST_H
#define OMPI_OSC_PT2PT_REQUEST_H

#include "osc_pt2pt.h"

#include "ompi/request/request.h"
#include "opal/util/output.h"

#ifndef container_of
#define container_of(ptr, type, member) ( \
        (type *)( ((char *)(ptr)) - offsetof(type,member) ))
#endif

struct ompi_osc_pt2pt_request_t {
    ompi_request_t super;

    int type;
    const void *origin_addr;
    int origin_count;
    struct ompi_datatype_t *origin_dt;
    ompi_osc_pt2pt_module_t* module;
    int32_t outstanding_requests;
    bool internal;
};
typedef struct ompi_osc_pt2pt_request_t ompi_osc_pt2pt_request_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_request_t);

struct ompi_osc_pt2pt_completed_request_t {
    opal_free_list_item_t free_super;
    opal_list_item_t super;
    ompi_request_t *request;
};
typedef struct ompi_osc_pt2pt_completed_request_t ompi_osc_pt2pt_completed_request_t;
OBJ_CLASS_DECLARATION(ompi_osc_pt2pt_completed_request_t);

/* REQUEST_ALLOC is only called from "top-level" functions (pt2pt_rput,
   pt2pt_rget, etc.), so it's ok to spin here... */
#define OMPI_OSC_PT2PT_REQUEST_ALLOC(win, req)                          \
    do {                                                                \
        opal_free_list_item_t *item;                                    \
        do {                                                            \
            item = opal_free_list_get (&mca_osc_pt2pt_component.requests); \
            if (NULL == item) {                                         \
                opal_progress();                                        \
            }                                                           \
        } while (NULL == item);                                         \
        req = (ompi_osc_pt2pt_request_t*) item;                         \
        OMPI_REQUEST_INIT(&req->super, false);                          \
        req->super.req_mpi_object.win = win;                            \
        req->super.req_complete = false;                                \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                     \
        req->module = GET_MODULE(win);                                  \
        req->internal = false;                                          \
    } while (0)

#define OMPI_OSC_PT2PT_REQUEST_RETURN(req)                              \
    do {                                                                \
        OMPI_REQUEST_FINI(&(req)->super);                               \
        (req)->outstanding_requests = 0;                                \
        opal_free_list_return (&mca_osc_pt2pt_component.requests,       \
                                 (opal_free_list_item_t *) (req));      \
    } while (0)

static inline void ompi_osc_pt2pt_request_complete (ompi_osc_pt2pt_request_t *request, int mpi_error)
{
    if (!request->internal) {
        request->super.req_status.MPI_ERROR = mpi_error;

        /* mark the request complete at the mpi level */
        ompi_request_complete(&request->super, true);
    } else {
        OMPI_OSC_PT2PT_REQUEST_RETURN (request);
    }
}

static inline void ompi_osc_pt2pt_request_schedule(ompi_request_t *request)
{
    opal_free_list_item_t *item;
    ompi_osc_pt2pt_completed_request_t *c_req;

    OPAL_THREAD_LOCK(&mca_osc_pt2pt_component.lock);
    item = opal_free_list_get(&mca_osc_pt2pt_component.completed_requests);
    if (item) {
        c_req          = container_of(item, ompi_osc_pt2pt_completed_request_t, free_super);
        c_req->request = request;

        opal_list_append(&mca_osc_pt2pt_component.completed_requests_list, &c_req->super);
    } else {
        /* failed to create entry - then try to remove request directly */
        ompi_request_free(&request);
    }
    OPAL_THREAD_UNLOCK(&mca_osc_pt2pt_component.lock);
}

#endif /* OMPI_OSC_PT2PT_REQUEST_H */
