/*
 * Copyright (c) 2011      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OSC_SM_REQUEST_H
#define OSC_SM_REQUEST_H

#include "ompi/request/request.h"

struct ompi_osc_sm_request_t {
    ompi_request_t super;
};
typedef struct ompi_osc_sm_request_t ompi_osc_sm_request_t;

OBJ_CLASS_DECLARATION(ompi_osc_sm_request_t);

/* REQUEST_ALLOC is only called from "top-level" functions (sm_rput,
   sm_rget, etc.), so it's ok to spin here... */ 
#define OMPI_OSC_SM_REQUEST_ALLOC(win, req)                             \
    do {                                                                \
        ompi_free_list_item_t *item = NULL;                             \
        do {                                                            \
            OMPI_FREE_LIST_GET_MT(&mca_osc_sm_component.requests, item); \
            if (NULL == item) {                                         \
                opal_progress();                                        \
            }                                                           \
        } while (NULL == item);                                         \
        req = (ompi_osc_sm_request_t*) item;                            \
        OMPI_REQUEST_INIT(&req->super, false);                          \
        req->super.req_mpi_object.win = win;                            \
        req->super.req_complete = false;                                \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                     \
        req->super.req_status._ucount = 0;                              \
    } while (0)

#define OMPI_OSC_SM_REQUEST_RETURN(req)                                 \
    do {                                                                \
        OMPI_REQUEST_FINI(&request->super);                             \
        OMPI_FREE_LIST_RETURN_MT(&mca_osc_sm_component.requests,        \
                              (ompi_free_list_item_t*) req);            \
    } while (0)

#define OMPI_OSC_SM_REQUEST_COMPLETE(req)                       \
    do {                                                        \
        OPAL_THREAD_LOCK(&ompi_request_lock);                   \
        ompi_request_complete(&req->super, true);               \
        OPAL_THREAD_UNLOCK(&ompi_request_lock);                 \
    } while (0)

#endif
