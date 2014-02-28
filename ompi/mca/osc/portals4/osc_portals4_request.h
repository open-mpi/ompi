/*
 * Copyright (c) 2011-2013 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OSC_PORTALS4_REQUEST_H
#define OSC_PORTALS4_REQUEST_H

#include "ompi/request/request.h"

struct ompi_osc_portals4_request_t {
    ompi_request_t super;
    int32_t ops_expected;
    volatile int32_t ops_committed;
};
typedef struct ompi_osc_portals4_request_t ompi_osc_portals4_request_t;

OBJ_CLASS_DECLARATION(ompi_osc_portals4_request_t);

#define OMPI_OSC_PORTALS4_REQUEST_ALLOC(win, req)                   \
    do {                                                                \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_WAIT_MT(&mca_osc_portals4_component.requests,    \
                               item);                               \
        req = (ompi_osc_portals4_request_t*) item;                      \
        OMPI_REQUEST_INIT(&req->super, false);                          \
        req->super.req_mpi_object.win = win;                            \
        req->super.req_complete = false;                                \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                     \
        req->ops_expected = 0;                                          \
        req->ops_committed = 0;                                         \
    } while (0)

#define OMPI_OSC_PORTALS4_REQUEST_RETURN(req)                           \
    do {                                                                \
        OMPI_REQUEST_FINI(&request->super);                             \
        OMPI_FREE_LIST_RETURN_MT(&mca_osc_portals4_component.requests,     \
                              (ompi_free_list_item_t*) req);            \
    } while (0)


#endif
