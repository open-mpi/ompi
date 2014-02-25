/*
 * Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef COLL_PORTALS4_REQUEST_H
#define COLL_PORTALS4_REQUEST_H

#include "ompi/request/request.h"

enum ompi_coll_portals4_request_type_t {
    OMPI_COLL_PORTALS4_TYPE_BARRIER,
};
typedef enum ompi_coll_portals4_request_type_t ompi_coll_portals4_request_type_t;

struct ompi_coll_portals4_request_t {
    ompi_request_t super;
    ompi_coll_portals4_request_type_t type;
    ptl_handle_ct_t ct_h;
    ptl_handle_me_t me_h;
};
typedef struct ompi_coll_portals4_request_t ompi_coll_portals4_request_t;

OBJ_CLASS_DECLARATION(ompi_coll_portals4_request_t);

#define OMPI_COLL_PORTALS4_REQUEST_ALLOC(comm, req)                     \
    do {                                                                \
        ompi_free_list_item_t *item;                                    \
        OMPI_FREE_LIST_GET_MT(&mca_coll_portals4_component.requests,       \
                           item);                                       \
        req = (ompi_coll_portals4_request_t*) item;                     \
        OMPI_REQUEST_INIT(&req->super, false);                          \
        req->super.req_mpi_object.comm = comm;                          \
        req->super.req_complete = false;                                \
        req->super.req_state = OMPI_REQUEST_ACTIVE;                     \
    } while (0)

#define OMPI_COLL_PORTALS4_REQUEST_RETURN(req)                          \
    do {                                                                \
        OMPI_REQUEST_FINI(&request->super);                             \
        OMPI_FREE_LIST_RETURN_MT(&mca_coll_portals4_component.requests,    \
                              (ompi_free_list_item_t*) req);            \
    } while (0)


#endif
