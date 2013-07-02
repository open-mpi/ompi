/*
 * Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/request/request.h"

#include "coll_portals4.h"
#include "coll_portals4_request.h"

static int
request_cancel(struct ompi_request_t *request, int complete)
{
    return MPI_ERR_REQUEST;
}

static int
request_free(struct ompi_request_t **ompi_req)
{
    ompi_coll_portals4_request_t *request = 
        (ompi_coll_portals4_request_t*) *ompi_req;

    if (true != request->super.req_complete) {
        return MPI_ERR_REQUEST;
    }

    OMPI_COLL_PORTALS4_REQUEST_RETURN(request);

    *ompi_req = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}

static
void
request_construct(ompi_coll_portals4_request_t *request)
{
    request->super.req_type = OMPI_REQUEST_WIN;
    request->super.req_status._cancelled = 0;
    request->super.req_free = request_free;
    request->super.req_cancel = request_cancel;
    request->ct_h = PTL_INVALID_HANDLE;
    request->me_h = PTL_INVALID_HANDLE;
}

OBJ_CLASS_INSTANCE(ompi_coll_portals4_request_t,
                   ompi_request_t,
                   request_construct,
                   NULL);
