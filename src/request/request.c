/*
 * $HEADER$
 */

#include "request/request.h"


static void ompi_request_construct(ompi_request_t* req)
{
    req->req_mode = OMPI_REQUEST_INVALID;
}


static void ompi_request_destruct(ompi_request_t* req)
{
}


OBJ_CLASS_INSTANCE(
    ompi_request_t,
    ompi_object_t,
    ompi_request_construct,
    ompi_request_destruct);


