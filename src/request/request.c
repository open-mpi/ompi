/*
 * $HEADER$
 */

#include "request/request.h"

ompi_class_t ompi_request_t_class = { 
    "ompi_request_t", 
    OBJ_CLASS(ompi_object_t),
    (ompi_construct_t) ompi_request_construct,
    (ompi_destruct_t) ompi_request_destruct,
};


void ompi_request_construct(ompi_request_t* req)
{
    req->req_mode = OMPI_REQUEST_INVALID;
}


void ompi_request_destruct(ompi_request_t* req)
{
}


