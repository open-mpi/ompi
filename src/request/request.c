/*
 * $HEADER$
 */

#include "request/request.h"
#include "include/constants.h"

/*
 * Table for Fortran <-> C Request handle conversion
 */
ompi_pointer_array_t *ompi_req_f_to_c_table;
ompi_pointer_array_t ompi_req_table;

static void ompi_request_construct(ompi_request_t* req)
{
    req->req_state = OMPI_REQUEST_INVALID;
}


static void ompi_request_destruct(ompi_request_t* req)
{
    req->req_state = OMPI_REQUEST_INVALID;
}


OBJ_CLASS_INSTANCE(
    ompi_request_t,
    ompi_object_t,
    ompi_request_construct,
    ompi_request_destruct);


