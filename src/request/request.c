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
   int ret_val;

    /* assign entry in fortran <-> c translation array */
    ret_val = ompi_pointer_array_add(ompi_req_f_to_c_table, req);
    req->req_f_to_c_index = ret_val;

    req->req_state = OMPI_REQUEST_INVALID;
}


static void ompi_request_destruct(ompi_request_t* req)
{
    /* reset the ompi_req_f_to_c_table entry - make sure that the
     * entry is in the table */
    if (NULL != ompi_pointer_array_get_item(ompi_req_f_to_c_table,
                                            req->req_f_to_c_index)) {
        ompi_pointer_array_set_item(ompi_req_f_to_c_table,
                                    req->req_f_to_c_index, NULL);
    }

    req->req_state = OMPI_REQUEST_INVALID;
}


OBJ_CLASS_INSTANCE(
    ompi_request_t,
    ompi_object_t,
    ompi_request_construct,
    ompi_request_destruct);


