/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "class/ompi_object.h"
#include "request/request.h"
#include "include/constants.h"

/*
 * Table for Fortran <-> C Request handle conversion
 */
ompi_pointer_array_t ompi_request_f_to_c_table;

/*
 * MPI_REQUEST_NULL
 */
ompi_request_t ompi_mpi_request_null;


static void ompi_request_construct(ompi_request_t* req)
{
    OMPI_REQUEST_INIT(req);
}


static void ompi_request_destruct(ompi_request_t* req)
{
    OMPI_REQUEST_FINI(req);
}


OBJ_CLASS_INSTANCE(
    ompi_request_t,
    ompi_object_t,
    ompi_request_construct,
    ompi_request_destruct);


int ompi_request_init(void)
{
    OBJ_CONSTRUCT(&ompi_mpi_request_null, ompi_request_t);
    if (0 != ompi_pointer_array_add(&ompi_request_f_to_c_table, 
                                    MPI_REQUEST_NULL)) {
        return OMPI_ERR_REQUEST;
    }
    return OMPI_SUCCESS;
}


int ompi_request_finalize(void)
{
    OBJ_DESTRUCT(&ompi_mpi_request_null);
    return OMPI_SUCCESS;
}
