/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "request/request.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Request_f2c = PMPI_Request_f2c
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Request_f2c";


OMPI_EXPORT
MPI_Request MPI_Request_f2c(MPI_Fint request) 
{
    size_t request_index;

    request_index = (size_t) request;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    /* Per MPI-2:4.12.4, do not invoke an error handler if we get an
       invalid fortran handle */
    
    if (request_index < 0 || 
        request_index >= 
        ompi_pointer_array_get_size(ompi_req_f_to_c_table)) {
        return MPI_REQUEST_NULL;
    }

    return ompi_pointer_array_get_item(ompi_req_f_to_c_table, request_index);
}
