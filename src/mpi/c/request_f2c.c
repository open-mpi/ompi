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

MPI_Request MPI_Request_f2c(MPI_Fint request) {
    /* local variables */
    ompi_request_t *request_c;
    size_t request_index;

    request_index = (size_t) request;

    /* Error checks */
    if (MPI_PARAM_CHECK) {
        if (0 > request_index) {
           (void)OMPI_ERRHANDLER_INVOKE( MPI_COMM_WORLD, MPI_ERR_REQUEST,
                                          "MPI_Request_f2c");
        }
    }
    if (request_index >= ompi_req_f_to_c_table->size) {
            (void) OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_REQUEST,
                                         "MPI_Request_f2c - II");
            return MPI_REQUEST_NULL;
    }

    request_c = ompi_req_f_to_c_table->addr[request_index];

    return (MPI_Request) request_c;
}
