/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Request_get_status = PMPI_Request_get_status
#endif

int MPI_Request_get_status(MPI_Request request, int *flag,
                           MPI_Status *status) {
    return MPI_SUCCESS;
}
