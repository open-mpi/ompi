/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Grequest_complete = PMPI_Grequest_complete
#endif

int MPI_Grequest_complete(MPI_Request request) {
    return MPI_SUCCESS;
}
