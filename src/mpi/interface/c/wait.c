/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Wait = PMPI_Wait
#endif

int MPI_Wait(MPI_Request *request, MPI_Status *status) {
    return MPI_SUCCESS;
}
