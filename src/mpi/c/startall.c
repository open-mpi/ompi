/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Startall = PMPI_Startall
#endif

int MPI_Startall(int count, MPI_Request *array_of_requests) {
    return MPI_SUCCESS;
}
