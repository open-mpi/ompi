/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Waitall = PMPI_Waitall
#endif

int MPI_Waitall(int count, MPI_Request *array_of_requests,
                MPI_Status *array_of_statuses) {
    return MPI_SUCCESS;
}
