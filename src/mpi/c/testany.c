/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Testany = PMPI_Testany
#endif

int MPI_Testany(int count, MPI_Request array_of_requests[], int *index,
                MPI_Status *status) {
    return MPI_SUCCESS;
}
