/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Waitany = PMPI_Waitany
#endif

int MPI_Waitany(int count, MPI_Request *array_of_requests,
		        int *index, MPI_Status *status) {
    return MPI_SUCCESS;
}
