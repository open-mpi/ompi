/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Test = PMPI_Test
#endif

int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status) {
    return MPI_SUCCESS;
}
