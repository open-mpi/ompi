/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Test_cancelled = PMPI_Test_cancelled
#endif

int MPI_Test_cancelled(MPI_Status *status, int *flag) {
    return MPI_SUCCESS;
}
