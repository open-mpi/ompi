/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Barrier = PMPI_Barrier
#endif

int MPI_Barrier(MPI_Comm comm) {
    return MPI_SUCCESS;
}
