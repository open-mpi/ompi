/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_unlock = PMPI_Win_unlock
#endif

int MPI_Win_unlock(int rank, MPI_Win win) {
    return MPI_SUCCESS;
}
