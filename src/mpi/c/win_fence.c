/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_fence = PMPI_Win_fence
#endif

int MPI_Win_fence(int assert, MPI_Win win) {
    return MPI_SUCCESS;
}
