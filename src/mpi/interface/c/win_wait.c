/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_wait = PMPI_Win_wait
#endif

int MPI_Win_wait(MPI_Win win) {
    return MPI_SUCCESS;
}
