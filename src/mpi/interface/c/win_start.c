/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_start = PMPI_Win_start
#endif

int MPI_Win_start(MPI_Group group, int assert, MPI_Win win) {
    return MPI_SUCCESS;
}
