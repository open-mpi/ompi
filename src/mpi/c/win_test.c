/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_test = PMPI_Win_test
#endif

int MPI_Win_test(MPI_Win win, int *flag) {
    return MPI_SUCCESS;
}
