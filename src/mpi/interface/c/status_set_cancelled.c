/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Status_set_cancelled = PMPI_Status_set_cancelled
#endif

int MPI_Status_set_cancelled(MPI_Status *status, int flag) {
    return MPI_SUCCESS;
}
