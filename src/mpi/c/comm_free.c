/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_free = PMPI_Comm_free
#endif

int MPI_Comm_free(MPI_Comm *comm) {
    return MPI_SUCCESS;
}
