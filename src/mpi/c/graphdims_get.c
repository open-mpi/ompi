/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Graphdims_get = PMPI_Graphdims_get
#endif

int MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges) {
    return MPI_SUCCESS;
}
