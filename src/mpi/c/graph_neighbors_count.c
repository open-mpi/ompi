/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Graph_neighbors_count = PMPI_Graph_neighbors_count
#endif

int MPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors) {
    return MPI_SUCCESS;
}
