/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Graph_create = PMPI_Graph_create
#endif

int MPI_Graph_create(MPI_Comm comm_old, int nnodes, int *index,
                     int *edges, int reorder, MPI_Comm *comm_graph) {
    return MPI_SUCCESS;
}
