/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Graph_map = PMPI_Graph_map
#endif

int MPI_Graph_map(MPI_Comm comm, int nnodes, int *index, int *edges,
                  int *newrank) {
    return MPI_SUCCESS;
}
