/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Topo_test = PMPI_Topo_test
#endif

int MPI_Topo_test(MPI_Comm comm, int *status) {
    return MPI_SUCCESS;
}
