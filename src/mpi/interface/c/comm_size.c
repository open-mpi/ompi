/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_size = PMPI_Comm_size
#endif

int MPI_Comm_size(MPI_Comm comm, int *size) {
    return MPI_SUCCESS;
}
