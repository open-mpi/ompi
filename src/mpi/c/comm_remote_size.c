/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_remote_size = PMPI_Comm_remote_size
#endif


int MPI_Comm_remote_size(MPI_Comm comm, int *size) {
    return MPI_SUCCESS;
}
