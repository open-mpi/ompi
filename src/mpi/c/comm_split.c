/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_split = PMPI_Comm_split
#endif

int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) {
    return MPI_SUCCESS;
}
