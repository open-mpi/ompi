/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_compare = PMPI_Comm_compare
#endif

int MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result) {
    return MPI_SUCCESS;
}
