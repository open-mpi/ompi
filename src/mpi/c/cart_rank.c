/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_rank = PMPI_Cart_rank
#endif


int MPI_Cart_rank(MPI_Comm comm, int *coords, int *rank) {
    return MPI_SUCCESS;
}
