/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_coords = PMPI_Cart_coords
#endif

int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims, int *coords) {
    return MPI_SUCCESS;
}
