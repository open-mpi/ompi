/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_get = PMPI_Cart_get
#endif

int MPI_Cart_get(MPI_Comm comm, int maxdims, int *dims,
                 int *periods, int *coords) {
    return MPI_SUCCESS;
}
