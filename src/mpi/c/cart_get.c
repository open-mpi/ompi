/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cart_get = PMPI_Cart_get
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Cart_get(MPI_Comm comm, int maxdims, int *dims,
                 int *periods, int *coords) {
    return MPI_SUCCESS;
}
