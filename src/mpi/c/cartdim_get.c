/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Cartdim_get = PMPI_Cartdim_get
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Cartdim_get(MPI_Comm comm, int *ndims) {
    return MPI_SUCCESS;
}
