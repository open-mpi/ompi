/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Dims_create = PMPI_Dims_create
#endif

int MPI_Dims_create(int nnodes, int ndims, int *dims) {
    return MPI_SUCCESS;
}
