/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Error_class = PMPI_Error_class
#endif

int MPI_Error_class(int errorcode, int *errorclass) {
    return MPI_SUCCESS;
}
