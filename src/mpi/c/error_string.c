/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Error_string = PMPI_Error_string
#endif

int MPI_Error_string(int errorcode, char *string, int *resultlen) {
    return MPI_SUCCESS;
}
