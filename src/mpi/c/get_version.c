/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Get_version = PMPI_Get_version
#endif

int MPI_Get_version(int *version, int *subversion) {
    return MPI_SUCCESS;
}
