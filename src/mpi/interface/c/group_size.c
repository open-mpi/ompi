/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Group_size = PMPI_Group_size
#endif

int MPI_Group_size(MPI_Group group, int *size) {
    return MPI_SUCCESS;
}
