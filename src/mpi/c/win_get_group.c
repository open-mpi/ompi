/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Win_get_group = PMPI_Win_get_group
#endif

int MPI_Win_get_group(MPI_Win win, MPI_Group *group) {
    return MPI_SUCCESS;
}
