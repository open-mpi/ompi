/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_group = PMPI_Comm_group
#endif

int MPI_Comm_group(MPI_Comm comm, MPI_Group *group) {
    return MPI_SUCCESS;
}
