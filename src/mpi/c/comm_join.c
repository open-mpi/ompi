/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_join = PMPI_Comm_join
#endif

int MPI_Comm_join(int fd, MPI_Comm *intercomm) {
    return MPI_SUCCESS;
}
