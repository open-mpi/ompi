/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Bcast = PMPI_Bcast
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype,
                int root, MPI_Comm comm) {
    return MPI_SUCCESS;
}
