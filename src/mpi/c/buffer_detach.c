/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Buffer_detach = PMPI_Buffer_detach
#endif

int MPI_Buffer_detach(void *buffer, int *size) {
    return MPI_SUCCESS;
}
