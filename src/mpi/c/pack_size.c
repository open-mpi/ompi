/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Pack_size = PMPI_Pack_size
#endif

int MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm,
                  int *size) {
    return MPI_SUCCESS;
}
