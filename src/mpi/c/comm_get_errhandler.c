/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_get_errhandler = PMPI_Comm_get_errhandler
#endif

int MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *erhandler) {
    return MPI_SUCCESS;
}
