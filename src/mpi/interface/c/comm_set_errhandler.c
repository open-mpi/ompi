/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_set_errhandler = PMPI_Comm_set_errhandler
#endif

int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler) {
    return MPI_SUCCESS;
}
