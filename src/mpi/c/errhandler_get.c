/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_get = PMPI_Errhandler_get
#endif

int MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler) {
    return MPI_SUCCESS;
}
