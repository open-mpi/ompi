/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_set = PMPI_Errhandler_set
#endif

int MPI_Errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler) {
    return MPI_SUCCESS;
}
