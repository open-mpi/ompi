/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Errhandler_create = PMPI_Errhandler_create
#endif

int MPI_Errhandler_create(MPI_Handler_function *function,
		                   MPI_Errhandler *errhandler) {
    return MPI_SUCCESS;
}
