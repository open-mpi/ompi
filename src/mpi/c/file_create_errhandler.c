/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_create_errhandler = PMPI_File_create_errhandler
#endif

int MPI_File_create_errhandler(MPI_File_errhandler_fn *function,
		                        MPI_Errhandler *errhandler) {
    return MPI_SUCCESS;
}
