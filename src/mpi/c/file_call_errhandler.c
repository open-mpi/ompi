/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_call_errhandler = PMPI_File_call_errhandler
#endif

int MPI_File_call_errhandler(MPI_File fh, int errorcode) {
    return MPI_SUCCESS;
}
