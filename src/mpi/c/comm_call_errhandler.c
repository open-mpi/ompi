/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Comm_call_errhandler = PMPI_Comm_call_errhandler
#endif
    

int MPI_Comm_call_errhandler(MPI_Comm comm, int errorcode) {
    return MPI_SUCCESS;
}
