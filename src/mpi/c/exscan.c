/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Exscan = PMPI_Exscan
#endif

int MPI_Exscan(void *sendbuf, void *recvbuf, int count,
		       MPI_Datatype datatype, MPI_Op op, MPI_Comm comm) {
    return MPI_SUCCESS;
}
