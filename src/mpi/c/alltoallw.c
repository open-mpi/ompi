/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Alltoallw = PMPI_Alltoallw
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Alltoallw(void *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype *sendtypes,
                  void *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype *recvtypes,
                  MPI_Comm comm) {
	return MPI_SUCCESS;
}

