/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Alltoall = PMPI_Alltoall
#endif

int MPI_Alltoall(void *sendbuf, int sendcount, MPI_Datatype sendtype,
		         void *recvbuf, int recvcount,
				 MPI_Datatype recvtype, MPI_Comm comm) {
	return MPI_SUCCESS;
}

