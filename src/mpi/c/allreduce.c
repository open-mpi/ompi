/*
 * $HEADER$
 */

#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Allreduce = PMPI_Allreduce
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Allreduce(void *sendbuf, void *recvbuf, int count,
		           MPI_Datatype datatype, MPI_Op op, MPI_Comm comm) {
	return MPI_SUCCESS;
}

