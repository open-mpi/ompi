/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Ibsend = PMPI_Ibsend
#endif

int MPI_Ibsend(void *buf, int count, MPI_Datatype datatype, int dest,
		       int tag, MPI_Comm comm, MPI_Request *request) {
	return MPI_SUCCESS;
}
