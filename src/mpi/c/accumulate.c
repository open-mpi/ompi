/*
 * $HEADER$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Accumulate = PMPI_Accumulate
#endif

int MPI_Accumulate(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
					int target_rank, MPI_Aint target_disp, int target_count,
					MPI_Datatype target_datatype, MPI_Op op, MPI_Win win) {

	return MPI_SUCCESS;
}
