/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Accumulate = PMPI_Accumulate
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Accumulate(void *origin_addr, int origin_count, MPI_Datatype origin_datatype,
					int target_rank, MPI_Aint target_disp, int target_count,
					MPI_Datatype target_datatype, MPI_Op op, MPI_Win win) {

	return MPI_SUCCESS;
}
