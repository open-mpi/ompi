/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_read_at = PMPI_File_read_at
#endif

int MPI_File_read_at(MPI_File fh, MPI_Offset offset, void *buf,
		            int count, MPI_Datatype datatype, MPI_Status *status) {
								
    return MPI_SUCCESS;
}
