/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_iwrite = PMPI_File_iwrite
#endif

int MPI_File_iwrite(MPI_File fh, void *buf, int count, MPI_Datatype
		                    datatype, MPI_Request *request) {
    return MPI_SUCCESS;
}
