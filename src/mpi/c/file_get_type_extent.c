/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_type_extent = PMPI_File_get_type_extent
#endif

int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype,
		                     MPI_Aint *extent) {
    return MPI_SUCCESS;
}
