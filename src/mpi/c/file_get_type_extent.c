/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_get_type_extent = PMPI_File_get_type_extent
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_File_get_type_extent(MPI_File fh, MPI_Datatype datatype,
		                     MPI_Aint *extent) {
    return MPI_SUCCESS;
}
