/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_get_view = PMPI_File_get_view
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_File_get_view(MPI_File fh, MPI_Offset *disp,
		              MPI_Datatype *etype,
					  MPI_Datatype *filetype, char *datarep) {
    return MPI_SUCCESS;
}
