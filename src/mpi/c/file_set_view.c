/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_set_view = PMPI_File_set_view
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_File_set_view(MPI_File fh, MPI_Offset disp, MPI_Datatype etype,
		              MPI_Datatype filetype, char *datarep, MPI_Info info) {
    return MPI_SUCCESS;
}
