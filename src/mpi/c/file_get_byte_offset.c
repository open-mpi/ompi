/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_File_get_byte_offset = PMPI_File_get_byte_offset
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_File_get_byte_offset(MPI_File fh, MPI_Offset offset,
		                     MPI_Offset *disp) {
    return MPI_SUCCESS;
}
