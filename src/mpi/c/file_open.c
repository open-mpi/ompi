/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_open = PMPI_File_open
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_File_open(MPI_Comm comm, char *filename, int amode,
		          MPI_Info info, MPI_File *fh) {
    return MPI_SUCCESS;
}
