/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_File_iwrite_at = PMPI_File_iwrite_at
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_File_iwrite_at(MPI_File fh, MPI_Offset offset, void *buf,
                       int count, MPI_Datatype datatype, 
                       MPI_Request *request) {
    return MPI_SUCCESS;
}
