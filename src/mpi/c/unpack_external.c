/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Unpack_external = PMPI_Unpack_external
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Unpack_external (char *datarep, void *inbuf, MPI_Aint insize,
                         MPI_Aint *position, void *outbuf, int outcount,
                         MPI_Datatype datatype) {
    return MPI_SUCCESS;
}
