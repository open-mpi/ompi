/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Pack_external = PMPI_Pack_external
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Pack_external(char *datarep, void *inbuf, int incount,
                      MPI_Datatype datatype, void *outbuf,
                      MPI_Aint outsize, MPI_Aint *position) {
    return MPI_SUCCESS;
}
