/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Open_port = PMPI_Open_port
#endif

int MPI_Open_port(MPI_Info info, char *port_name) {
    return MPI_SUCCESS;
}
