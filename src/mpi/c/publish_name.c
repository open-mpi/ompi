/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Publish_name = PMPI_Publish_name
#endif

int MPI_Publish_name(char *service_name, MPI_Info info,
                     char *port_name) {
    return MPI_SUCCESS;
}
