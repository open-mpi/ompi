/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "errhandler/errhandler.h"
#include "mca/pml/pml.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Request_free = PMPI_Request_free
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Request_free";


int MPI_Request_free(MPI_Request *request) 
{
    int rc;
    if( request == NULL ) {
        rc = OMPI_ERR_BAD_PARAM;
        goto error_return;
    }
    if( *request == NULL ) {
        return MPI_SUCCESS;
    }
    rc = mca_pml.pml_free(request);

error_return:
    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}

