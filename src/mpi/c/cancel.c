/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Cancel = PMPI_Cancel
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Cancel(MPI_Request *request) 
{
    int rc;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( OMPI_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        /* JMS: Tim will fix to invoke on the communicator/window/file
           on the request (i.e., not COMM_WORLD), if the request is
           available/valid */
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, "MPI_Cancel");
    }

    if (NULL == *request) {
        return MPI_SUCCESS;
    }
    rc = mca_pml.pml_cancel(*request);
    /* JMS: Tim will fix to invoke on the communicator/window/file on
       the request (i.e., not COMM_WORLD) */
    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, "MPI_Cancel");
}

