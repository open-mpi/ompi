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
#pragma weak MPI_Wait = PMPI_Wait
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Wait(MPI_Request *request, MPI_Status *status) 
{
    int index, rc;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( OMPI_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_CHECK(rc, (ompi_communicator_t*)NULL, rc, "MPI_Wait");
    }

    if (NULL == *request) {
        if (NULL != status) {
            status->MPI_SOURCE = MPI_PROC_NULL;
            status->MPI_TAG = MPI_ANY_TAG;
            status->MPI_ERROR = MPI_SUCCESS;
            status->_count = 0;
        }
        return MPI_SUCCESS;
    }
    rc = mca_pml.pml_wait(1, request, &index, status);
    OMPI_ERRHANDLER_RETURN(rc, (ompi_communicator_t*)NULL, rc, "MPI_Wait");
}

