/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Wait = PMPI_Wait
#endif

int MPI_Wait(MPI_Request *request, MPI_Status *status) 
{
    int index, rc;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( LAM_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Wait");
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
    LAM_ERRHANDLER_RETURN(rc, (lam_communicator_t*)NULL, rc, "MPI_Wait");
}

