/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/runtime/runtime.h"
#include "mpi/interface/c/bindings.h"
#include "mca/mpi/pml/pml.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Wait = PMPI_Wait
#endif

int MPI_Wait(MPI_Request *request, MPI_Status *status) 
{
    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if (lam_mpi_finalized) {
            rc = MPI_ERR_INTERN;
        } else if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        if (rc != MPI_SUCCESS) {
            return rc;
        }
    }

    if (*request == NULL) {
        status->MPI_SOURCE = MPI_PROC_NULL;
        status->MPI_TAG = MPI_ANY_TAG;
        status->MPI_ERROR = MPI_SUCCESS;
        status->_count = 0;
        return MPI_SUCCESS;
    }
    return mca_pml.pml_wait(request,status);
}

