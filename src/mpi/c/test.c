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
#pragma weak MPI_Test = PMPI_Test
#endif

int MPI_Test(MPI_Request *request, int *completed, MPI_Status *status) 
{
    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if ( lam_mpi_finalized ) {
            rc = MPI_ERR_INTERN;
        } else if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        } else if (completed == NULL) {
            rc = MPI_ERR_ARG;
        }
        if (rc != MPI_SUCCESS) {
            return rc;
        }
    }

    if(*request == NULL) {
        *completed = true;
        status->MPI_SOURCE = MPI_PROC_NULL;
        status->MPI_TAG = MPI_ANY_TAG;
        status->MPI_ERROR = MPI_SUCCESS;
        status->_count = 0;
        return MPI_SUCCESS;
    }
    return mca_pml.pml_test(request,completed,status);
}

