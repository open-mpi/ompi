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
#pragma weak MPI_Test = PMPI_Test
#endif

int MPI_Test(MPI_Request *request, int *completed, MPI_Status *status) 
{
    int rc, index;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( LAM_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (request == NULL) {
            rc = MPI_ERR_REQUEST;
        } else if (completed == NULL) {
            rc = MPI_ERR_ARG;
        }
        LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Test");
    }

    if(*request == NULL) {
        *completed = true;
        status->MPI_SOURCE = MPI_PROC_NULL;
        status->MPI_TAG = MPI_ANY_TAG;
        status->MPI_ERROR = MPI_SUCCESS;
        status->_count = 0;
        return MPI_SUCCESS;
    }
    rc = mca_pml.pml_test(1, request, index, completed, status);
    if(completed < 0)
        completed = 0;
    LAM_ERRHANDLER_RETURN(rc, (lam_communicator_t*)NULL, rc, "MPI_Test");
}

