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
#pragma weak MPI_Testsome = PMPI_Testsome
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Testsome(int incount, MPI_Request requests[],
                 int *outcount, int indices[],
                 MPI_Status statuses[]) 
{
    int rc, index, completed;
    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if ( LAM_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (NULL == requests) {
            rc = MPI_ERR_REQUEST;
        } else if (NULL == indices) {
            rc = MPI_ERR_ARG;
        }
        LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Testsome");
    }
                                                                                                                        
    /* optimize this in the future */
    rc = mca_pml.pml_test(incount, requests, &index, &completed, statuses);
    LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Testsome");
    if(completed) {
        *outcount = 1;
        indices[0] = index;
    } else {
        *outcount = 0;
    }
    return MPI_SUCCESS;
}

