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
#pragma weak MPI_Waitany = PMPI_Waitany
#endif

int MPI_Waitany(int count, MPI_Request *requests, int *index, MPI_Status *status) 
{
    int rc;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( LAM_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (requests == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Waitany");
    }
    rc = mca_pml.pml_wait(count, requests, index, status);
    LAM_ERRHANDLER_RETURN(rc, (lam_communicator_t*)NULL, rc, "MPI_Waitany");
}

