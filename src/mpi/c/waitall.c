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
#pragma weak MPI_Waitall = PMPI_Waitall
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Waitall(int count, MPI_Request *requests, MPI_Status *statuses) 
{
    int rc;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( LAM_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (requests == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Waitall");
    }
    rc = mca_pml.pml_wait_all(count, requests, statuses);
    LAM_ERRHANDLER_RETURN(rc, (lam_communicator_t*)NULL, rc, "MPI_Waitall");
}

