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
    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if (lam_mpi_finalized) {
            rc = MPI_ERR_INTERN;
        } else if (requests == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        if (rc != MPI_SUCCESS) {
            return rc;
        }
    }
    return mca_pml.pml_wait(count, requests, index, status);
}

