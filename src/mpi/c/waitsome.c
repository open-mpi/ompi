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
#pragma weak MPI_Waitsome = PMPI_Waitsome
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Waitsome(int incount, MPI_Request *requests,
                 int *outcount, int *indices,
                 MPI_Status *statuses) 
{
    int index;
    int rc;

    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if (lam_mpi_finalized) {
            rc = MPI_ERR_INTERN;
        } else if (requests == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Waitsome");
    }

    /* optimize this in the future */
    rc = mca_pml.pml_wait(incount, requests, &index, statuses);
    LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Waitsome");
    *outcount = 1;
    indices[0] = index;
    return MPI_SUCCESS;
}

