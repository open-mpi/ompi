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
#pragma weak MPI_Testany = PMPI_Testany
#endif

#if LAM_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Testany(int count, MPI_Request requests[], int *index, int *completed, MPI_Status *status) 
{
    int rc;
    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if ( LAM_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (NULL == requests) {
            rc = MPI_ERR_REQUEST;
        } else if (NULL == index) {
            rc = MPI_ERR_ARG;
        }
        LAM_ERRHANDLER_CHECK(rc, (lam_communicator_t*)NULL, rc, "MPI_Testany");
    }
                                                                                                                        
    rc = mca_pml.pml_test(count, requests, index, completed, status);
    LAM_ERRHANDLER_RETURN(rc, (lam_communicator_t*)NULL, rc, "MPI_Testany");
}

