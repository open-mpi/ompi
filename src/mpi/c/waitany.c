/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Waitany = PMPI_Waitany
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

int MPI_Waitany(int count, MPI_Request *requests, int *index, MPI_Status *status) 
{
    int rc;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( OMPI_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (requests == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, "MPI_Waitany");
    }
    rc = mca_pml.pml_wait(count, requests, index, status);
    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, "MPI_Waitany");
}

