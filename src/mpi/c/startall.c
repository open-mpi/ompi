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
#pragma weak MPI_Startall = PMPI_Startall
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif


int MPI_Startall(int count, MPI_Request *requests) 
{
    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if (ompi_mpi_finalized) {
            rc = MPI_ERR_INTERN;
        } else if (requests == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        if (rc != MPI_SUCCESS) {
            return rc;
        }
    }
    return mca_pml.pml_start(count, requests);
}

