/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"


#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Waitsome = PMPI_Waitsome
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Waitsome";


int MPI_Waitsome(int incount, MPI_Request *requests,
                 int *outcount, int *indices,
                 MPI_Status *statuses) 
{
    int index;
    int rc;
    MPI_Status status;

    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (requests == NULL) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    /* optimize this in the future */
    rc = ompi_request_wait_any( incount, requests, &index, &status );
    OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    if( MPI_UNDEFINED ==index ) {
       *outcount = MPI_UNDEFINED;
    } else {
       *outcount = 1;
       indices[0] = index;
       if( MPI_STATUSES_IGNORE != statuses ) {
          statuses[index] = status;
       }
    }
    return MPI_SUCCESS;
}

