/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Testsome = PMPI_Testsome
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Testsome";


int MPI_Testsome(int incount, MPI_Request requests[],
                 int *outcount, int indices[],
                 MPI_Status statuses[]) 
{
    int rc, index, completed;
    ompi_status_public_t status;
    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == requests) {
            rc = MPI_ERR_REQUEST;
        } else if (NULL == indices) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    /* optimize this in the future */
    rc = ompi_request_test_any(incount, requests, &index, &completed, &status);
    OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    if(completed) {
        *outcount = (index == MPI_UNDEFINED) ? MPI_UNDEFINED : 1;
        indices[0] = index;
        statuses[0] = status;
    } else {
        *outcount = 0;
    }
    return MPI_SUCCESS;
}

