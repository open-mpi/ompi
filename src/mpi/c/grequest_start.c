/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "communicator/communicator.h"
#include "request/grequest.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Grequest_start = PMPI_Grequest_start
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Grequest_start";


int MPI_Grequest_start(MPI_Grequest_query_function *query_fn,
                       MPI_Grequest_free_function *free_fn,
                       MPI_Grequest_cancel_function *cancel_fn,
                       void *extra_state, MPI_Request *request) 
{
    int rc;
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }

    rc = ompi_grequest_start(query_fn,free_fn,cancel_fn,extra_state,request);
    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}

