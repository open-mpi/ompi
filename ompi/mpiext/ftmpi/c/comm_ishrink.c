/*
 * Copyright (c) 2018-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "ompi/proc/proc.h"
#include "ompi/op/op.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_Comm_ishrink = PMPIX_Comm_ishrink
#endif
#define MPIX_Comm_ishrink PMPIX_Comm_ishrink
#endif

#include "ompi/mpiext/ftmpi/c/mpiext_ftmpi_c.h"

static const char FUNC_NAME[] = "MPIX_Comm_ishrink";


int MPIX_Comm_ishrink(MPI_Comm comm, MPI_Comm* newcomm, MPI_Request *request)
{
    int rc = MPI_SUCCESS;

    /* Argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        }
        if (NULL == newcomm) {
            rc = MPI_ERR_ARG;
        }
        if (NULL == request) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    rc = ompi_comm_ishrink_internal(comm, newcomm, request);
    OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
}

