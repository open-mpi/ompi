/*
 * Copyright (c) 2022      The University of Tennessee and The University
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
#include "ompi/proc/proc.h"

#include "ompi/mpiext/ftmpi/c/mpiext_ftmpi_c.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_Comm_get_failed = PMPIX_Comm_get_failed
#endif
#define MPIX_Comm_get_failed PMPIX_Comm_get_failed
#endif

static const char FUNC_NAME[] = "MPIX_Comm_get_failed";

int MPIX_Comm_get_failed(MPI_Comm comm, MPI_Group *failedgrp)
{
    int rc = MPI_SUCCESS;

    /* Argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    rc = ompi_comm_get_failed_internal( (ompi_communicator_t*)comm, (ompi_group_t**)failedgrp );
    if( OMPI_SUCCESS != rc ) {
        OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
    }

    return MPI_SUCCESS;
}

