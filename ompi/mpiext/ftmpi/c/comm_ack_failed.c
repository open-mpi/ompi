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
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpiext/ftmpi/c/mpiext_ftmpi_c.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_Comm_ack_failed = PMPIX_Comm_ack_failed
#endif
#define MPIX_Comm_ack_failed PMPIX_Comm_ack_failed
#endif

static const char FUNC_NAME[] = "MPIX_Comm_ack_failed";


int MPIX_Comm_ack_failed(MPI_Comm comm, int num_to_ack, int* num_acked)
{
    int rc = MPI_SUCCESS;

    /* Argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        }
        if (num_to_ack > ompi_comm_size(comm)) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    rc = ompi_comm_ack_failed_internal( (ompi_communicator_t*)comm, num_to_ack, num_acked );
    OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
}

