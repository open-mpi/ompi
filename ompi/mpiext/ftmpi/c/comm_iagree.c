/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
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

#include "ompi/mpiext/ftmpi/c/mpiext_ftmpi_c.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_Comm_iagree = PMPIX_Comm_iagree
#endif
#define MPIX_Comm_iagree PMPIX_Comm_iagree
#endif

static const char FUNC_NAME[] = "MPIX_Comm_iagree";


int MPIX_Comm_iagree(MPI_Comm comm, int *flag, MPI_Request *request)
{
    int rc = MPI_SUCCESS;
    ompi_group_t* acked;

    /* Argument checking */
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        }
        if (NULL == request) {
            rc = MPI_ERR_REQUEST;
        }
        else if (NULL == flag) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    ompi_comm_failure_get_acked_internal( comm, &acked );
    rc = comm->c_coll->coll_iagree( flag,
                                    1,
                                    &ompi_mpi_int.dt,
                                    &ompi_mpi_op_band.op,
                                    &acked, false,
                                    (ompi_communicator_t*)comm,
                                    request,
                                    comm->c_coll->coll_iagree_module);
    OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
}

