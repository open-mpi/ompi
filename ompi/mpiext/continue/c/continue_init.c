/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2021      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mpiext/continue/c/continuation.h"
#include "ompi/memchecker.h"

#include "ompi/mpiext/continue/c/mpiext_continue_c.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPIX_Continue_init = PMPIX_Continue_init
#endif
#define MPIX_Continue_init PMPIX_Continue_init
#endif

static const char FUNC_NAME[] = "MPIX_Continue_init";

int MPIX_Continue_init(
    int max_poll,
    int flags,
    MPI_Info info,
    MPI_Request *cont_req)
{
    int rc = MPI_SUCCESS;

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == cont_req) {
            rc = MPI_ERR_ARG;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }


    ompi_request_t *res;
    rc = ompi_continue_allocate_request(&res, max_poll, flags, info);

    if (MPI_SUCCESS == rc) {
        *cont_req = res;
    }

    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}
