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
#pragma weak MPIX_Continue = PMPIX_Continue
#endif
#define MPIX_Continue PMPIX_Continue
#endif

static const char FUNC_NAME[] = "MPIX_Continue";

int MPIX_Continue(
    MPI_Request       *request,
    MPIX_Continue_cb_function *cont_cb,
    void              *cb_data,
    int                flags,
    MPI_Status        *status,
    MPI_Request        cont_req)
{
    int rc;

    MEMCHECKER(
        memchecker_request(request);
    );

    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == request) {
            rc = MPI_ERR_REQUEST;
        }
        if (MPI_REQUEST_NULL == cont_req || OMPI_REQUEST_CONT != cont_req->req_type) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    rc = ompi_continue_attach(cont_req, 1, request, cont_cb, cb_data, flags,
                              MPI_STATUS_IGNORE == status ? MPI_STATUSES_IGNORE : status);

    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}
