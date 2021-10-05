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
#pragma weak MPIX_Continueall = PMPIX_Continueall
#endif
#define MPIX_Continueall PMPIX_Continueall
#endif

static const char FUNC_NAME[] = "MPIX_Continueall";

int MPIX_Continueall(
    int                count,
    MPI_Request        requests[],
    MPIX_Continue_cb_function *cont_cb,
    void              *cont_data,
    MPI_Status         statuses[],
    MPI_Request        cont_req)
{
    int rc;

    MEMCHECKER(
        for (int j = 0; j < count; j++){
            memchecker_request(&requests[j]);
        }
    );


    if (MPI_PARAM_CHECK) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (MPI_REQUEST_NULL == cont_req || OMPI_REQUEST_CONT != cont_req->req_type) {
            rc = MPI_ERR_REQUEST;
        }
        if( (NULL == requests) && (0 != count) ) {
            rc = MPI_ERR_REQUEST;
        } else {
            for (int i = 0; i < count; i++) {
                if (NULL == requests[i]) {
                    rc = MPI_ERR_REQUEST;
                    break;
                }
            }
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    rc = ompi_continue_attach(cont_req, count, requests, cont_cb,
                              cont_data, statuses);

    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}
