/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Triad National Security, LLC. All rights
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
#include "ompi/errhandler/errhandler.h"

#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Session_finalize = PMPI_Session_finalize
#endif
#define MPI_Session_finalize PMPI_Session_finalize
#endif

static const char FUNC_NAME[] = "MPI_Session_finalize";


int MPI_Session_finalize (MPI_Session *session)
{
    int rc;

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

        if (NULL == session || NULL == *session || MPI_SESSION_NULL == *session) {
            return MPI_ERR_ARG;
        }
    }

    rc = ompi_mpi_instance_finalize (session);
    /* if an error occured raise it on the null session */
    OMPI_ERRHANDLER_RETURN (rc, MPI_SESSION_NULL, rc, FUNC_NAME);
}
