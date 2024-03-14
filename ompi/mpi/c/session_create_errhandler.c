/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2021 Triad National Security, LLC. All rights
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
#include "ompi/instance/instance.h"

#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Session_create_errhandler = PMPI_Session_create_errhandler
#endif
#define MPI_Session_create_errhandler PMPI_Session_create_errhandler
#endif

static const char FUNC_NAME[] = "MPI_Session_create_errhandler";


int MPI_Session_create_errhandler (MPI_Session_errhandler_function *session_errhandler_fn, MPI_Errhandler *errhandler)
{
    int err = MPI_SUCCESS;

    if ( MPI_PARAM_CHECK ) {
        if (NULL == errhandler || NULL == session_errhandler_fn) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }

    /* Create and cache the errhandler.  Sets a refcount of 1. */
    *errhandler =
        ompi_errhandler_create(OMPI_ERRHANDLER_TYPE_INSTANCE,
                               (ompi_errhandler_generic_handler_fn_t *) session_errhandler_fn,
                               OMPI_ERRHANDLER_LANG_C);
    if (NULL == *errhandler) {
        err = MPI_ERR_INTERN;
    }

    OMPI_ERRHANDLER_NOHANDLE_RETURN(err, MPI_ERR_INTERN, FUNC_NAME);
}
