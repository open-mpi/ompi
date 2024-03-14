/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018-2020 Triad National Security, LLC. All rights
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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Session_get_nth_pset = PMPI_Session_get_nth_pset
#endif
#define MPI_Session_get_nth_pset PMPI_Session_get_nth_pset
#endif

static const char FUNC_NAME[] = "MPI_Session_get_nth_pset";


int MPI_Session_get_nth_pset (MPI_Session session, MPI_Info info, int n, int *len, char *pset_name)
{
    int rc = MPI_SUCCESS;

    if ( MPI_PARAM_CHECK ) {
        if (ompi_instance_invalid(session)) {
            if (NULL != session) {
                return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_SESSION, FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_SESSION, FUNC_NAME);
            }
        } else if ((NULL == pset_name && *len > 0) || n < 0) {
            return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    rc = ompi_instance_get_nth_pset (session, n, len, pset_name);

    OMPI_ERRHANDLER_RETURN (rc, (NULL == session) ? MPI_SESSION_NULL : session, 
                            rc, FUNC_NAME);
}
