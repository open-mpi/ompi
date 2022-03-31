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
#include "ompi/instance/instance.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Group_from_session_pset = PMPI_Group_from_session_pset
#endif
#define MPI_Group_from_session_pset PMPI_Group_from_session_pset
#endif

static const char FUNC_NAME[] = "MPI_Group_from_session_pset";


int MPI_Group_from_session_pset (MPI_Session session, const char *pset_name, MPI_Group *newgroup)
{
    int rc;

    if ( MPI_PARAM_CHECK ) {
        if (NULL == session || NULL == pset_name || NULL == newgroup) {
            if (NULL != session) {
                return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_ARG, FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG, FUNC_NAME);
            }
        }
    }

    rc = ompi_group_from_pset (session, pset_name, newgroup);

    OMPI_ERRHANDLER_RETURN (rc, (NULL == session) ? MPI_SESSION_NULL : session,
                            rc, FUNC_NAME);
}
