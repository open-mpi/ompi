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
        if (ompi_instance_invalid(session)) {
            if (NULL != session) {
                return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_SESSION, FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_SESSION, FUNC_NAME);
            }
	} else if (NULL == pset_name || NULL == newgroup) {
            return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    rc = ompi_group_from_pset (session, pset_name, newgroup);
    /*
     * if process set was not found, OMPI_ERR_NOT_FOUND is the return value.
     * we want to map this to MPI_ERR_ARG but we have to do it manually here
     * since the OMPI error to MPI error code code maps this to MPI_ERR_INTERN
     */
    if (OMPI_ERR_NOT_FOUND == rc) {
        rc = MPI_ERR_ARG;
    }


    OMPI_ERRHANDLER_RETURN (rc, (NULL == session) ? MPI_SESSION_NULL : session,
                            rc, FUNC_NAME);
}
