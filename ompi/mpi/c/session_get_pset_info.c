/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2022 Triad National Security, LLC. All rights
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
#include "ompi/instance/instance.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"
#include <stdlib.h>
#include <string.h>

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Session_get_pset_info = PMPI_Session_get_pset_info
#endif
#define MPI_Session_get_pset_info PMPI_Session_get_pset_info
#endif

static const char FUNC_NAME[] = "MPI_Session_get_pset_info";


int MPI_Session_get_pset_info (MPI_Session session, const char *pset_name, MPI_Info *info_used)
{
    int rc;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_instance_invalid(session)) {
            if (NULL != session) {
                return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_SESSION, FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_SESSION, FUNC_NAME);
            }
        }
        if (NULL == info_used) {
            return OMPI_ERRHANDLER_INVOKE (session, MPI_ERR_INFO, FUNC_NAME);
        }
        if (NULL == pset_name) {
            return OMPI_ERRHANDLER_INVOKE (session, MPI_ERR_ARG, FUNC_NAME);
        }
    }

    rc = ompi_instance_get_pset_info (session, pset_name, (opal_info_t **) info_used);
    /*
     * if process set was not found, OMPI_ERR_NOT_FOUND is the return value.
     * we want to map this to MPI_ERR_ARG but we have to do it manually here
     * since the OMPI error to MPI error code code maps this to MPI_ERR_INTERN
     */
    if (OMPI_ERR_NOT_FOUND == rc) {
        rc = MPI_ERR_ARG;
    }

    return OMPI_ERRHANDLER_INVOKE(session, rc, FUNC_NAME);
}
