/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
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
    int ret;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == session || MPI_SESSION_NULL == session) {
            return MPI_ERR_ARG;
        }
        if (NULL == info_used) {
            return OMPI_ERRHANDLER_INVOKE (session, MPI_ERR_INFO, FUNC_NAME);
        }
    }

    ret = ompi_instance_get_pset_info (session, pset_name, (opal_info_t **) info_used);
    return OMPI_ERRHANDLER_INVOKE(session, ret, FUNC_NAME);
}
