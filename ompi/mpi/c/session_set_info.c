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
#include "opal/util/info_subscriber.h"
#include <stdlib.h>
#include <string.h>

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Session_set_info = PMPI_Session_set_info
#endif
#define MPI_Session_set_info PMPI_Session_set_info
#endif

static const char FUNC_NAME[] = "MPI_Session_set_info";


int MPI_Session_set_info (MPI_Session session, MPI_Info info)
{
    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_instance_invalid(session)) {
            if (NULL != session) {
                return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_SESSION, FUNC_NAME);
            } else {
                return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_SESSION, FUNC_NAME);
            }

        }
        if (NULL == info || MPI_INFO_NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_INVOKE (session, MPI_ERR_INFO, FUNC_NAME);
        }
    }

    opal_infosubscribe_change_info (&session->super, &info->super);

    return MPI_SUCCESS;
}
