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
#include "ompi/info/info.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/instance/instance.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Session_init = PMPI_Session_init
#endif
#define MPI_Session_init PMPI_Session_init
#endif

static const char FUNC_NAME[] = "MPI_Session_init";


int MPI_Session_init (MPI_Info info, MPI_Errhandler errhandler, MPI_Session *session)
{
    int rc, flag;
    int ts_level = MPI_THREAD_SINGLE;  /* for now we default to thread single for OMPI sessions */
    opal_cstring_t *info_value;
    const char ts_level_multi[] = "MPI_THREAD_MULTIPLE";

    if ( MPI_PARAM_CHECK ) {
        if (NULL == errhandler || NULL == session) {
            return MPI_ERR_ARG;
        }

        if (NULL == info || ompi_info_is_freed (info)) {
            return MPI_ERR_INFO;
        }
    }

    if (MPI_INFO_NULL != info) {
        (void) ompi_info_get (info, "thread_level", &info_value, &flag);
        if (flag) {
            if(strncmp(info_value->string, ts_level_multi, strlen(ts_level_multi)) == 0) {
                ts_level = MPI_THREAD_MULTIPLE;
            }
            OBJ_RELEASE(info_value);
        }
    }

    rc = ompi_mpi_instance_init (ts_level, &info->super, errhandler, session, 0, NULL);
    /* if an error occurred raise it on the null session */
    OMPI_ERRHANDLER_RETURN (rc, MPI_SESSION_NULL, rc, FUNC_NAME);
}
