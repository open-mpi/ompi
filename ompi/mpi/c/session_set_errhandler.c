/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights
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
#pragma weak MPI_Session_set_errhandler = PMPI_Session_set_errhandler
#endif
#define MPI_Session_set_errhandler PMPI_Session_set_errhandler
#endif

static const char FUNC_NAME[] = "MPI_Session_set_errhandler";


int MPI_Session_set_errhandler(MPI_Session session, MPI_Errhandler errhandler)
{
    MPI_Errhandler tmp;

    /* Error checking */

    if (MPI_PARAM_CHECK) {
        if (ompi_instance_invalid(session)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_SESSION, FUNC_NAME);
        } else if (NULL == errhandler ||
                   MPI_ERRHANDLER_NULL == errhandler ||
                   ( OMPI_ERRHANDLER_TYPE_INSTANCE != errhandler->eh_mpi_object_type &&
                     OMPI_ERRHANDLER_TYPE_PREDEFINED != errhandler->eh_mpi_object_type) ) {
            return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_ARG,
                                          FUNC_NAME);
        }
    }

    /* Prepare the new error handler */
    OBJ_RETAIN(errhandler);

    OPAL_THREAD_LOCK(&(session->s_lock));
    /* Ditch the old errhandler, and decrement its refcount. */
    tmp = session->error_handler;
    session->error_handler = errhandler;
    OBJ_RELEASE(tmp);
    OPAL_THREAD_UNLOCK(&(session->s_lock));

    /* All done */
    return MPI_SUCCESS;
}
