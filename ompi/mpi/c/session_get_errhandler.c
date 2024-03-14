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
 * Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
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
#pragma weak MPI_Session_get_errhandler = PMPI_Session_get_errhandler
#endif
#define MPI_Session_get_errhandler PMPI_Session_get_errhandler
#endif


static const char FUNC_NAME[] = "MPI_Session_get_errhandler";


int MPI_Session_get_errhandler(MPI_Session session, MPI_Errhandler *errhandler)
{
    int ret = MPI_SUCCESS;

    /* Error checking */

    if (MPI_PARAM_CHECK) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (ompi_instance_invalid(session)) {
        if (NULL != session) {
            return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_SESSION, FUNC_NAME);
        } else {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_SESSION, FUNC_NAME);
        }
      }
    }

    OPAL_THREAD_LOCK(&(session->s_lock));
    /* Retain the errhandler, corresponding to object refcount decrease
       in errhandler_free.c. */
    OBJ_RETAIN(session->error_handler);
    *errhandler = session->error_handler;
    OPAL_THREAD_UNLOCK(&(session->s_lock));

   /* make sure the infrastructure is initialized */
    ret = ompi_mpi_instance_retain ();

    /* All done */
    OMPI_ERRHANDLER_RETURN (ret, session, ret, FUNC_NAME);
}
