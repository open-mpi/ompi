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
 * Copyright (c) 2020      Triad National Security, LLC. All rights
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
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"
#include "ompi/instance/instance.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Comm_get_errhandler = PMPI_Comm_get_errhandler
#endif
#define MPI_Comm_get_errhandler PMPI_Comm_get_errhandler
#endif


static const char FUNC_NAME[] = "MPI_Comm_get_errhandler";


int MPI_Comm_get_errhandler(MPI_Comm comm, MPI_Errhandler *errhandler)
{
    int ret = MPI_SUCCESS;

    /* Error checking */
    MEMCHECKER(
        memchecker_comm(comm);
    );

    /* Error checking */

    if (MPI_PARAM_CHECK) {
      OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
      if (ompi_comm_invalid(comm)) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_COMM,
                                      FUNC_NAME);
      } else if (NULL == errhandler) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                      FUNC_NAME);
      }
    }

    OPAL_THREAD_LOCK(&(comm->c_lock));
    /* Retain the errhandler, corresponding to object refcount decrease
       in errhandler_free.c. */
    OBJ_RETAIN(comm->error_handler);
    *errhandler = comm->error_handler;
    OPAL_THREAD_UNLOCK(&(comm->c_lock));

   /* make sure the infrastructure is initialized */
    ret = ompi_mpi_instance_retain ();

    /* All done */

    return ret;
}
