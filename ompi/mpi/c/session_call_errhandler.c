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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/instance/instance.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Session_call_errhandler = PMPI_Session_call_errhandler
#endif
#define MPI_Session_call_errhandler PMPI_Session_call_errhandler
#endif


static const char FUNC_NAME[] __opal_attribute_unused__ = "MPI_Session_call_errhandler";


int MPI_Session_call_errhandler(MPI_Session session, int errorcode)
{
  /* Error checking */

  if (MPI_PARAM_CHECK) {
    if (ompi_instance_invalid(session)) {
        if (NULL != session) {
            return OMPI_ERRHANDLER_INVOKE(session, MPI_ERR_SESSION, FUNC_NAME);
        } else {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_SESSION, FUNC_NAME);
        }
    }
  }

  /* Invoke the errhandler */

  OMPI_ERRHANDLER_INVOKE(session, errorcode, FUNC_NAME);

  return MPI_SUCCESS;
}

