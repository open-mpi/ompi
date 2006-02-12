/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Comm_create_errhandler = PMPI_Comm_create_errhandler
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif


static const char FUNC_NAME[] = "MPI_Comm_create_errhandler";


int MPI_Comm_create_errhandler(MPI_Comm_errhandler_fn *function,
                               MPI_Errhandler *errhandler) 
{
  int err = MPI_SUCCESS;

  /* Error checking */

  if (MPI_PARAM_CHECK) {
    OMPI_ERR_INIT_FINALIZE(FUNC_NAME);

    if (NULL == function || 
        NULL == errhandler) {
      return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG,
                                    FUNC_NAME);
    }
  }

  /* Create and cache the errhandler.  Sets a refcount of 1. */

  *errhandler = 
    ompi_errhandler_create(OMPI_ERRHANDLER_TYPE_COMM,
                          (ompi_errhandler_generic_handler_fn_t*) function);
  if (NULL == *errhandler) {
    err = MPI_ERR_INTERN;
  }

  OMPI_ERRHANDLER_RETURN(err, MPI_COMM_WORLD, MPI_ERR_INTERN, FUNC_NAME);
}
