/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_BUILD_MPI_PROFILING
#pragma weak MPI_Errhandler_create = PMPI_Errhandler_create
#endif

#if OMPI_BUILD_MPI_PROFILING
#define MPI_Errhandler_create PMPI_Errhandler_create
#endif

#if OMPI_ENABLE_MPI_PROFILING
#define MPI_Comm_create_errhandler PMPI_Comm_create_errhandler
#endif

int MPI_Errhandler_create(MPI_Handler_function *function,
                          MPI_Errhandler *errhandler)
{

    /* This is a deprecated -- just turn around and call the real
       function */

    return MPI_Comm_create_errhandler(function, errhandler);
}
