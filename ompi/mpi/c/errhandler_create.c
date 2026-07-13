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
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

/* This implementation has been removed from the MPI 3.0 standard.
 * Open MPI v4.0.x is keeping the implementation in the library, but
 * removing the prototypes from the headers, unless the user configures
 * with --enable-mpi1-compatibility.
 */

#include "ompi/mpi/c/bindings.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_ALIASES
#pragma weak MPI_Errhandler_create = PMPI_Errhandler_create
#endif
/* undef before defining, to prevent possible redefinition when
 * using _Static_assert to error on usage of removed functions.
 */
#undef MPI_Errhandler_create
#define MPI_Errhandler_create PMPI_Errhandler_create
#else
/*
 * Emit the public MPI_* symbol as a *weak* definition.  Where weak aliases
 * are available the alias above is already weak; where they are not, the
 * bindings are compiled a second time to produce MPI_*, and this is that
 * copy -- so mark it weak here.
 *
 * Weak MPI_* is what lets a profiling library provide a strong MPI_* that
 * overrides ours, and it is what the MPI Forum ABI requires of libmpi_abi.
 */
#pragma weak MPI_Errhandler_create
#endif

int MPI_Errhandler_create(MPI_Handler_function *function,
                          MPI_Errhandler *errhandler)
{

    /* This is a deprecated -- just turn around and call the real
       function */

    return PMPI_Comm_create_errhandler(function, errhandler);
}
