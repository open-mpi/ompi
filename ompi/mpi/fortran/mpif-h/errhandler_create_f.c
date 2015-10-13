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
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"
#include "ompi/errhandler/errhandler.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ERRHANDLER_CREATE = ompi_errhandler_create_f
#pragma weak pmpi_errhandler_create = ompi_errhandler_create_f
#pragma weak pmpi_errhandler_create_ = ompi_errhandler_create_f
#pragma weak pmpi_errhandler_create__ = ompi_errhandler_create_f

#pragma weak PMPI_Errhandler_create_f = ompi_errhandler_create_f
#pragma weak PMPI_Errhandler_create_f08 = ompi_errhandler_create_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_CREATE,
                           pmpi_errhandler_create,
                           pmpi_errhandler_create_,
                           pmpi_errhandler_create__,
                           pompi_errhandler_create_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_CREATE = ompi_errhandler_create_f
#pragma weak mpi_errhandler_create = ompi_errhandler_create_f
#pragma weak mpi_errhandler_create_ = ompi_errhandler_create_f
#pragma weak mpi_errhandler_create__ = ompi_errhandler_create_f

#pragma weak MPI_Errhandler_create_f = ompi_errhandler_create_f
#pragma weak MPI_Errhandler_create_f08 = ompi_errhandler_create_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_CREATE,
                           mpi_errhandler_create,
                           mpi_errhandler_create_,
                           mpi_errhandler_create__,
                           ompi_errhandler_create_f,
                           (ompi_errhandler_fortran_handler_fn_t* function, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (function, errhandler, ierr) )
#else
#define ompi_errhandler_create_f pompi_errhandler_create_f
#endif
#endif

void ompi_errhandler_create_f(ompi_errhandler_fortran_handler_fn_t* function,
			     MPI_Fint *errhandler, MPI_Fint *ierr)
{
    ompi_comm_create_errhandler_f(function, errhandler, ierr);
}
