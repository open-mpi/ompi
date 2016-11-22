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
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_ERRHANDLER_FREE = ompi_errhandler_free_f
#pragma weak pmpi_errhandler_free = ompi_errhandler_free_f
#pragma weak pmpi_errhandler_free_ = ompi_errhandler_free_f
#pragma weak pmpi_errhandler_free__ = ompi_errhandler_free_f

#pragma weak PMPI_Errhandler_free_f = ompi_errhandler_free_f
#pragma weak PMPI_Errhandler_free_f08 = ompi_errhandler_free_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ERRHANDLER_FREE,
                           pmpi_errhandler_free,
                           pmpi_errhandler_free_,
                           pmpi_errhandler_free__,
                           pompi_errhandler_free_f,
                           (MPI_Fint *errhandler, MPI_Fint *ierr),
                           (errhandler, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ERRHANDLER_FREE = ompi_errhandler_free_f
#pragma weak mpi_errhandler_free = ompi_errhandler_free_f
#pragma weak mpi_errhandler_free_ = ompi_errhandler_free_f
#pragma weak mpi_errhandler_free__ = ompi_errhandler_free_f

#pragma weak MPI_Errhandler_free_f = ompi_errhandler_free_f
#pragma weak MPI_Errhandler_free_f08 = ompi_errhandler_free_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ERRHANDLER_FREE,
                           mpi_errhandler_free,
                           mpi_errhandler_free_,
                           mpi_errhandler_free__,
                           ompi_errhandler_free_f,
                           (MPI_Fint *errhandler, MPI_Fint *ierr),
                           (errhandler, ierr) )
#else
#define ompi_errhandler_free_f pompi_errhandler_free_f
#endif
#endif


void ompi_errhandler_free_f(MPI_Fint *errhandler, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Errhandler c_errhandler;

    c_errhandler = PMPI_Errhandler_f2c(*errhandler);

    c_ierr = PMPI_Errhandler_free(&c_errhandler);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *errhandler = PMPI_Errhandler_c2f(c_errhandler);
    }
}
