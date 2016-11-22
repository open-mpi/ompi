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
#pragma weak PMPI_FINALIZE = ompi_finalize_f
#pragma weak pmpi_finalize = ompi_finalize_f
#pragma weak pmpi_finalize_ = ompi_finalize_f
#pragma weak pmpi_finalize__ = ompi_finalize_f

#pragma weak PMPI_Finalize_f = ompi_finalize_f
#pragma weak PMPI_Finalize_f08 = ompi_finalize_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_FINALIZE,
                           pmpi_finalize,
                           pmpi_finalize_,
                           pmpi_finalize__,
                           pompi_finalize_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FINALIZE = ompi_finalize_f
#pragma weak mpi_finalize = ompi_finalize_f
#pragma weak mpi_finalize_ = ompi_finalize_f
#pragma weak mpi_finalize__ = ompi_finalize_f

#pragma weak MPI_Finalize_f = ompi_finalize_f
#pragma weak MPI_Finalize_f08 = ompi_finalize_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_FINALIZE,
                           mpi_finalize,
                           mpi_finalize_,
                           mpi_finalize__,
                           ompi_finalize_f,
                           (MPI_Fint *ierr),
                           (ierr) )
#else
#define ompi_finalize_f pompi_finalize_f
#endif
#endif


void ompi_finalize_f(MPI_Fint *ierr)
{
    int c_ierr = PMPI_Finalize();
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
