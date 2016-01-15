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
#pragma weak PMPI_INITIALIZED = ompi_initialized_f
#pragma weak pmpi_initialized = ompi_initialized_f
#pragma weak pmpi_initialized_ = ompi_initialized_f
#pragma weak pmpi_initialized__ = ompi_initialized_f

#pragma weak PMPI_Initialized_f = ompi_initialized_f
#pragma weak PMPI_Initialized_f08 = ompi_initialized_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INITIALIZED,
                           pmpi_initialized,
                           pmpi_initialized_,
                           pmpi_initialized__,
                           pompi_initialized_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INITIALIZED = ompi_initialized_f
#pragma weak mpi_initialized = ompi_initialized_f
#pragma weak mpi_initialized_ = ompi_initialized_f
#pragma weak mpi_initialized__ = ompi_initialized_f

#pragma weak MPI_Initialized_f = ompi_initialized_f
#pragma weak MPI_Initialized_f08 = ompi_initialized_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INITIALIZED,
                           mpi_initialized,
                           mpi_initialized_,
                           mpi_initialized__,
                           ompi_initialized_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#else
#define ompi_initialized_f pompi_initialized_f
#endif
#endif


void ompi_initialized_f(ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_LOGICAL_NAME_DECL(flag);
    c_ierr = PMPI_Initialized(OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
