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
 * Copyright (c) 2025      UT-Battelle, LLC.  All rights reserved.
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
#pragma weak PMPI_REMOVE_ERROR_CLASS = ompi_remove_error_class_f
#pragma weak pmpi_remove_error_class = ompi_remove_error_class_f
#pragma weak pmpi_remove_error_class_ = ompi_remove_error_class_f
#pragma weak pmpi_remove_error_class__ = ompi_remove_error_class_f

#pragma weak PMPI_Remove_error_class_f = ompi_remove_error_class_f
#pragma weak PMPI_Remove_error_class_f08 = ompi_remove_error_class_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_REMOVE_ERROR_CLASS,
                           pmpi_remove_error_class,
                           pmpi_remove_error_class_,
                           pmpi_remove_error_class__,
                           pompi_remove_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_REMOVE_ERROR_CLASS = ompi_remove_error_class_f
#pragma weak mpi_remove_error_class = ompi_remove_error_class_f
#pragma weak mpi_remove_error_class_ = ompi_remove_error_class_f
#pragma weak mpi_remove_error_class__ = ompi_remove_error_class_f

#pragma weak MPI_Remove_error_class_f = ompi_remove_error_class_f
#pragma weak MPI_Remove_error_class_f08 = ompi_remove_error_class_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_REMOVE_ERROR_CLASS,
                           mpi_remove_error_class,
                           mpi_remove_error_class_,
                           mpi_remove_error_class__,
                           ompi_remove_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#else
#define ompi_remove_error_class_f pompi_remove_error_class_f
#endif
#endif


void ompi_remove_error_class_f(MPI_Fint *errorclass, MPI_Fint *ierr)
{
    int ierr_c;

    ierr_c = PMPI_Remove_error_class(OMPI_FINT_2_INT(*errorclass));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);
}
