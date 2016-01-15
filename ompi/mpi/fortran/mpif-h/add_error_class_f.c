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
#pragma weak PMPI_ADD_ERROR_CLASS = ompi_add_error_class_f
#pragma weak pmpi_add_error_class = ompi_add_error_class_f
#pragma weak pmpi_add_error_class_ = ompi_add_error_class_f
#pragma weak pmpi_add_error_class__ = ompi_add_error_class_f

#pragma weak PMPI_Add_error_class_f = ompi_add_error_class_f
#pragma weak PMPI_Add_error_class_f08 = ompi_add_error_class_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_ADD_ERROR_CLASS,
                           pmpi_add_error_class,
                           pmpi_add_error_class_,
                           pmpi_add_error_class__,
                           pompi_add_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_ADD_ERROR_CLASS = ompi_add_error_class_f
#pragma weak mpi_add_error_class = ompi_add_error_class_f
#pragma weak mpi_add_error_class_ = ompi_add_error_class_f
#pragma weak mpi_add_error_class__ = ompi_add_error_class_f

#pragma weak MPI_Add_error_class_f = ompi_add_error_class_f
#pragma weak MPI_Add_error_class_f08 = ompi_add_error_class_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_ADD_ERROR_CLASS,
                           mpi_add_error_class,
                           mpi_add_error_class_,
                           mpi_add_error_class__,
                           ompi_add_error_class_f,
                           (MPI_Fint *errorclass, MPI_Fint *ierr),
                           (errorclass, ierr) )
#else
#define ompi_add_error_class_f pompi_add_error_class_f
#endif
#endif


void ompi_add_error_class_f(MPI_Fint *errorclass, MPI_Fint *ierr)
{
    int ierr_c;
    OMPI_SINGLE_NAME_DECL(errorclass);

    ierr_c = PMPI_Add_error_class(OMPI_SINGLE_NAME_CONVERT(errorclass));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(ierr_c);

    if (MPI_SUCCESS == ierr_c) {
        OMPI_SINGLE_INT_2_FINT(errorclass);
    }
}
