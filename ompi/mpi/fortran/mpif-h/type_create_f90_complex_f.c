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
#pragma weak PMPI_TYPE_CREATE_F90_COMPLEX = ompi_type_create_f90_complex_f
#pragma weak pmpi_type_create_f90_complex = ompi_type_create_f90_complex_f
#pragma weak pmpi_type_create_f90_complex_ = ompi_type_create_f90_complex_f
#pragma weak pmpi_type_create_f90_complex__ = ompi_type_create_f90_complex_f

#pragma weak PMPI_Type_create_f90_complex_f = ompi_type_create_f90_complex_f
#pragma weak PMPI_Type_create_f90_complex_f08 = ompi_type_create_f90_complex_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_F90_COMPLEX,
                           pmpi_type_create_f90_complex,
                           pmpi_type_create_f90_complex_,
                           pmpi_type_create_f90_complex__,
                           pompi_type_create_f90_complex_f,
                           (MPI_Fint *p, MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (p, r, newtype, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_F90_COMPLEX = ompi_type_create_f90_complex_f
#pragma weak mpi_type_create_f90_complex = ompi_type_create_f90_complex_f
#pragma weak mpi_type_create_f90_complex_ = ompi_type_create_f90_complex_f
#pragma weak mpi_type_create_f90_complex__ = ompi_type_create_f90_complex_f

#pragma weak MPI_Type_create_f90_complex_f = ompi_type_create_f90_complex_f
#pragma weak MPI_Type_create_f90_complex_f08 = ompi_type_create_f90_complex_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_F90_COMPLEX,
                           mpi_type_create_f90_complex,
                           mpi_type_create_f90_complex_,
                           mpi_type_create_f90_complex__,
                           ompi_type_create_f90_complex_f,
                           (MPI_Fint *p, MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (p, r, newtype, ierr) )
#else
#define ompi_type_create_f90_complex_f pompi_type_create_f90_complex_f
#endif
#endif


void ompi_type_create_f90_complex_f(MPI_Fint *p, MPI_Fint *r,
				   MPI_Fint *newtype, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_newtype;

    c_ierr = PMPI_Type_create_f90_complex(OMPI_FINT_2_INT(*p),
                                         OMPI_FINT_2_INT(*r),
                                         &c_newtype);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
      *newtype = PMPI_Type_c2f (c_newtype);
    }
}
