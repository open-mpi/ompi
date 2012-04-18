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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_F90_INTEGER = ompi_type_create_f90_integer_f
#pragma weak pmpi_type_create_f90_integer = ompi_type_create_f90_integer_f
#pragma weak pmpi_type_create_f90_integer_ = ompi_type_create_f90_integer_f
#pragma weak pmpi_type_create_f90_integer__ = ompi_type_create_f90_integer_f

#pragma weak PMPI_Type_create_f90_integer_f = ompi_type_create_f90_integer_f
#pragma weak PMPI_Type_create_f90_integer_f08 = ompi_type_create_f90_integer_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_F90_INTEGER,
                           pmpi_type_create_f90_integer,
                           pmpi_type_create_f90_integer_,
                           pmpi_type_create_f90_integer__,
                           pompi_type_create_f90_integer_f,
                           (MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (r, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_F90_INTEGER = ompi_type_create_f90_integer_f
#pragma weak mpi_type_create_f90_integer = ompi_type_create_f90_integer_f
#pragma weak mpi_type_create_f90_integer_ = ompi_type_create_f90_integer_f
#pragma weak mpi_type_create_f90_integer__ = ompi_type_create_f90_integer_f

#pragma weak MPI_Type_create_f90_integer_f = ompi_type_create_f90_integer_f
#pragma weak MPI_Type_create_f90_integer_f08 = ompi_type_create_f90_integer_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_F90_INTEGER,
                           mpi_type_create_f90_integer,
                           mpi_type_create_f90_integer_,
                           mpi_type_create_f90_integer__,
                           ompi_type_create_f90_integer_f,
                           (MPI_Fint *r, MPI_Fint *newtype, MPI_Fint *ierr),
                           (r, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_create_f90_integer_f(MPI_Fint *r, MPI_Fint *newtype,
				   MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_new = MPI_Type_f2c(*newtype);

    c_ierr = MPI_Type_create_f90_integer(OMPI_FINT_2_INT(*r), &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
      *newtype = MPI_Type_c2f(c_new);
    }
}
