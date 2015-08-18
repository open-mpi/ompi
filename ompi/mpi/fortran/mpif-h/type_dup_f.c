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
#pragma weak PMPI_TYPE_DUP = ompi_type_dup_f
#pragma weak pmpi_type_dup = ompi_type_dup_f
#pragma weak pmpi_type_dup_ = ompi_type_dup_f
#pragma weak pmpi_type_dup__ = ompi_type_dup_f

#pragma weak PMPI_Type_dup_f = ompi_type_dup_f
#pragma weak PMPI_Type_dup_f08 = ompi_type_dup_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_DUP,
                           pmpi_type_dup,
                           pmpi_type_dup_,
                           pmpi_type_dup__,
                           pompi_type_dup_f,
                           (MPI_Fint *type, MPI_Fint *newtype, MPI_Fint *ierr),
                           (type, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_DUP = ompi_type_dup_f
#pragma weak mpi_type_dup = ompi_type_dup_f
#pragma weak mpi_type_dup_ = ompi_type_dup_f
#pragma weak mpi_type_dup__ = ompi_type_dup_f

#pragma weak MPI_Type_dup_f = ompi_type_dup_f
#pragma weak MPI_Type_dup_f08 = ompi_type_dup_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_DUP,
                           mpi_type_dup,
                           mpi_type_dup_,
                           mpi_type_dup__,
                           ompi_type_dup_f,
                           (MPI_Fint *type, MPI_Fint *newtype, MPI_Fint *ierr),
                           (type, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_dup_f(MPI_Fint *type, MPI_Fint *newtype, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = MPI_Type_f2c(*type);
    MPI_Datatype c_new;

    c_ierr = MPI_Type_dup(c_type, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
