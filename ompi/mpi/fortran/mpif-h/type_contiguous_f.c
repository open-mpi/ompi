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
#pragma weak PMPI_TYPE_CONTIGUOUS = ompi_type_contiguous_f
#pragma weak pmpi_type_contiguous = ompi_type_contiguous_f
#pragma weak pmpi_type_contiguous_ = ompi_type_contiguous_f
#pragma weak pmpi_type_contiguous__ = ompi_type_contiguous_f

#pragma weak PMPI_Type_contiguous_f = ompi_type_contiguous_f
#pragma weak PMPI_Type_contiguous_f08 = ompi_type_contiguous_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CONTIGUOUS,
                           pmpi_type_contiguous,
                           pmpi_type_contiguous_,
                           pmpi_type_contiguous__,
                           pompi_type_contiguous_f,
                           (MPI_Fint *count, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, oldtype, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CONTIGUOUS = ompi_type_contiguous_f
#pragma weak mpi_type_contiguous = ompi_type_contiguous_f
#pragma weak mpi_type_contiguous_ = ompi_type_contiguous_f
#pragma weak mpi_type_contiguous__ = ompi_type_contiguous_f

#pragma weak MPI_Type_contiguous_f = ompi_type_contiguous_f
#pragma weak MPI_Type_contiguous_f08 = ompi_type_contiguous_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CONTIGUOUS,
                           mpi_type_contiguous,
                           mpi_type_contiguous_,
                           mpi_type_contiguous__,
                           ompi_type_contiguous_f,
                           (MPI_Fint *count, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_contiguous_f(MPI_Fint *count, MPI_Fint *oldtype,
			   MPI_Fint *newtype, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;

    c_ierr = MPI_Type_contiguous(OMPI_FINT_2_INT(*count), c_old, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
