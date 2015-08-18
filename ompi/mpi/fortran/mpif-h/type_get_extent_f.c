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
#pragma weak PMPI_TYPE_GET_EXTENT = ompi_type_get_extent_f
#pragma weak pmpi_type_get_extent = ompi_type_get_extent_f
#pragma weak pmpi_type_get_extent_ = ompi_type_get_extent_f
#pragma weak pmpi_type_get_extent__ = ompi_type_get_extent_f

#pragma weak PMPI_Type_get_extent_f = ompi_type_get_extent_f
#pragma weak PMPI_Type_get_extent_f08 = ompi_type_get_extent_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_EXTENT,
                           pmpi_type_get_extent,
                           pmpi_type_get_extent_,
                           pmpi_type_get_extent__,
                           pompi_type_get_extent_f,
                           (MPI_Fint *type, MPI_Aint *lb, MPI_Aint *extent, MPI_Fint *ierr),
                           (type, lb, extent, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_EXTENT = ompi_type_get_extent_f
#pragma weak mpi_type_get_extent = ompi_type_get_extent_f
#pragma weak mpi_type_get_extent_ = ompi_type_get_extent_f
#pragma weak mpi_type_get_extent__ = ompi_type_get_extent_f

#pragma weak MPI_Type_get_extent_f = ompi_type_get_extent_f
#pragma weak MPI_Type_get_extent_f08 = ompi_type_get_extent_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_EXTENT,
                           mpi_type_get_extent,
                           mpi_type_get_extent_,
                           mpi_type_get_extent__,
                           ompi_type_get_extent_f,
                           (MPI_Fint *type, MPI_Aint *lb, MPI_Aint *extent, MPI_Fint *ierr),
                           (type, lb, extent, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_get_extent_f(MPI_Fint *type, MPI_Aint *lb,
			   MPI_Aint *extent, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = MPI_Type_f2c(*type);

    c_ierr = MPI_Type_get_extent(c_type, lb, extent);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
