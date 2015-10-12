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
#pragma weak PMPI_TYPE_EXTENT = ompi_type_extent_f
#pragma weak pmpi_type_extent = ompi_type_extent_f
#pragma weak pmpi_type_extent_ = ompi_type_extent_f
#pragma weak pmpi_type_extent__ = ompi_type_extent_f

#pragma weak PMPI_Type_extent_f = ompi_type_extent_f
#pragma weak PMPI_Type_extent_f08 = ompi_type_extent_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_EXTENT,
                           pmpi_type_extent,
                           pmpi_type_extent_,
                           pmpi_type_extent__,
                           pompi_type_extent_f,
                           (MPI_Fint *type, MPI_Fint *extent, MPI_Fint *ierr),
                           (type, extent, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_EXTENT = ompi_type_extent_f
#pragma weak mpi_type_extent = ompi_type_extent_f
#pragma weak mpi_type_extent_ = ompi_type_extent_f
#pragma weak mpi_type_extent__ = ompi_type_extent_f

#pragma weak MPI_Type_extent_f = ompi_type_extent_f
#pragma weak MPI_Type_extent_f08 = ompi_type_extent_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_EXTENT,
                           mpi_type_extent,
                           mpi_type_extent_,
                           mpi_type_extent__,
                           ompi_type_extent_f,
                           (MPI_Fint *type, MPI_Fint *extent, MPI_Fint *ierr),
                           (type, extent, ierr) )
#else
#define ompi_type_extent_f pompi_type_extent_f
#endif
#endif


void ompi_type_extent_f(MPI_Fint *type, MPI_Fint *extent, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = PMPI_Type_f2c(*type);
    MPI_Aint c_extent;

    c_ierr = PMPI_Type_extent(c_type, &c_extent);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *extent = (MPI_Fint)c_extent;
    }
}
