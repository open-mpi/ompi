/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
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
#pragma weak PMPI_TYPE_GET_TRUE_EXTENT_X = ompi_type_get_true_extent_x_f
#pragma weak pmpi_type_get_true_extent_x = ompi_type_get_true_extent_x_f
#pragma weak pmpi_type_get_true_extent_x_ = ompi_type_get_true_extent_x_f
#pragma weak pmpi_type_get_true_extent_x__ = ompi_type_get_true_extent_x_f

#pragma weak PMPI_Type_get_true_extent_x_f = ompi_type_get_true_extent_x_f
#pragma weak PMPI_Type_get_true_extent_x_f08 = ompi_type_get_true_extent_x_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_TRUE_EXTENT_X,
                           pmpi_type_get_true_extent_x,
                           pmpi_type_get_true_extent_x_,
                           pmpi_type_get_true_extent_x__,
                           pompi_type_get_true_extent_x_f,
                           (MPI_Fint *datatype, MPI_Count *true_lb, MPI_Count *true_extent, MPI_Fint *ierr),
                           (datatype, true_lb, true_extent, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_TRUE_EXTENT_X = ompi_type_get_true_extent_x_f
#pragma weak mpi_type_get_true_extent_x = ompi_type_get_true_extent_x_f
#pragma weak mpi_type_get_true_extent_x_ = ompi_type_get_true_extent_x_f
#pragma weak mpi_type_get_true_extent_x__ = ompi_type_get_true_extent_x_f

#pragma weak MPI_Type_get_true_extent_x_f = ompi_type_get_true_extent_x_f
#pragma weak MPI_Type_get_true_extent_x_f08 = ompi_type_get_true_extent_x_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_TRUE_EXTENT_X,
                           mpi_type_get_true_extent_x,
                           mpi_type_get_true_extent_x_,
                           mpi_type_get_true_extent_x__,
                           ompi_type_get_true_extent_x_f,
                           (MPI_Fint *datatype, MPI_Count *true_lb, MPI_Count *true_extent, MPI_Fint *ierr),
                           (datatype, true_lb, true_extent, ierr) )
#else
#define ompi_type_get_true_extent_x_f pompi_type_get_true_extent_x_f
#endif
#endif


void ompi_type_get_true_extent_x_f(MPI_Fint *datatype, MPI_Count *true_lb, MPI_Count *true_extent, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = PMPI_Type_f2c(*datatype);

    c_ierr = PMPI_Type_get_true_extent_x(c_type, true_lb, true_extent);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
