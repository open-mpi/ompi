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
#pragma weak PMPI_TYPE_GET_VALUE_INDEX = ompi_type_get_value_index_f
#pragma weak pmpi_type_get_value_index = ompi_type_get_value_index_f
#pragma weak pmpi_type_get_value_index_ = ompi_type_get_value_index_f
#pragma weak pmpi_type_get_value_index__ = ompi_type_get_value_index_f

#pragma weak PMPI_Type_get_value_index_f = ompi_type_get_value_index_f
#pragma weak PMPI_Type_get_value_index_f08 = ompi_type_get_value_index_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_VALUE_INDEX,
                           pmpi_type_get_value_index,
                           pmpi_type_get_value_index_,
                           pmpi_type_get_value_index__,
                           pompi_type_get_value_index_f,
                           (MPI_Fint *value_type, MPI_Fint *index_type, MPI_Fint *pair_type, MPI_Fint *ierr),
                           (value_type, index_type, pair_type, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_VALUE_INDEX = ompi_type_get_value_index_f
#pragma weak mpi_type_get_value_index = ompi_type_get_value_index_f
#pragma weak mpi_type_get_value_index_ = ompi_type_get_value_index_f
#pragma weak mpi_type_get_value_index__ = ompi_type_get_value_index_f

#pragma weak MPI_Type_get_value_index_f = ompi_type_get_value_index_f
#pragma weak MPI_Type_get_value_index_f08 = ompi_type_get_value_index_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_GET_VALUE_INDEX,
                           mpi_type_get_value_index,
                           mpi_type_get_value_index_,
                           mpi_type_get_value_index__,
                           ompi_type_get_value_index_f,
                           (MPI_Fint *value_type, MPI_Fint *index_type, MPI_Fint *pair_type, MPI_Fint *ierr),
                           (value_type, index_type, pair_type, ierr) )
#else
#define ompi_type_get_value_index_f pompi_type_get_value_index_f
#endif
#endif


void ompi_type_get_value_index_f(MPI_Fint *value_type, MPI_Fint *index_type, MPI_Fint *pair_type, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_value_type = PMPI_Type_f2c(*value_type);
    MPI_Datatype c_index_type = PMPI_Type_f2c(*index_type);
    MPI_Datatype c_new;

    c_ierr = PMPI_Type_get_value_index(c_value_type, c_index_type, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *pair_type = PMPI_Type_c2f(c_new);
    }
}
