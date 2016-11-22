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
#pragma weak PMPI_TYPE_SIZE = ompi_type_size_f
#pragma weak pmpi_type_size = ompi_type_size_f
#pragma weak pmpi_type_size_ = ompi_type_size_f
#pragma weak pmpi_type_size__ = ompi_type_size_f

#pragma weak PMPI_Type_size_f = ompi_type_size_f
#pragma weak PMPI_Type_size_f08 = ompi_type_size_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_SIZE,
                           pmpi_type_size,
                           pmpi_type_size_,
                           pmpi_type_size__,
                           pompi_type_size_f,
                           (MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr),
                           (type, size, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_SIZE = ompi_type_size_f
#pragma weak mpi_type_size = ompi_type_size_f
#pragma weak mpi_type_size_ = ompi_type_size_f
#pragma weak mpi_type_size__ = ompi_type_size_f

#pragma weak MPI_Type_size_f = ompi_type_size_f
#pragma weak MPI_Type_size_f08 = ompi_type_size_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_SIZE,
                           mpi_type_size,
                           mpi_type_size_,
                           mpi_type_size__,
                           ompi_type_size_f,
                           (MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr),
                           (type, size, ierr) )
#else
#define ompi_type_size_f pompi_type_size_f
#endif
#endif


void ompi_type_size_f(MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_type = PMPI_Type_f2c(*type);
    OMPI_SINGLE_NAME_DECL(size);

    c_ierr = PMPI_Type_size(c_type, OMPI_SINGLE_NAME_CONVERT(size));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(size);
    }
}
