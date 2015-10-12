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
#pragma weak PMPI_TYPE_INDEXED = ompi_type_indexed_f
#pragma weak pmpi_type_indexed = ompi_type_indexed_f
#pragma weak pmpi_type_indexed_ = ompi_type_indexed_f
#pragma weak pmpi_type_indexed__ = ompi_type_indexed_f

#pragma weak PMPI_Type_indexed_f = ompi_type_indexed_f
#pragma weak PMPI_Type_indexed_f08 = ompi_type_indexed_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_INDEXED,
                           pmpi_type_indexed,
                           pmpi_type_indexed_,
                           pmpi_type_indexed__,
                           pompi_type_indexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_INDEXED = ompi_type_indexed_f
#pragma weak mpi_type_indexed = ompi_type_indexed_f
#pragma weak mpi_type_indexed_ = ompi_type_indexed_f
#pragma weak mpi_type_indexed__ = ompi_type_indexed_f

#pragma weak MPI_Type_indexed_f = ompi_type_indexed_f
#pragma weak MPI_Type_indexed_f08 = ompi_type_indexed_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_INDEXED,
                           mpi_type_indexed,
                           mpi_type_indexed_,
                           mpi_type_indexed__,
                           ompi_type_indexed_f,
                           (MPI_Fint *count, MPI_Fint *array_of_blocklengths, MPI_Fint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierr) )
#else
#define ompi_type_indexed_f pompi_type_indexed_f
#endif
#endif


void ompi_type_indexed_f(MPI_Fint *count, MPI_Fint *array_of_blocklengths,
			MPI_Fint *array_of_displacements, MPI_Fint *oldtype,
			MPI_Fint *newtype, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_old = PMPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;
    OMPI_ARRAY_NAME_DECL(array_of_blocklengths);
    OMPI_ARRAY_NAME_DECL(array_of_displacements);

    OMPI_ARRAY_FINT_2_INT(array_of_blocklengths, *count);
    OMPI_ARRAY_FINT_2_INT(array_of_displacements, *count);

    c_ierr = PMPI_Type_indexed(OMPI_FINT_2_INT(*count),
                              OMPI_ARRAY_NAME_CONVERT(array_of_blocklengths),
                              OMPI_ARRAY_NAME_CONVERT(array_of_displacements),
                              c_old, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_blocklengths);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_displacements);

    if (MPI_SUCCESS == c_ierr) {
        *newtype = PMPI_Type_c2f(c_new);
    }
}
