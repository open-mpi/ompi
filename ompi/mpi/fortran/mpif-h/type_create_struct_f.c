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
#include "ompi/errhandler/errhandler.h"
#include "ompi/communicator/communicator.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_TYPE_CREATE_STRUCT = ompi_type_create_struct_f
#pragma weak pmpi_type_create_struct = ompi_type_create_struct_f
#pragma weak pmpi_type_create_struct_ = ompi_type_create_struct_f
#pragma weak pmpi_type_create_struct__ = ompi_type_create_struct_f

#pragma weak PMPI_Type_create_struct_f = ompi_type_create_struct_f
#pragma weak PMPI_Type_create_struct_f08 = ompi_type_create_struct_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_STRUCT,
                           pmpi_type_create_struct,
                           pmpi_type_create_struct_,
                           pmpi_type_create_struct__,
                           pompi_type_create_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_block_lengths, MPI_Aint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_block_lengths, array_of_displacements, array_of_types, newtype, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_STRUCT = ompi_type_create_struct_f
#pragma weak mpi_type_create_struct = ompi_type_create_struct_f
#pragma weak mpi_type_create_struct_ = ompi_type_create_struct_f
#pragma weak mpi_type_create_struct__ = ompi_type_create_struct_f

#pragma weak MPI_Type_create_struct_f = ompi_type_create_struct_f
#pragma weak MPI_Type_create_struct_f08 = ompi_type_create_struct_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_STRUCT,
                           mpi_type_create_struct,
                           mpi_type_create_struct_,
                           mpi_type_create_struct__,
                           ompi_type_create_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_block_lengths, MPI_Aint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_block_lengths, array_of_displacements, array_of_types, newtype, ierr) )
#else
#define ompi_type_create_struct_f pompi_type_create_struct_f
#endif
#endif


static const char FUNC_NAME[] = "MPI_TYPE_CREATE_STRUCT";


void ompi_type_create_struct_f(MPI_Fint *count,
			      MPI_Fint *array_of_block_lengths,
			      MPI_Aint *array_of_displacements,
			      MPI_Fint *array_of_types, MPI_Fint *newtype,
			      MPI_Fint *ierr)
{
    MPI_Datatype c_new;
    MPI_Datatype *c_type_old_array;
    int i, c_ierr;
    OMPI_ARRAY_NAME_DECL(array_of_block_lengths);

    c_type_old_array = (MPI_Datatype *) malloc(*count * sizeof(MPI_Datatype));
    if (NULL == c_type_old_array) {
        c_ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                        FUNC_NAME);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
        return;
    }

    for (i = 0; i < *count; i++) {
        c_type_old_array[i] = PMPI_Type_f2c(array_of_types[i]);
    }

    OMPI_ARRAY_FINT_2_INT(array_of_block_lengths, *count);

    c_ierr = PMPI_Type_create_struct(OMPI_FINT_2_INT(*count),
			   OMPI_ARRAY_NAME_CONVERT(array_of_block_lengths),
                           array_of_displacements, c_type_old_array, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_block_lengths);

    if (MPI_SUCCESS == c_ierr) {
        *newtype = PMPI_Type_c2f(c_new);
    }

    free(c_type_old_array);
}
