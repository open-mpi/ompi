/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi/f77/bindings.h"
#include "errhandler/errhandler.h"
#include "communicator/communicator.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_STRUCT = mpi_type_create_struct_f
#pragma weak pmpi_type_create_struct = mpi_type_create_struct_f
#pragma weak pmpi_type_create_struct_ = mpi_type_create_struct_f
#pragma weak pmpi_type_create_struct__ = mpi_type_create_struct_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_STRUCT,
                           pmpi_type_create_struct,
                           pmpi_type_create_struct_,
                           pmpi_type_create_struct__,
                           pmpi_type_create_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_block_lengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_block_lengths, array_of_displacements, array_of_types, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_STRUCT = mpi_type_create_struct_f
#pragma weak mpi_type_create_struct = mpi_type_create_struct_f
#pragma weak mpi_type_create_struct_ = mpi_type_create_struct_f
#pragma weak mpi_type_create_struct__ = mpi_type_create_struct_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_STRUCT,
                           mpi_type_create_struct,
                           mpi_type_create_struct_,
                           mpi_type_create_struct__,
                           mpi_type_create_struct_f,
                           (MPI_Fint *count, MPI_Fint *array_of_block_lengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, array_of_block_lengths, array_of_displacements, array_of_types, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_TYPE_CREATE_STRUCT";


void mpi_type_create_struct_f(MPI_Fint *count, 
			      MPI_Fint *array_of_block_lengths,
			      MPI_Fint *array_of_displacements, 
			      MPI_Fint *array_of_types, MPI_Fint *newtype,
			      MPI_Fint *ierr)
{
    MPI_Datatype c_new;
    MPI_Datatype *c_type_old_array;
    MPI_Aint *c_disp_array;
    int i;
    OMPI_ARRAY_NAME_DECL(array_of_block_lengths);

    c_type_old_array = malloc(*count * (sizeof(MPI_Datatype) + 
                                        sizeof(MPI_Aint)));
    if (NULL == c_type_old_array) {
        *ierr = OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_NO_MEM,
                                       FUNC_NAME);
        return;
    }
    c_disp_array = (MPI_Aint*) c_type_old_array + *count;

    for (i = 0; i < *count; i++) {
        c_disp_array[i] = (MPI_Aint) array_of_displacements[i];
        c_type_old_array[i] = MPI_Type_f2c(array_of_types[i]);
    }

    OMPI_ARRAY_FINT_2_INT(array_of_block_lengths, *count);

    *ierr = OMPI_INT_2_FINT(MPI_Type_create_struct(OMPI_FINT_2_INT(*count),
			   OMPI_ARRAY_NAME_CONVERT(array_of_block_lengths),
                           c_disp_array, c_type_old_array, &c_new));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(array_of_block_lengths);

    if (MPI_SUCCESS == *ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
