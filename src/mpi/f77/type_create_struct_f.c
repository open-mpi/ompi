/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
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


void mpi_type_create_struct_f(MPI_Fint *count, MPI_Fint *array_of_block_lengths, MPI_Fint *array_of_displacements, MPI_Fint *array_of_types, MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_new;
    MPI_Datatype *c_type_old_array;
    MPI_Aint *c_disp_array;
    int i;

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

    *ierr = MPI_Type_create_struct(*count, array_of_block_lengths,
                                   c_disp_array, c_type_old_array, &c_new);

    if (MPI_SUCCESS == *ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }
}
