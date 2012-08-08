/*
 * Copyright (c) 2012      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2012      Inria.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_HINDEXED_BLOCK = ompi_type_create_hindexed_block_f
#pragma weak pmpi_type_create_hindexed_block = ompi_type_create_hindexed_block_f
#pragma weak pmpi_type_create_hindexed_block_ = ompi_type_create_hindexed_block_f
#pragma weak pmpi_type_create_hindexed_block__ = ompi_type_create_hindexed_block_f

#pragma weak PMPI_Type_create_hindexed_block_f = ompi_type_create_hindexed_block_f
#pragma weak PMPI_Type_create_hindexed_block_f08 = ompi_type_create_hindexed_block_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_HINDEXED_BLOCK,
                           pmpi_type_create_hindexed_block,
                           pmpi_type_create_hindexed_block_,
                           pmpi_type_create_hindexed_block__,
                           pompi_type_create_hindexed_block_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Aint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, array_of_displacements, oldtype, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_HINDEXED_BLOCK = ompi_type_create_hindexed_block_f
#pragma weak mpi_type_create_hindexed_block = ompi_type_create_hindexed_block_f
#pragma weak mpi_type_create_hindexed_block_ = ompi_type_create_hindexed_block_f
#pragma weak mpi_type_create_hindexed_block__ = ompi_type_create_hindexed_block_f

#pragma weak MPI_Type_create_hindexed_block_f = ompi_type_create_hindexed_block_f
#pragma weak MPI_Type_create_hindexed_block_f08 = ompi_type_create_hindexed_block_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_HINDEXED_BLOCK,
                           mpi_type_create_hindexed_block,
                           mpi_type_create_hindexed_block_,
                           mpi_type_create_hindexed_block__,
                           ompi_type_create_hindexed_block_f,
                           (MPI_Fint *count, MPI_Fint *blocklength, MPI_Aint *array_of_displacements, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (count, blocklength, array_of_displacements, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_create_hindexed_block_f(MPI_Fint *count, MPI_Fint *blocklength,
				     MPI_Aint *array_of_displacements, 
				     MPI_Fint *oldtype, MPI_Fint *newtype,
				     MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;

    c_ierr = MPI_Type_create_hindexed_block(OMPI_FINT_2_INT(*count),
			OMPI_FINT_2_INT(*blocklength),
			array_of_displacements,
                        c_old, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    
    if (MPI_SUCCESS == c_ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }
}

