/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_SUBARRAY = mpi_type_create_subarray_f
#pragma weak pmpi_type_create_subarray = mpi_type_create_subarray_f
#pragma weak pmpi_type_create_subarray_ = mpi_type_create_subarray_f
#pragma weak pmpi_type_create_subarray__ = mpi_type_create_subarray_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_SUBARRAY,
                           pmpi_type_create_subarray,
                           pmpi_type_create_subarray_,
                           pmpi_type_create_subarray__,
                           pmpi_type_create_subarray_f,
                           (MPI_Fint *ndims, MPI_Fint *size_array, MPI_Fint *subsize_array, MPI_Fint *start_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (ndims, size_array, subsize_array, start_array, order, oldtype, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_SUBARRAY = mpi_type_create_subarray_f
#pragma weak mpi_type_create_subarray = mpi_type_create_subarray_f
#pragma weak mpi_type_create_subarray_ = mpi_type_create_subarray_f
#pragma weak mpi_type_create_subarray__ = mpi_type_create_subarray_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_SUBARRAY,
                           mpi_type_create_subarray,
                           mpi_type_create_subarray_,
                           mpi_type_create_subarray__,
                           mpi_type_create_subarray_f,
                           (MPI_Fint *ndims, MPI_Fint *size_array, MPI_Fint *subsize_array, MPI_Fint *start_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (ndims, size_array, subsize_array, start_array, order, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_create_subarray_f(MPI_Fint *ndims, MPI_Fint *size_array,
				MPI_Fint *subsize_array, 
				MPI_Fint *start_array, MPI_Fint *order,
				MPI_Fint *oldtype, MPI_Fint *newtype, 
				MPI_Fint *ierr)
{
    MPI_Datatype c_old;
    MPI_Datatype c_new;
    OMPI_ARRAY_NAME_DECL(size_array);
    OMPI_ARRAY_NAME_DECL(subsize_array);
    OMPI_ARRAY_NAME_DECL(start_array);

    c_old = MPI_Type_f2c(*oldtype);

    OMPI_ARRAY_FINT_2_INT(size_array, *ndims);
    OMPI_ARRAY_FINT_2_INT(subsize_array, *ndims);
    OMPI_ARRAY_FINT_2_INT(start_array, *ndims);

    *ierr = OMPI_INT_2_FINT(MPI_Type_create_subarray(OMPI_FINT_2_INT(*ndims),
				     OMPI_ARRAY_NAME_CONVERT(size_array),
                                     OMPI_ARRAY_NAME_CONVERT(subsize_array),
                                     OMPI_ARRAY_NAME_CONVERT(start_array),
                                     *order, c_old, &c_new));

    if (MPI_SUCCESS == *ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(size_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(subsize_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(start_array);
}
