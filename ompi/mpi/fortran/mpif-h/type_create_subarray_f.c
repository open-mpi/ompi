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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_SUBARRAY = ompi_type_create_subarray_f
#pragma weak pmpi_type_create_subarray = ompi_type_create_subarray_f
#pragma weak pmpi_type_create_subarray_ = ompi_type_create_subarray_f
#pragma weak pmpi_type_create_subarray__ = ompi_type_create_subarray_f

#pragma weak PMPI_Type_create_subarray_f = ompi_type_create_subarray_f
#pragma weak PMPI_Type_create_subarray_f08 = ompi_type_create_subarray_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_SUBARRAY,
                           pmpi_type_create_subarray,
                           pmpi_type_create_subarray_,
                           pmpi_type_create_subarray__,
                           pompi_type_create_subarray_f,
                           (MPI_Fint *ndims, MPI_Fint *size_array, MPI_Fint *subsize_array, MPI_Fint *start_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (ndims, size_array, subsize_array, start_array, order, oldtype, newtype, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_SUBARRAY = ompi_type_create_subarray_f
#pragma weak mpi_type_create_subarray = ompi_type_create_subarray_f
#pragma weak mpi_type_create_subarray_ = ompi_type_create_subarray_f
#pragma weak mpi_type_create_subarray__ = ompi_type_create_subarray_f

#pragma weak MPI_Type_create_subarray_f = ompi_type_create_subarray_f
#pragma weak MPI_Type_create_subarray_f08 = ompi_type_create_subarray_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_SUBARRAY,
                           mpi_type_create_subarray,
                           mpi_type_create_subarray_,
                           mpi_type_create_subarray__,
                           ompi_type_create_subarray_f,
                           (MPI_Fint *ndims, MPI_Fint *size_array, MPI_Fint *subsize_array, MPI_Fint *start_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (ndims, size_array, subsize_array, start_array, order, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_type_create_subarray_f(MPI_Fint *ndims, MPI_Fint *size_array,
				MPI_Fint *subsize_array, 
				MPI_Fint *start_array, MPI_Fint *order,
				MPI_Fint *oldtype, MPI_Fint *newtype, 
				MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Datatype c_old;
    MPI_Datatype c_new;
    OMPI_ARRAY_NAME_DECL(size_array);
    OMPI_ARRAY_NAME_DECL(subsize_array);
    OMPI_ARRAY_NAME_DECL(start_array);

    c_old = MPI_Type_f2c(*oldtype);

    OMPI_ARRAY_FINT_2_INT(size_array, *ndims);
    OMPI_ARRAY_FINT_2_INT(subsize_array, *ndims);
    OMPI_ARRAY_FINT_2_INT(start_array, *ndims);

    c_ierr = MPI_Type_create_subarray(OMPI_FINT_2_INT(*ndims),
                                      OMPI_ARRAY_NAME_CONVERT(size_array),
                                      OMPI_ARRAY_NAME_CONVERT(subsize_array),
                                      OMPI_ARRAY_NAME_CONVERT(start_array),
                                      *order, c_old, &c_new);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *newtype = MPI_Type_c2f(c_new);
    }

    OMPI_ARRAY_FINT_2_INT_CLEANUP(size_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(subsize_array);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(start_array);
}
