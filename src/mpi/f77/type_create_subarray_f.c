/*
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
#include "mpi/c/profile/defines.h"
#endif

void mpi_type_create_subarray_f(MPI_Fint *ndims, MPI_Fint *size_array, MPI_Fint *subsize_array, MPI_Fint *start_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr)
{

}
