/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_TYPE_CREATE_DARRAY = mpi_type_create_darray_f
#pragma weak pmpi_type_create_darray = mpi_type_create_darray_f
#pragma weak pmpi_type_create_darray_ = mpi_type_create_darray_f
#pragma weak pmpi_type_create_darray__ = mpi_type_create_darray_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_TYPE_CREATE_DARRAY,
                           pmpi_type_create_darray,
                           pmpi_type_create_darray_,
                           pmpi_type_create_darray__,
                           pmpi_type_create_darray_f,
                           (MPI_Fint *size, MPI_Fint *rank, MPI_Fint *ndims, MPI_Fint *gsize_array, MPI_Fint *distrib_array, MPI_Fint *darg_array, MPI_Fint *psize_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (size, rank, ndims, gsize_array, distrib_array, darg_array, psize_array, order, oldtype, newtype, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_CREATE_DARRAY = mpi_type_create_darray_f
#pragma weak mpi_type_create_darray = mpi_type_create_darray_f
#pragma weak mpi_type_create_darray_ = mpi_type_create_darray_f
#pragma weak mpi_type_create_darray__ = mpi_type_create_darray_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_TYPE_CREATE_DARRAY,
                           mpi_type_create_darray,
                           mpi_type_create_darray_,
                           mpi_type_create_darray__,
                           mpi_type_create_darray_f,
                           (MPI_Fint *size, MPI_Fint *rank, MPI_Fint *ndims, MPI_Fint *gsize_array, MPI_Fint *distrib_array, MPI_Fint *darg_array, MPI_Fint *psize_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr),
                           (size, rank, ndims, gsize_array, distrib_array, darg_array, psize_array, order, oldtype, newtype, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_type_create_darray_f(MPI_Fint *size, MPI_Fint *rank, MPI_Fint *ndims, MPI_Fint *gsize_array, MPI_Fint *distrib_array, MPI_Fint *darg_array, MPI_Fint *psize_array, MPI_Fint *order, MPI_Fint *oldtype, MPI_Fint *newtype, MPI_Fint *ierr)
{
    MPI_Datatype c_old = MPI_Type_f2c(*oldtype);
    MPI_Datatype c_new;

    *ierr = MPI_Type_create_darray(*size, *rank, *ndims, gsize_array, 
                                   distrib_array, darg_array, psize_array,
                                   *order, c_old, &c_new);

    if (*ierr == MPI_SUCCESS) 
      *newtype = MPI_Type_c2f(c_new);
}
