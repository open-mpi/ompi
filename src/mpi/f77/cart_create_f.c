/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_CART_CREATE = mpi_cart_create_f
#pragma weak pmpi_cart_create = mpi_cart_create_f
#pragma weak pmpi_cart_create_ = mpi_cart_create_f
#pragma weak pmpi_cart_create__ = mpi_cart_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_CREATE,
                           pmpi_cart_create,
                           pmpi_cart_create_,
                           pmpi_cart_create__,
                           pmpi_cart_create_f,
                           (MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *reorder, MPI_Fint *comm_cart, MPI_Fint *ierr),
                           (old_comm, ndims, dims, periods, reorder, comm_cart, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_CREATE = mpi_cart_create_f
#pragma weak mpi_cart_create = mpi_cart_create_f
#pragma weak mpi_cart_create_ = mpi_cart_create_f
#pragma weak mpi_cart_create__ = mpi_cart_create_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_CREATE,
                           mpi_cart_create,
                           mpi_cart_create_,
                           mpi_cart_create__,
                           mpi_cart_create_f,
                           (MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *reorder, MPI_Fint *comm_cart, MPI_Fint *ierr),
                           (old_comm, ndims, dims, periods, reorder, comm_cart, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_cart_create_f(MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims,
		       MPI_Fint *periods, MPI_Fint *reorder,
		       MPI_Fint *comm_cart, MPI_Fint *ierr)
{
    MPI_Comm c_comm1, c_comm2;
    int size;
    OMPI_ARRAY_NAME_DECL(dims);
    OMPI_ARRAY_NAME_DECL(periods);

    c_comm1 = MPI_Comm_f2c(*old_comm);
    c_comm2 = MPI_Comm_f2c(*comm_cart);

    size = OMPI_FINT_2_INT(*ndims);
    OMPI_ARRAY_FINT_2_INT(dims, size);
    OMPI_ARRAY_FINT_2_INT(periods, size);

    *ierr = OMPI_INT_2_FINT(MPI_Cart_create(c_comm1, OMPI_FINT_2_INT(*ndims),
					    OMPI_ARRAY_NAME_CONVERT(dims),
					    OMPI_ARRAY_NAME_CONVERT(periods), 
					    OMPI_FINT_2_INT(*reorder),
					    &c_comm2));

    OMPI_ARRAY_FINT_2_INT_CLEANUP(dims);
    OMPI_ARRAY_FINT_2_INT_CLEANUP(periods);
}
