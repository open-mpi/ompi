/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_CART_CREATE = mpi_cart_create_f
#pragma weak pmpi_cart_create = mpi_cart_create_f
#pragma weak pmpi_cart_create_ = mpi_cart_create_f
#pragma weak pmpi_cart_create__ = mpi_cart_create_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_CART_CREATE,
                           pmpi_cart_create,
                           pmpi_cart_create_,
                           pmpi_cart_create__,
                           pmpi_cart_create_f,
                           (MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *redorder, MPI_Fint *comm_cart, MPI_Fint *ierr),
                           (old_comm, ndims, dims, periods, redorder, comm_cart, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_CREATE = mpi_cart_create_f
#pragma weak mpi_cart_create = mpi_cart_create_f
#pragma weak mpi_cart_create_ = mpi_cart_create_f
#pragma weak mpi_cart_create__ = mpi_cart_create_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_CART_CREATE,
                           mpi_cart_create,
                           mpi_cart_create_,
                           mpi_cart_create__,
                           mpi_cart_create_f,
                           (MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *redorder, MPI_Fint *comm_cart, MPI_Fint *ierr),
                           (old_comm, ndims, dims, periods, redorder, comm_cart, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_cart_create_f(MPI_Fint *old_comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *redorder, MPI_Fint *comm_cart, MPI_Fint *ierr)
{

}
