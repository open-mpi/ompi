/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_CART_GET = mpi_cart_get_f
#pragma weak pmpi_cart_get = mpi_cart_get_f
#pragma weak pmpi_cart_get_ = mpi_cart_get_f
#pragma weak pmpi_cart_get__ = mpi_cart_get_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_CART_GET,
                           pmpi_cart_get,
                           pmpi_cart_get_,
                           pmpi_cart_get__,
                           pmpi_cart_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, maxdims, dims, periods, coords, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_GET = mpi_cart_get_f
#pragma weak mpi_cart_get = mpi_cart_get_f
#pragma weak mpi_cart_get_ = mpi_cart_get_f
#pragma weak mpi_cart_get__ = mpi_cart_get_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_CART_GET,
                           mpi_cart_get,
                           mpi_cart_get_,
                           mpi_cart_get__,
                           mpi_cart_get_f,
                           (MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, maxdims, dims, periods, coords, ierr) )
#endif

void mpi_cart_get_f(MPI_Fint *comm, MPI_Fint *maxdims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *coords, MPI_Fint *ierr)
{

}
