/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_CART_COORDS = mpi_cart_coords_f
#pragma weak pmpi_cart_coords = mpi_cart_coords_f
#pragma weak pmpi_cart_coords_ = mpi_cart_coords_f
#pragma weak pmpi_cart_coords__ = mpi_cart_coords_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_COORDS,
                           pmpi_cart_coords,
                           pmpi_cart_coords_,
                           pmpi_cart_coords__,
                           pmpi_cart_coords_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, rank, maxdims, coords, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_COORDS = mpi_cart_coords_f
#pragma weak mpi_cart_coords = mpi_cart_coords_f
#pragma weak mpi_cart_coords_ = mpi_cart_coords_f
#pragma weak mpi_cart_coords__ = mpi_cart_coords_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_COORDS,
                           mpi_cart_coords,
                           mpi_cart_coords_,
                           mpi_cart_coords__,
                           mpi_cart_coords_f,
                           (MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims, MPI_Fint *coords, MPI_Fint *ierr),
                           (comm, rank, maxdims, coords, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_cart_coords_f(MPI_Fint *comm, MPI_Fint *rank, MPI_Fint *maxdims,
		       MPI_Fint *coords, MPI_Fint *ierr)
{
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c(*comm);
    
    *ierr = MPI_Cart_coords(c_comm, *rank, *maxdims, coords);
}
