/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_CART_MAP = mpi_cart_map_f
#pragma weak pmpi_cart_map = mpi_cart_map_f
#pragma weak pmpi_cart_map_ = mpi_cart_map_f
#pragma weak pmpi_cart_map__ = mpi_cart_map_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_CART_MAP,
                           pmpi_cart_map,
                           pmpi_cart_map_,
                           pmpi_cart_map__,
                           pmpi_cart_map_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, ndims, dims, periods, newrank, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_MAP = mpi_cart_map_f
#pragma weak mpi_cart_map = mpi_cart_map_f
#pragma weak mpi_cart_map_ = mpi_cart_map_f
#pragma weak mpi_cart_map__ = mpi_cart_map_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_CART_MAP,
                           mpi_cart_map,
                           mpi_cart_map_,
                           mpi_cart_map__,
                           mpi_cart_map_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, ndims, dims, periods, newrank, ierr) )
#endif

void mpi_cart_map_f(MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *newrank, MPI_Fint *ierr)
{

}
