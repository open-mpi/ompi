/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_CART_MAP = mpi_cart_map_f
#pragma weak pmpi_cart_map = mpi_cart_map_f
#pragma weak pmpi_cart_map_ = mpi_cart_map_f
#pragma weak pmpi_cart_map__ = mpi_cart_map_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_CART_MAP,
                           pmpi_cart_map,
                           pmpi_cart_map_,
                           pmpi_cart_map__,
                           pmpi_cart_map_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, ndims, dims, periods, newrank, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_MAP = mpi_cart_map_f
#pragma weak mpi_cart_map = mpi_cart_map_f
#pragma weak mpi_cart_map_ = mpi_cart_map_f
#pragma weak mpi_cart_map__ = mpi_cart_map_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_CART_MAP,
                           mpi_cart_map,
                           mpi_cart_map_,
                           mpi_cart_map__,
                           mpi_cart_map_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *newrank, MPI_Fint *ierr),
                           (comm, ndims, dims, periods, newrank, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_cart_map_f(MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *periods, MPI_Fint *newrank, MPI_Fint *ierr)
{

}
