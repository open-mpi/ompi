/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_CART_RANK = mpi_cart_rank_f
#pragma weak pmpi_cart_rank = mpi_cart_rank_f
#pragma weak pmpi_cart_rank_ = mpi_cart_rank_f
#pragma weak pmpi_cart_rank__ = mpi_cart_rank_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_CART_RANK,
                           pmpi_cart_rank,
                           pmpi_cart_rank_,
                           pmpi_cart_rank__,
                           pmpi_cart_rank_f,
                           (MPI_Fint *comm, MPI_Fint *coords, MPI_Fint *rank, MPI_Fint *ierr),
                           (comm, coords, rank, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CART_RANK = mpi_cart_rank_f
#pragma weak mpi_cart_rank = mpi_cart_rank_f
#pragma weak mpi_cart_rank_ = mpi_cart_rank_f
#pragma weak mpi_cart_rank__ = mpi_cart_rank_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_CART_RANK,
                           mpi_cart_rank,
                           mpi_cart_rank_,
                           mpi_cart_rank__,
                           mpi_cart_rank_f,
                           (MPI_Fint *comm, MPI_Fint *coords, MPI_Fint *rank, MPI_Fint *ierr),
                           (comm, coords, rank, ierr) )
#endif

void mpi_cart_rank_f(MPI_Fint *comm, MPI_Fint *coords, MPI_Fint *rank, MPI_Fint *ierr)
{

}
