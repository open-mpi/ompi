/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_CARTDIM_GET = mpi_cartdim_get_f
#pragma weak pmpi_cartdim_get = mpi_cartdim_get_f
#pragma weak pmpi_cartdim_get_ = mpi_cartdim_get_f
#pragma weak pmpi_cartdim_get__ = mpi_cartdim_get_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_CARTDIM_GET,
                           pmpi_cartdim_get,
                           pmpi_cartdim_get_,
                           pmpi_cartdim_get__,
                           pmpi_cartdim_get_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *ierr),
                           (comm, ndims, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_CARTDIM_GET = mpi_cartdim_get_f
#pragma weak mpi_cartdim_get = mpi_cartdim_get_f
#pragma weak mpi_cartdim_get_ = mpi_cartdim_get_f
#pragma weak mpi_cartdim_get__ = mpi_cartdim_get_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_CARTDIM_GET,
                           mpi_cartdim_get,
                           mpi_cartdim_get_,
                           mpi_cartdim_get__,
                           mpi_cartdim_get_f,
                           (MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *ierr),
                           (comm, ndims, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_cartdim_get_f(MPI_Fint *comm, MPI_Fint *ndims, MPI_Fint *ierr)
{

}
