/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_DIMS_CREATE = mpi_dims_create_f
#pragma weak pmpi_dims_create = mpi_dims_create_f
#pragma weak pmpi_dims_create_ = mpi_dims_create_f
#pragma weak pmpi_dims_create__ = mpi_dims_create_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_DIMS_CREATE,
                           pmpi_dims_create,
                           pmpi_dims_create_,
                           pmpi_dims_create__,
                           pmpi_dims_create_f,
                           (MPI_Fint *nnodes, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *ierr),
                           (nnodes, ndims, dims, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_DIMS_CREATE = mpi_dims_create_f
#pragma weak mpi_dims_create = mpi_dims_create_f
#pragma weak mpi_dims_create_ = mpi_dims_create_f
#pragma weak mpi_dims_create__ = mpi_dims_create_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_DIMS_CREATE,
                           mpi_dims_create,
                           mpi_dims_create_,
                           mpi_dims_create__,
                           mpi_dims_create_f,
                           (MPI_Fint *nnodes, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *ierr),
                           (nnodes, ndims, dims, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_dims_create_f(MPI_Fint *nnodes, MPI_Fint *ndims, MPI_Fint *dims, MPI_Fint *ierr)
{

}
