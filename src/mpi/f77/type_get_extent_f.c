/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_GET_EXTENT = mpi_type_get_extent_f
#pragma weak pmpi_type_get_extent = mpi_type_get_extent_f
#pragma weak pmpi_type_get_extent_ = mpi_type_get_extent_f
#pragma weak pmpi_type_get_extent__ = mpi_type_get_extent_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_GET_EXTENT,
                           pmpi_type_get_extent,
                           pmpi_type_get_extent_,
                           pmpi_type_get_extent__,
                           pmpi_type_get_extent_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *extent, MPI_Fint *ierr),
                           (type, lb, extent, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_GET_EXTENT = mpi_type_get_extent_f
#pragma weak mpi_type_get_extent = mpi_type_get_extent_f
#pragma weak mpi_type_get_extent_ = mpi_type_get_extent_f
#pragma weak mpi_type_get_extent__ = mpi_type_get_extent_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_GET_EXTENT,
                           mpi_type_get_extent,
                           mpi_type_get_extent_,
                           mpi_type_get_extent__,
                           mpi_type_get_extent_f,
                           (MPI_Fint *type, MPI_Fint *lb, MPI_Fint *extent, MPI_Fint *ierr),
                           (type, lb, extent, ierr) )
#endif

void mpi_type_get_extent_f(MPI_Fint *type, MPI_Fint *lb, MPI_Fint *extent, MPI_Fint *ierr)
{

}
