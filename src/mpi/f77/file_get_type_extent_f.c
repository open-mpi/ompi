/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_FILE_GET_TYPE_EXTENT = mpi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent = mpi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent_ = mpi_file_get_type_extent_f
#pragma weak pmpi_file_get_type_extent__ = mpi_file_get_type_extent_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_FILE_GET_TYPE_EXTENT,
                           pmpi_file_get_type_extent,
                           pmpi_file_get_type_extent_,
                           pmpi_file_get_type_extent__,
                           pmpi_file_get_type_extent_f,
                           (MPI_Fint *fh, MPI_Fint *datatype, MPI_Fint *extent, MPI_Fint *ierr),
                           (fh, datatype, extent, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_FILE_GET_TYPE_EXTENT = mpi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent = mpi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent_ = mpi_file_get_type_extent_f
#pragma weak mpi_file_get_type_extent__ = mpi_file_get_type_extent_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_FILE_GET_TYPE_EXTENT,
                           mpi_file_get_type_extent,
                           mpi_file_get_type_extent_,
                           mpi_file_get_type_extent__,
                           mpi_file_get_type_extent_f,
                           (MPI_Fint *fh, MPI_Fint *datatype, MPI_Fint *extent, MPI_Fint *ierr),
                           (fh, datatype, extent, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_file_get_type_extent_f(MPI_Fint *fh, MPI_Fint *datatype, MPI_Fint *extent, MPI_Fint *ierr)
{

}
