/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_TYPE_SIZE = mpi_type_size_f
#pragma weak pmpi_type_size = mpi_type_size_f
#pragma weak pmpi_type_size_ = mpi_type_size_f
#pragma weak pmpi_type_size__ = mpi_type_size_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_TYPE_SIZE,
                           pmpi_type_size,
                           pmpi_type_size_,
                           pmpi_type_size__,
                           pmpi_type_size_f,
                           (MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr),
                           (type, size, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_TYPE_SIZE = mpi_type_size_f
#pragma weak mpi_type_size = mpi_type_size_f
#pragma weak mpi_type_size_ = mpi_type_size_f
#pragma weak mpi_type_size__ = mpi_type_size_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_TYPE_SIZE,
                           mpi_type_size,
                           mpi_type_size_,
                           mpi_type_size__,
                           mpi_type_size_f,
                           (MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr),
                           (type, size, ierr) )
#endif

void mpi_type_size_f(MPI_Fint *type, MPI_Fint *size, MPI_Fint *ierr)
{

}
