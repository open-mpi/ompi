/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_SIZE = mpi_comm_size_f
#pragma weak pmpi_comm_size = mpi_comm_size_f
#pragma weak pmpi_comm_size_ = mpi_comm_size_f
#pragma weak pmpi_comm_size__ = mpi_comm_size_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_SIZE,
                           pmpi_comm_size,
                           pmpi_comm_size_,
                           pmpi_comm_size__,
                           pmpi_comm_size_f,
                           (MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, size, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_SIZE = mpi_comm_size_f
#pragma weak mpi_comm_size = mpi_comm_size_f
#pragma weak mpi_comm_size_ = mpi_comm_size_f
#pragma weak mpi_comm_size__ = mpi_comm_size_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_SIZE,
                           mpi_comm_size,
                           mpi_comm_size_,
                           mpi_comm_size__,
                           mpi_comm_size_f,
                           (MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr),
                           (comm, size, ierr) )
#endif

void mpi_comm_size_f(MPI_Fint *comm, MPI_Fint *size, MPI_Fint *ierr)
{

}
