/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_BCAST = mpi_bcast_f
#pragma weak pmpi_bcast = mpi_bcast_f
#pragma weak pmpi_bcast_ = mpi_bcast_f
#pragma weak pmpi_bcast__ = mpi_bcast_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_BCAST,
                           pmpi_bcast,
                           pmpi_bcast_,
                           pmpi_bcast__,
                           pmpi_bcast_f,
                           (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (buffer, count, datatype, root, comm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_BCAST = mpi_bcast_f
#pragma weak mpi_bcast = mpi_bcast_f
#pragma weak mpi_bcast_ = mpi_bcast_f
#pragma weak mpi_bcast__ = mpi_bcast_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_BCAST,
                           mpi_bcast,
                           mpi_bcast_,
                           mpi_bcast__,
                           mpi_bcast_f,
                           (char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr),
                           (buffer, count, datatype, root, comm, ierr) )
#endif

void mpi_bcast_f(char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr)
{

}
