/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_DUP = mpi_comm_dup_f
#pragma weak pmpi_comm_dup = mpi_comm_dup_f
#pragma weak pmpi_comm_dup_ = mpi_comm_dup_f
#pragma weak pmpi_comm_dup__ = mpi_comm_dup_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_DUP,
                           pmpi_comm_dup,
                           pmpi_comm_dup_,
                           pmpi_comm_dup__,
                           pmpi_comm_dup_f,
                           (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, newcomm, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_DUP = mpi_comm_dup_f
#pragma weak mpi_comm_dup = mpi_comm_dup_f
#pragma weak mpi_comm_dup_ = mpi_comm_dup_f
#pragma weak mpi_comm_dup__ = mpi_comm_dup_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_DUP,
                           mpi_comm_dup,
                           mpi_comm_dup_,
                           mpi_comm_dup__,
                           mpi_comm_dup_f,
                           (MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr),
                           (comm, newcomm, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_comm_dup_f(MPI_Fint *comm, MPI_Fint *newcomm, MPI_Fint *ierr)
{

}
