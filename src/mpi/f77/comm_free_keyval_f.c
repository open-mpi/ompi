/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_COMM_FREE_KEYVAL = mpi_comm_free_keyval_f
#pragma weak pmpi_comm_free_keyval = mpi_comm_free_keyval_f
#pragma weak pmpi_comm_free_keyval_ = mpi_comm_free_keyval_f
#pragma weak pmpi_comm_free_keyval__ = mpi_comm_free_keyval_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_COMM_FREE_KEYVAL,
                           pmpi_comm_free_keyval,
                           pmpi_comm_free_keyval_,
                           pmpi_comm_free_keyval__,
                           pmpi_comm_free_keyval_f,
                           (MPI_Fint *comm_keyval, MPI_Fint *ierr),
                           (comm_keyval, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_COMM_FREE_KEYVAL = mpi_comm_free_keyval_f
#pragma weak mpi_comm_free_keyval = mpi_comm_free_keyval_f
#pragma weak mpi_comm_free_keyval_ = mpi_comm_free_keyval_f
#pragma weak mpi_comm_free_keyval__ = mpi_comm_free_keyval_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_COMM_FREE_KEYVAL,
                           mpi_comm_free_keyval,
                           mpi_comm_free_keyval_,
                           mpi_comm_free_keyval__,
                           mpi_comm_free_keyval_f,
                           (MPI_Fint *comm_keyval, MPI_Fint *ierr),
                           (comm_keyval, ierr) )
#endif

void mpi_comm_free_keyval_f(MPI_Fint *comm_keyval, MPI_Fint *ierr)
{

}
