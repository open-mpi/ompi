/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_LOCK = mpi_win_lock_f
#pragma weak pmpi_win_lock = mpi_win_lock_f
#pragma weak pmpi_win_lock_ = mpi_win_lock_f
#pragma weak pmpi_win_lock__ = mpi_win_lock_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_LOCK,
                           pmpi_win_lock,
                           pmpi_win_lock_,
                           pmpi_win_lock__,
                           pmpi_win_lock_f,
                           (MPI_Fint *lock_type, MPI_Fint *rank, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (lock_type, rank, assert, win, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_LOCK = mpi_win_lock_f
#pragma weak mpi_win_lock = mpi_win_lock_f
#pragma weak mpi_win_lock_ = mpi_win_lock_f
#pragma weak mpi_win_lock__ = mpi_win_lock_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_LOCK,
                           mpi_win_lock,
                           mpi_win_lock_,
                           mpi_win_lock__,
                           mpi_win_lock_f,
                           (MPI_Fint *lock_type, MPI_Fint *rank, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (lock_type, rank, assert, win, ierr) )
#endif

void mpi_win_lock_f(MPI_Fint *lock_type, MPI_Fint *rank, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr)
{

}
