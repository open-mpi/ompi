/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_UNLOCK = mpi_win_unlock_f
#pragma weak pmpi_win_unlock = mpi_win_unlock_f
#pragma weak pmpi_win_unlock_ = mpi_win_unlock_f
#pragma weak pmpi_win_unlock__ = mpi_win_unlock_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_UNLOCK,
                           pmpi_win_unlock,
                           pmpi_win_unlock_,
                           pmpi_win_unlock__,
                           pmpi_win_unlock_f,
                           (MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr),
                           (rank, win, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_UNLOCK = mpi_win_unlock_f
#pragma weak mpi_win_unlock = mpi_win_unlock_f
#pragma weak mpi_win_unlock_ = mpi_win_unlock_f
#pragma weak mpi_win_unlock__ = mpi_win_unlock_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_UNLOCK,
                           mpi_win_unlock,
                           mpi_win_unlock_,
                           mpi_win_unlock__,
                           mpi_win_unlock_f,
                           (MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr),
                           (rank, win, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_win_unlock_f(MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr)
{

}
