/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_WAIT = mpi_win_wait_f
#pragma weak pmpi_win_wait = mpi_win_wait_f
#pragma weak pmpi_win_wait_ = mpi_win_wait_f
#pragma weak pmpi_win_wait__ = mpi_win_wait_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_WAIT,
                           pmpi_win_wait,
                           pmpi_win_wait_,
                           pmpi_win_wait__,
                           pmpi_win_wait_f,
                           (MPI_Fint *win, MPI_Fint *ierr),
                           (win, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_WAIT = mpi_win_wait_f
#pragma weak mpi_win_wait = mpi_win_wait_f
#pragma weak mpi_win_wait_ = mpi_win_wait_f
#pragma weak mpi_win_wait__ = mpi_win_wait_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_WAIT,
                           mpi_win_wait,
                           mpi_win_wait_,
                           mpi_win_wait__,
                           mpi_win_wait_f,
                           (MPI_Fint *win, MPI_Fint *ierr),
                           (win, ierr) )
#endif

void mpi_win_wait_f(MPI_Fint *win, MPI_Fint *ierr)
{

}
