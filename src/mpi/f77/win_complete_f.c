/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_COMPLETE = mpi_win_complete_f
#pragma weak pmpi_win_complete = mpi_win_complete_f
#pragma weak pmpi_win_complete_ = mpi_win_complete_f
#pragma weak pmpi_win_complete__ = mpi_win_complete_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_COMPLETE,
                           pmpi_win_complete,
                           pmpi_win_complete_,
                           pmpi_win_complete__,
                           pmpi_win_complete_f,
                           (MPI_Fint *win, MPI_Fint *ierr),
                           (win, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_COMPLETE = mpi_win_complete_f
#pragma weak mpi_win_complete = mpi_win_complete_f
#pragma weak mpi_win_complete_ = mpi_win_complete_f
#pragma weak mpi_win_complete__ = mpi_win_complete_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_COMPLETE,
                           mpi_win_complete,
                           mpi_win_complete_,
                           mpi_win_complete__,
                           mpi_win_complete_f,
                           (MPI_Fint *win, MPI_Fint *ierr),
                           (win, ierr) )
#endif

void mpi_win_complete_f(MPI_Fint *win, MPI_Fint *ierr)
{

}
