/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_START = mpi_win_start_f
#pragma weak pmpi_win_start = mpi_win_start_f
#pragma weak pmpi_win_start_ = mpi_win_start_f
#pragma weak pmpi_win_start__ = mpi_win_start_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_START,
                           pmpi_win_start,
                           pmpi_win_start_,
                           pmpi_win_start__,
                           pmpi_win_start_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_START = mpi_win_start_f
#pragma weak mpi_win_start = mpi_win_start_f
#pragma weak mpi_win_start_ = mpi_win_start_f
#pragma weak mpi_win_start__ = mpi_win_start_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_START,
                           mpi_win_start,
                           mpi_win_start_,
                           mpi_win_start__,
                           mpi_win_start_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_win_start_f(MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr)
{

}
