/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_FENCE = mpi_win_fence_f
#pragma weak pmpi_win_fence = mpi_win_fence_f
#pragma weak pmpi_win_fence_ = mpi_win_fence_f
#pragma weak pmpi_win_fence__ = mpi_win_fence_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_FENCE,
                           pmpi_win_fence,
                           pmpi_win_fence_,
                           pmpi_win_fence__,
                           pmpi_win_fence_f,
                           (MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (assert, win, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_FENCE = mpi_win_fence_f
#pragma weak mpi_win_fence = mpi_win_fence_f
#pragma weak mpi_win_fence_ = mpi_win_fence_f
#pragma weak mpi_win_fence__ = mpi_win_fence_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_FENCE,
                           mpi_win_fence,
                           mpi_win_fence_,
                           mpi_win_fence__,
                           mpi_win_fence_f,
                           (MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (assert, win, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_win_fence_f(MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr)
{

}
