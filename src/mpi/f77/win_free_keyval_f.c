/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_FREE_KEYVAL = mpi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval = mpi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval_ = mpi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval__ = mpi_win_free_keyval_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_FREE_KEYVAL,
                           pmpi_win_free_keyval,
                           pmpi_win_free_keyval_,
                           pmpi_win_free_keyval__,
                           pmpi_win_free_keyval_f,
                           (MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win_keyval, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_FREE_KEYVAL = mpi_win_free_keyval_f
#pragma weak mpi_win_free_keyval = mpi_win_free_keyval_f
#pragma weak mpi_win_free_keyval_ = mpi_win_free_keyval_f
#pragma weak mpi_win_free_keyval__ = mpi_win_free_keyval_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_FREE_KEYVAL,
                           mpi_win_free_keyval,
                           mpi_win_free_keyval_,
                           mpi_win_free_keyval__,
                           mpi_win_free_keyval_f,
                           (MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win_keyval, ierr) )
#endif

void mpi_win_free_keyval_f(MPI_Fint *win_keyval, MPI_Fint *ierr)
{

}
