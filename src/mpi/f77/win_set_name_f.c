/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_SET_NAME = mpi_win_set_name_f
#pragma weak pmpi_win_set_name = mpi_win_set_name_f
#pragma weak pmpi_win_set_name_ = mpi_win_set_name_f
#pragma weak pmpi_win_set_name__ = mpi_win_set_name_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_SET_NAME,
                           pmpi_win_set_name,
                           pmpi_win_set_name_,
                           pmpi_win_set_name__,
                           pmpi_win_set_name_f,
                           (MPI_Fint *win, char *win_name, MPI_Fint *ierr),
                           (win, win_name, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_SET_NAME = mpi_win_set_name_f
#pragma weak mpi_win_set_name = mpi_win_set_name_f
#pragma weak mpi_win_set_name_ = mpi_win_set_name_f
#pragma weak mpi_win_set_name__ = mpi_win_set_name_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_SET_NAME,
                           mpi_win_set_name,
                           mpi_win_set_name_,
                           mpi_win_set_name__,
                           mpi_win_set_name_f,
                           (MPI_Fint *win, char *win_name, MPI_Fint *ierr),
                           (win, win_name, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_win_set_name_f(MPI_Fint *win, char *win_name, MPI_Fint *ierr)
{

}
