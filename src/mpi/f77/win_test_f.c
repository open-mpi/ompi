/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_TEST = mpi_win_test_f
#pragma weak pmpi_win_test = mpi_win_test_f
#pragma weak pmpi_win_test_ = mpi_win_test_f
#pragma weak pmpi_win_test__ = mpi_win_test_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_TEST,
                           pmpi_win_test,
                           pmpi_win_test_,
                           pmpi_win_test__,
                           pmpi_win_test_f,
                           (MPI_Fint *win, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_TEST = mpi_win_test_f
#pragma weak mpi_win_test = mpi_win_test_f
#pragma weak mpi_win_test_ = mpi_win_test_f
#pragma weak mpi_win_test__ = mpi_win_test_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_TEST,
                           mpi_win_test,
                           mpi_win_test_,
                           mpi_win_test__,
                           mpi_win_test_f,
                           (MPI_Fint *win, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, flag, ierr) )
#endif

void mpi_win_test_f(MPI_Fint *win, MPI_Fint *flag, MPI_Fint *ierr)
{

}
