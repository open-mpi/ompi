/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_TEST = mpi_win_test_f
#pragma weak pmpi_win_test = mpi_win_test_f
#pragma weak pmpi_win_test_ = mpi_win_test_f
#pragma weak pmpi_win_test__ = mpi_win_test_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_TEST,
                           pmpi_win_test,
                           pmpi_win_test_,
                           pmpi_win_test__,
                           pmpi_win_test_f,
                           (MPI_Fint *win, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_TEST = mpi_win_test_f
#pragma weak mpi_win_test = mpi_win_test_f
#pragma weak mpi_win_test_ = mpi_win_test_f
#pragma weak mpi_win_test__ = mpi_win_test_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_TEST,
                           mpi_win_test,
                           mpi_win_test_,
                           mpi_win_test__,
                           mpi_win_test_f,
                           (MPI_Fint *win, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_test_f(MPI_Fint *win, MPI_Fint *flag, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c(*win);
    OMPI_SINGLE_NAME_DECL(flag);

    *ierr = OMPI_INT_2_FINT(MPI_Win_test(c_win,
					 OMPI_SINGLE_NAME_CONVERT(flag)));
    
    OMPI_SINGLE_INT_2_FINT(flag);
}
