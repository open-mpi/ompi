/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_UNLOCK = mpi_win_unlock_f
#pragma weak pmpi_win_unlock = mpi_win_unlock_f
#pragma weak pmpi_win_unlock_ = mpi_win_unlock_f
#pragma weak pmpi_win_unlock__ = mpi_win_unlock_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_UNLOCK,
                           pmpi_win_unlock,
                           pmpi_win_unlock_,
                           pmpi_win_unlock__,
                           pmpi_win_unlock_f,
                           (MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr),
                           (rank, win, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_UNLOCK = mpi_win_unlock_f
#pragma weak mpi_win_unlock = mpi_win_unlock_f
#pragma weak mpi_win_unlock_ = mpi_win_unlock_f
#pragma weak mpi_win_unlock__ = mpi_win_unlock_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_UNLOCK,
                           mpi_win_unlock,
                           mpi_win_unlock_,
                           mpi_win_unlock__,
                           mpi_win_unlock_f,
                           (MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr),
                           (rank, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_win_unlock_f(MPI_Fint *rank, MPI_Fint *win, MPI_Fint *ierr)
{

}
