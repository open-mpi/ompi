/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_POST = mpi_win_post_f
#pragma weak pmpi_win_post = mpi_win_post_f
#pragma weak pmpi_win_post_ = mpi_win_post_f
#pragma weak pmpi_win_post__ = mpi_win_post_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_POST,
                           pmpi_win_post,
                           pmpi_win_post_,
                           pmpi_win_post__,
                           pmpi_win_post_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_POST = mpi_win_post_f
#pragma weak mpi_win_post = mpi_win_post_f
#pragma weak mpi_win_post_ = mpi_win_post_f
#pragma weak mpi_win_post__ = mpi_win_post_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_POST,
                           mpi_win_post,
                           mpi_win_post_,
                           mpi_win_post__,
                           mpi_win_post_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_post_f(MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr)
{

}
