/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_GET_NAME = mpi_win_get_name_f
#pragma weak pmpi_win_get_name = mpi_win_get_name_f
#pragma weak pmpi_win_get_name_ = mpi_win_get_name_f
#pragma weak pmpi_win_get_name__ = mpi_win_get_name_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_NAME,
                           pmpi_win_get_name,
                           pmpi_win_get_name_,
                           pmpi_win_get_name__,
                           pmpi_win_get_name_f,
                           (MPI_Fint *win, char *win_name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (win, win_name, resultlen, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_NAME = mpi_win_get_name_f
#pragma weak mpi_win_get_name = mpi_win_get_name_f
#pragma weak mpi_win_get_name_ = mpi_win_get_name_f
#pragma weak mpi_win_get_name__ = mpi_win_get_name_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_NAME,
                           mpi_win_get_name,
                           mpi_win_get_name_,
                           mpi_win_get_name__,
                           mpi_win_get_name_f,
                           (MPI_Fint *win, char *win_name, MPI_Fint *resultlen, MPI_Fint *ierr),
                           (win, win_name, resultlen, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_win_get_name_f(MPI_Fint *win, char *win_name, MPI_Fint *resultlen, MPI_Fint *ierr)
{

}
