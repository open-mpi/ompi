/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_GET_ATTR = mpi_win_get_attr_f
#pragma weak pmpi_win_get_attr = mpi_win_get_attr_f
#pragma weak pmpi_win_get_attr_ = mpi_win_get_attr_f
#pragma weak pmpi_win_get_attr__ = mpi_win_get_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_ATTR,
                           pmpi_win_get_attr,
                           pmpi_win_get_attr_,
                           pmpi_win_get_attr__,
                           pmpi_win_get_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, win_keyval, attribute_val, flag, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_ATTR = mpi_win_get_attr_f
#pragma weak mpi_win_get_attr = mpi_win_get_attr_f
#pragma weak mpi_win_get_attr_ = mpi_win_get_attr_f
#pragma weak mpi_win_get_attr__ = mpi_win_get_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_ATTR,
                           mpi_win_get_attr,
                           mpi_win_get_attr_,
                           mpi_win_get_attr__,
                           mpi_win_get_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, win_keyval, attribute_val, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_get_attr_f(MPI_Fint *win, MPI_Fint *win_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c( *win );

    *ierr = MPI_Win_get_attr( c_win, *win_keyval,  attribute_val, flag );
}
