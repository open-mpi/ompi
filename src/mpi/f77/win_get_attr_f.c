/*
 * $HEADER$
 */

#include "lam_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILE_LAYER
#pragma weak PMPI_WIN_GET_ATTR = mpi_win_get_attr_f
#pragma weak pmpi_win_get_attr = mpi_win_get_attr_f
#pragma weak pmpi_win_get_attr_ = mpi_win_get_attr_f
#pragma weak pmpi_win_get_attr__ = mpi_win_get_attr_f
#elif LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (PMPI_WIN_GET_ATTR,
                           pmpi_win_get_attr,
                           pmpi_win_get_attr_,
                           pmpi_win_get_attr__,
                           pmpi_win_get_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, win_keyval, attribute_val, flag, ierr) )
#endif

#if LAM_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_ATTR = mpi_win_get_attr_f
#pragma weak mpi_win_get_attr = mpi_win_get_attr_f
#pragma weak mpi_win_get_attr_ = mpi_win_get_attr_f
#pragma weak mpi_win_get_attr__ = mpi_win_get_attr_f
#endif

#if ! LAM_HAVE_WEAK_SYMBOLS && ! LAM_PROFILE_LAYER
LAM_GENERATE_F77_BINDINGS (MPI_WIN_GET_ATTR,
                           mpi_win_get_attr,
                           mpi_win_get_attr_,
                           mpi_win_get_attr__,
                           mpi_win_get_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, win_keyval, attribute_val, flag, ierr) )
#endif


#if LAM_PROFILE_LAYER && ! LAM_HAVE_WEAK_SYMBOLS
#include "mpi/c/profile/defines.h"
#endif

void mpi_win_get_attr_f(MPI_Fint *win, MPI_Fint *win_keyval, char *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{

}
