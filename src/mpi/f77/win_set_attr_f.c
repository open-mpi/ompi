/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_SET_ATTR = mpi_win_set_attr_f
#pragma weak pmpi_win_set_attr = mpi_win_set_attr_f
#pragma weak pmpi_win_set_attr_ = mpi_win_set_attr_f
#pragma weak pmpi_win_set_attr__ = mpi_win_set_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_SET_ATTR,
                           pmpi_win_set_attr,
                           pmpi_win_set_attr_,
                           pmpi_win_set_attr__,
                           pmpi_win_set_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Aint *attribute_val, MPI_Fint *ierr),
                           (win, win_keyval, attribute_val, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_SET_ATTR = mpi_win_set_attr_f
#pragma weak mpi_win_set_attr = mpi_win_set_attr_f
#pragma weak mpi_win_set_attr_ = mpi_win_set_attr_f
#pragma weak mpi_win_set_attr__ = mpi_win_set_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_SET_ATTR,
                           mpi_win_set_attr,
                           mpi_win_set_attr_,
                           mpi_win_set_attr__,
                           mpi_win_set_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Aint *attribute_val, MPI_Fint *ierr),
                           (win, win_keyval, attribute_val, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_win_set_attr_f(MPI_Fint *win, MPI_Fint *win_keyval,
			MPI_Aint *attribute_val, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c( *win );

    *ierr = OMPI_INT_2_FINT(MPI_Win_set_attr( c_win, 
					      OMPI_FINT_2_INT(*win_keyval),
					      attribute_val ));
}
