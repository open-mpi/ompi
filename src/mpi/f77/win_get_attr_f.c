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
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Aint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
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
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Aint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr),
                           (win, win_keyval, attribute_val, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_get_attr_f(MPI_Fint *win, MPI_Fint *win_keyval,
			MPI_Aint *attribute_val, MPI_Fint *flag, MPI_Fint *ierr)
{
    int c_err, c_flag;
    MPI_Win c_win;
    int *c_value;

    c_win = MPI_Win_f2c(*win);

    /* This stuff is very confusing.  Be sure to see MPI-2 4.12.7. */

    /* Didn't use all the FINT macros that could have prevented a few
       extra variables in this function, but I figured that the
       clarity of code, and the fact that this is not expected to be a
       high-performance function, was worth it */

    /* Note that there is no conversion on attribute_val -- MPI-2 says
       that it is supposed to be the right size already */

    c_err = MPI_Win_get_attr(c_win, OMPI_FINT_2_INT(*win_keyval),
                              &c_value, &c_flag);
    *ierr = OMPI_INT_2_FINT(c_err);
    *flag = OMPI_INT_2_FINT(c_flag);

    /* Note that MPI-2 4.12.7 specifically says that Fortran's
       xxx_GET_ATTR functions will take the address returned from C
       and "convert it to an integer" (which assumedly means
       dereference) */

    if (MPI_SUCCESS == c_err && 1 == c_flag) {
        *attribute_val = (MPI_Aint) *c_value;
    }
}
