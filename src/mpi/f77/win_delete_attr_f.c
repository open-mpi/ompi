/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"
#include "attribute/attribute.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_DELETE_ATTR = mpi_win_delete_attr_f
#pragma weak pmpi_win_delete_attr = mpi_win_delete_attr_f
#pragma weak pmpi_win_delete_attr_ = mpi_win_delete_attr_f
#pragma weak pmpi_win_delete_attr__ = mpi_win_delete_attr_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_DELETE_ATTR,
                           pmpi_win_delete_attr,
                           pmpi_win_delete_attr_,
                           pmpi_win_delete_attr__,
                           pmpi_win_delete_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win, win_keyval, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_DELETE_ATTR = mpi_win_delete_attr_f
#pragma weak mpi_win_delete_attr = mpi_win_delete_attr_f
#pragma weak mpi_win_delete_attr_ = mpi_win_delete_attr_f
#pragma weak mpi_win_delete_attr__ = mpi_win_delete_attr_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_DELETE_ATTR,
                           mpi_win_delete_attr,
                           mpi_win_delete_attr_,
                           mpi_win_delete_attr__,
                           mpi_win_delete_attr_f,
                           (MPI_Fint *win, MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win, win_keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif
static const char FUNC_NAME[] = "MPI_Win_delete_attr_f";

void mpi_win_delete_attr_f(MPI_Fint *win, MPI_Fint *win_keyval, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c( *win );
    int ret, c_err; 

    if (MPI_PARAM_CHECK) {
        if (MPI_WIN_NULL == c_win) {
            c_err = OMPI_ERRHANDLER_INVOKE(c_win, MPI_ERR_WIN, 
					   FUNC_NAME);
	    *ierr = OMPI_INT_2_FINT(c_err);
        }
    }
  
    ret = ompi_attr_delete(WIN_ATTR, c_win, c_win->w_keyhash, *win_keyval, 
                           OMPI_KEYVAL_F77);

    if (MPI_SUCCESS != ret) {
        OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_OTHER, FUNC_NAME);  
    } else {
        *ierr = MPI_SUCCESS;
    }
}
