/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_GET_GROUP = mpi_win_get_group_f
#pragma weak pmpi_win_get_group = mpi_win_get_group_f
#pragma weak pmpi_win_get_group_ = mpi_win_get_group_f
#pragma weak pmpi_win_get_group__ = mpi_win_get_group_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_GROUP,
                           pmpi_win_get_group,
                           pmpi_win_get_group_,
                           pmpi_win_get_group__,
                           pmpi_win_get_group_f,
                           (MPI_Fint *win, MPI_Fint *group, MPI_Fint *ierr),
                           (win, group, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_GROUP = mpi_win_get_group_f
#pragma weak mpi_win_get_group = mpi_win_get_group_f
#pragma weak mpi_win_get_group_ = mpi_win_get_group_f
#pragma weak mpi_win_get_group__ = mpi_win_get_group_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_GROUP,
                           mpi_win_get_group,
                           mpi_win_get_group_,
                           mpi_win_get_group__,
                           mpi_win_get_group_f,
                           (MPI_Fint *win, MPI_Fint *group, MPI_Fint *ierr),
                           (win, group, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

OMPI_EXPORT
void mpi_win_get_group_f(MPI_Fint *win, MPI_Fint *group, MPI_Fint *ierr)
{
    MPI_Group c_grp;
    MPI_Win c_win = MPI_Win_f2c(*win);

    *ierr = OMPI_INT_2_FINT(MPI_Win_get_group(c_win, &c_grp));
    
    *group = MPI_Group_c2f(c_grp);
}
