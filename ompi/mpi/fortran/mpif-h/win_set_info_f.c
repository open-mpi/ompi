/*
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_SET_INFO = ompi_win_set_info_f
#pragma weak pmpi_win_set_info = ompi_win_set_info_f
#pragma weak pmpi_win_set_info_ = ompi_win_set_info_f
#pragma weak pmpi_win_set_info__ = ompi_win_set_info_f

#pragma weak PMPI_Win_create_f = ompi_win_set_info_f
#pragma weak PMPI_Win_create_f08 = ompi_win_set_info_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_SET_INFO,
                           pmpi_win_set_info,
                           pmpi_win_set_info_,
                           pmpi_win_set_info__,
                           pompi_win_set_info_f,
                           (MPI_Fint *win, MPI_Fint *info, MPI_Fint *ierr),
                           (win, info, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_SET_INFO = ompi_win_set_info_f
#pragma weak mpi_win_set_info = ompi_win_set_info_f
#pragma weak mpi_win_set_info_ = ompi_win_set_info_f
#pragma weak mpi_win_set_info__ = ompi_win_set_info_f

#pragma weak MPI_Win_create_f = ompi_win_set_info_f
#pragma weak MPI_Win_create_f08 = ompi_win_set_info_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_SET_INFO,
                           mpi_win_set_info,
                           mpi_win_set_info_,
                           mpi_win_set_info__,
                           ompi_win_set_info_f,
                           (MPI_Fint *win, MPI_Fint *info, MPI_Fint *ierr),
                           (win, info, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_set_info_f(MPI_Fint *win, MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;
    MPI_Info c_info;

    c_win = MPI_Win_f2c(*win);
    c_info = MPI_Info_f2c(*info);
    c_ierr = MPI_Win_set_info(c_win, c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
