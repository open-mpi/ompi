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

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_WIN_SET_INFO = ompi_win_set_info_f
#pragma weak pmpi_win_set_info = ompi_win_set_info_f
#pragma weak pmpi_win_set_info_ = ompi_win_set_info_f
#pragma weak pmpi_win_set_info__ = ompi_win_set_info_f

#pragma weak PMPI_Win_set_info_f = ompi_win_set_info_f
#pragma weak PMPI_Win_set_info_f08 = ompi_win_set_info_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_SET_INFO,
                           pmpi_win_set_info,
                           pmpi_win_set_info_,
                           pmpi_win_set_info__,
                           pompi_win_set_info_f,
                           (MPI_Fint *win, MPI_Fint *info, MPI_Fint *ierr),
                           (win, info, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_SET_INFO = ompi_win_set_info_f
#pragma weak mpi_win_set_info = ompi_win_set_info_f
#pragma weak mpi_win_set_info_ = ompi_win_set_info_f
#pragma weak mpi_win_set_info__ = ompi_win_set_info_f

#pragma weak MPI_Win_set_info_f = ompi_win_set_info_f
#pragma weak MPI_Win_set_info_f08 = ompi_win_set_info_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_SET_INFO,
                           mpi_win_set_info,
                           mpi_win_set_info_,
                           mpi_win_set_info__,
                           ompi_win_set_info_f,
                           (MPI_Fint *win, MPI_Fint *info, MPI_Fint *ierr),
                           (win, info, ierr) )
#else
#define ompi_win_set_info_f pompi_win_set_info_f
#endif
#endif


void ompi_win_set_info_f(MPI_Fint *win, MPI_Fint *info, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;
    MPI_Info c_info;

    c_win = PMPI_Win_f2c(*win);
    c_info = PMPI_Info_f2c(*info);
    c_ierr = PMPI_Win_set_info(c_win, c_info);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
