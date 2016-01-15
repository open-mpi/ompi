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
#pragma weak PMPI_WIN_CREATE_DYNAMIC = ompi_win_create_dynamic_f
#pragma weak pmpi_win_create_dynamic = ompi_win_create_dynamic_f
#pragma weak pmpi_win_create_dynamic_ = ompi_win_create_dynamic_f
#pragma weak pmpi_win_create_dynamic__ = ompi_win_create_dynamic_f

#pragma weak PMPI_Win_create_dynamic_f = ompi_win_create_dynamic_f
#pragma weak PMPI_Win_create_dynamic_f08 = ompi_win_create_dynamic_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_CREATE_DYNAMIC,
                           pmpi_win_create_dynamic,
                           pmpi_win_create_dynamic_,
                           pmpi_win_create_dynamic__,
                           pompi_win_create_dynamic_f,
                           (MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (info, comm, win, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_CREATE_DYNAMIC = ompi_win_create_dynamic_f
#pragma weak mpi_win_create_dynamic = ompi_win_create_dynamic_f
#pragma weak mpi_win_create_dynamic_ = ompi_win_create_dynamic_f
#pragma weak mpi_win_create_dynamic__ = ompi_win_create_dynamic_f

#pragma weak MPI_Win_create_dynamic_f = ompi_win_create_dynamic_f
#pragma weak MPI_Win_create_dynamic_f08 = ompi_win_create_dynamic_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_CREATE_DYNAMIC,
                           mpi_win_create_dynamic,
                           mpi_win_create_dynamic_,
                           mpi_win_create_dynamic__,
                           ompi_win_create_dynamic_f,
                           (MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (info, comm, win, ierr) )
#else
#define ompi_win_create_dynamic_f pompi_win_create_dynamic_f
#endif
#endif


void ompi_win_create_dynamic_f(MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win,
		              MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;
    MPI_Info c_info;
    MPI_Comm c_comm;

    c_comm = PMPI_Comm_f2c(*comm);
    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Win_create_dynamic(c_info, c_comm, &c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
       *win = PMPI_Win_c2f(c_win);
    }
}
