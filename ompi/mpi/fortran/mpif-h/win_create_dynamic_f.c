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
#pragma weak PMPI_WIN_CREATE_DYNAMIC = ompi_win_create_dynamic_f
#pragma weak pmpi_win_create_dynamic = ompi_win_create_dynamic_f
#pragma weak pmpi_win_create_dynamic_ = ompi_win_create_dynamic_f
#pragma weak pmpi_win_create_dynamic__ = ompi_win_create_dynamic_f

#pragma weak PMPI_Win_create_dynamic_f = ompi_win_create_dynamic_f
#pragma weak PMPI_Win_create_dynamic_f08 = ompi_win_create_dynamic_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_CREATE_DYNAMIC,
                           pmpi_win_create_dynamic,
                           pmpi_win_create_dynamic_,
                           pmpi_win_create_dynamic__,
                           pompi_win_create_dynamic_f,
                           (MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (info, comm, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_CREATE_DYNAMIC = ompi_win_create_dynamic_f
#pragma weak mpi_win_create_dynamic = ompi_win_create_dynamic_f
#pragma weak mpi_win_create_dynamic_ = ompi_win_create_dynamic_f
#pragma weak mpi_win_create_dynamic__ = ompi_win_create_dynamic_f

#pragma weak MPI_Win_create_dynamic_f = ompi_win_create_dynamic_f
#pragma weak MPI_Win_create_dynamic_f08 = ompi_win_create_dynamic_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_CREATE_DYNAMIC,
                           mpi_win_create_dynamic,
                           mpi_win_create_dynamic_,
                           mpi_win_create_dynamic__,
                           ompi_win_create_dynamic_f,
                           (MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (info, comm, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_create_dynamic_f(MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win,
		              MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;
    MPI_Info c_info;
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);

    c_ierr = MPI_Win_create_dynamic(c_info, c_comm, &c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
       *win = MPI_Win_c2f(c_win);
    }
}
