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
#pragma weak PMPI_WIN_DETACH = ompi_win_detach_f
#pragma weak pmpi_win_detach = ompi_win_detach_f
#pragma weak pmpi_win_detach_ = ompi_win_detach_f
#pragma weak pmpi_win_detach__ = ompi_win_detach_f

#pragma weak PMPI_Win_detach_f = ompi_win_detach_f
#pragma weak PMPI_Win_detach_f08 = ompi_win_detach_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_DETACH,
                           pmpi_win_detach,
                           pmpi_win_detach_,
                           pmpi_win_detach__,
                           pompi_win_detach_f,
                           (MPI_Fint *win, char *base, MPI_Fint *ierr),
                           (win, base, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_DETACH = ompi_win_detach_f
#pragma weak mpi_win_detach = ompi_win_detach_f
#pragma weak mpi_win_detach_ = ompi_win_detach_f
#pragma weak mpi_win_detach__ = ompi_win_detach_f

#pragma weak MPI_Win_detach_f = ompi_win_detach_f
#pragma weak MPI_Win_detach_f08 = ompi_win_detach_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_DETACH,
                           mpi_win_detach,
                           mpi_win_detach_,
                           mpi_win_detach__,
                           ompi_win_detach_f,
                           (MPI_Fint *win, char *base, MPI_Fint *ierr),
                           (win, base, ierr) )
#else
#define ompi_win_detach_f pompi_win_detach_f
#endif
#endif


void ompi_win_detach_f(MPI_Fint *win, char *base,
		      MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;

    c_win = PMPI_Win_f2c(*win);
    c_ierr = PMPI_Win_detach(c_win, base);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
