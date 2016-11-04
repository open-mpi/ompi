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
#pragma weak PMPI_WIN_ATTACH = ompi_win_attach_f
#pragma weak pmpi_win_attach = ompi_win_attach_f
#pragma weak pmpi_win_attach_ = ompi_win_attach_f
#pragma weak pmpi_win_attach__ = ompi_win_attach_f

#pragma weak PMPI_Win_attach_f = ompi_win_attach_f
#pragma weak PMPI_Win_attach_f08 = ompi_win_attach_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_ATTACH,
                           pmpi_win_attach,
                           pmpi_win_attach_,
                           pmpi_win_attach__,
                           pompi_win_attach_f,
                           (MPI_Fint *win, char *base, MPI_Aint *size, MPI_Fint *ierr),
                           (win, base, size, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_ATTACH = ompi_win_attach_f
#pragma weak mpi_win_attach = ompi_win_attach_f
#pragma weak mpi_win_attach_ = ompi_win_attach_f
#pragma weak mpi_win_attach__ = ompi_win_attach_f

#pragma weak MPI_Win_attach_f = ompi_win_attach_f
#pragma weak MPI_Win_attach_f08 = ompi_win_attach_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_ATTACH,
                           mpi_win_attach,
                           mpi_win_attach_,
                           mpi_win_attach__,
                           ompi_win_attach_f,
                           (MPI_Fint *win, char *base, MPI_Aint *size, MPI_Fint *ierr),
                           (win, base, size, ierr) )
#else
#define ompi_win_attach_f pompi_win_attach_f
#endif
#endif


void ompi_win_attach_f(MPI_Fint *win, char *base, MPI_Aint *size,
		      MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;

    c_win = PMPI_Win_f2c(*win);
    c_ierr = PMPI_Win_attach(c_win, base, *size);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
