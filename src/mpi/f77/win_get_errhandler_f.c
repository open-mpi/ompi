/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_GET_ERRHANDLER = mpi_win_get_errhandler_f
#pragma weak pmpi_win_get_errhandler = mpi_win_get_errhandler_f
#pragma weak pmpi_win_get_errhandler_ = mpi_win_get_errhandler_f
#pragma weak pmpi_win_get_errhandler__ = mpi_win_get_errhandler_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_ERRHANDLER,
                           pmpi_win_get_errhandler,
                           pmpi_win_get_errhandler_,
                           pmpi_win_get_errhandler__,
                           pmpi_win_get_errhandler_f,
                           (MPI_Fint *win, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (win, errhandler, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_ERRHANDLER = mpi_win_get_errhandler_f
#pragma weak mpi_win_get_errhandler = mpi_win_get_errhandler_f
#pragma weak mpi_win_get_errhandler_ = mpi_win_get_errhandler_f
#pragma weak mpi_win_get_errhandler__ = mpi_win_get_errhandler_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_ERRHANDLER,
                           mpi_win_get_errhandler,
                           mpi_win_get_errhandler_,
                           mpi_win_get_errhandler__,
                           mpi_win_get_errhandler_f,
                           (MPI_Fint *win, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (win, errhandler, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_get_errhandler_f(MPI_Fint *win, MPI_Fint *errhandler, 
			      MPI_Fint *ierr)
{
    MPI_Errhandler c_err;
    MPI_Win c_win = MPI_Win_f2c(*win);

    *ierr = OMPI_INT_2_FINT(MPI_Win_get_errhandler(c_win, &c_err));

    *errhandler = MPI_Errhandler_c2f(c_err);
}
