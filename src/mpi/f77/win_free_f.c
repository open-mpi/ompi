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
#pragma weak PMPI_WIN_FREE = mpi_win_free_f
#pragma weak pmpi_win_free = mpi_win_free_f
#pragma weak pmpi_win_free_ = mpi_win_free_f
#pragma weak pmpi_win_free__ = mpi_win_free_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_FREE,
                           pmpi_win_free,
                           pmpi_win_free_,
                           pmpi_win_free__,
                           pmpi_win_free_f,
                           (MPI_Fint *win, MPI_Fint *ierr),
                           (win, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_FREE = mpi_win_free_f
#pragma weak mpi_win_free = mpi_win_free_f
#pragma weak mpi_win_free_ = mpi_win_free_f
#pragma weak mpi_win_free__ = mpi_win_free_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_FREE,
                           mpi_win_free,
                           mpi_win_free_,
                           mpi_win_free__,
                           mpi_win_free_f,
                           (MPI_Fint *win, MPI_Fint *ierr),
                           (win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_free_f(MPI_Fint *win, MPI_Fint *ierr)
{
    MPI_Win c_win = MPI_Win_f2c(*win);

    *ierr = OMPI_INT_2_FINT(MPI_Win_free(&c_win));

    *win = MPI_Win_c2f(c_win);
}
