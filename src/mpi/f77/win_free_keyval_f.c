/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "mpi/f77/bindings.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_FREE_KEYVAL = mpi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval = mpi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval_ = mpi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval__ = mpi_win_free_keyval_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_FREE_KEYVAL,
                           pmpi_win_free_keyval,
                           pmpi_win_free_keyval_,
                           pmpi_win_free_keyval__,
                           pmpi_win_free_keyval_f,
                           (MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win_keyval, ierr) )
#endif

#if OMPI_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_FREE_KEYVAL = mpi_win_free_keyval_f
#pragma weak mpi_win_free_keyval = mpi_win_free_keyval_f
#pragma weak mpi_win_free_keyval_ = mpi_win_free_keyval_f
#pragma weak mpi_win_free_keyval__ = mpi_win_free_keyval_f
#endif

#if ! OMPI_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_FREE_KEYVAL,
                           mpi_win_free_keyval,
                           mpi_win_free_keyval_,
                           mpi_win_free_keyval__,
                           mpi_win_free_keyval_f,
                           (MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win_keyval, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OMPI_HAVE_WEAK_SYMBOLS
#include "mpi/f77/profile/defines.h"
#endif

void mpi_win_free_keyval_f(MPI_Fint *win_keyval, MPI_Fint *ierr)
{
    OMPI_SINGLE_NAME_DECL(win_keyval);

    *ierr = OMPI_INT_2_FINT(MPI_Win_free_keyval(OMPI_SINGLE_NAME_CONVERT(win_keyval)));

    OMPI_SINGLE_INT_2_FINT(win_keyval);
}
