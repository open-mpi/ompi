/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_START = ompi_win_start_f
#pragma weak pmpi_win_start = ompi_win_start_f
#pragma weak pmpi_win_start_ = ompi_win_start_f
#pragma weak pmpi_win_start__ = ompi_win_start_f

#pragma weak PMPI_Win_start_f = ompi_win_start_f
#pragma weak PMPI_Win_start_f08 = ompi_win_start_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_START,
                           pmpi_win_start,
                           pmpi_win_start_,
                           pmpi_win_start__,
                           pompi_win_start_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_START = ompi_win_start_f
#pragma weak mpi_win_start = ompi_win_start_f
#pragma weak mpi_win_start_ = ompi_win_start_f
#pragma weak mpi_win_start__ = ompi_win_start_f

#pragma weak MPI_Win_start_f = ompi_win_start_f
#pragma weak MPI_Win_start_f08 = ompi_win_start_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_START,
                           mpi_win_start,
                           mpi_win_start_,
                           mpi_win_start__,
                           ompi_win_start_f,
                           (MPI_Fint *group, MPI_Fint *assert, MPI_Fint *win, MPI_Fint *ierr),
                           (group, assert, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_start_f(MPI_Fint *group, MPI_Fint *assert,
		     MPI_Fint *win, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Group c_grp = MPI_Group_f2c(*group);
    MPI_Win c_win = MPI_Win_f2c(*win);

    c_ierr = MPI_Win_start(c_grp, 
                           OMPI_FINT_2_INT(*assert),
                           c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
}
