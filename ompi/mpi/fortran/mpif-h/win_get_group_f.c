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
#pragma weak PMPI_WIN_GET_GROUP = ompi_win_get_group_f
#pragma weak pmpi_win_get_group = ompi_win_get_group_f
#pragma weak pmpi_win_get_group_ = ompi_win_get_group_f
#pragma weak pmpi_win_get_group__ = ompi_win_get_group_f

#pragma weak PMPI_Win_get_group_f = ompi_win_get_group_f
#pragma weak PMPI_Win_get_group_f08 = ompi_win_get_group_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_GROUP,
                           pmpi_win_get_group,
                           pmpi_win_get_group_,
                           pmpi_win_get_group__,
                           pompi_win_get_group_f,
                           (MPI_Fint *win, MPI_Fint *group, MPI_Fint *ierr),
                           (win, group, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_GROUP = ompi_win_get_group_f
#pragma weak mpi_win_get_group = ompi_win_get_group_f
#pragma weak mpi_win_get_group_ = ompi_win_get_group_f
#pragma weak mpi_win_get_group__ = ompi_win_get_group_f

#pragma weak MPI_Win_get_group_f = ompi_win_get_group_f
#pragma weak MPI_Win_get_group_f08 = ompi_win_get_group_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_GROUP,
                           mpi_win_get_group,
                           mpi_win_get_group_,
                           mpi_win_get_group__,
                           ompi_win_get_group_f,
                           (MPI_Fint *win, MPI_Fint *group, MPI_Fint *ierr),
                           (win, group, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_get_group_f(MPI_Fint *win, MPI_Fint *group, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Group c_grp;
    MPI_Win c_win = MPI_Win_f2c(*win);

    c_ierr = MPI_Win_get_group(c_win, &c_grp);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *group = MPI_Group_c2f(c_grp);
    }
}
