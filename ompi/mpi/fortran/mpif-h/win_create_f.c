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
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/fortran/mpif-h/bindings.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILE_LAYER
#pragma weak PMPI_WIN_CREATE = ompi_win_create_f
#pragma weak pmpi_win_create = ompi_win_create_f
#pragma weak pmpi_win_create_ = ompi_win_create_f
#pragma weak pmpi_win_create__ = ompi_win_create_f

#pragma weak PMPI_Win_create_f = ompi_win_create_f
#pragma weak PMPI_Win_create_f08 = ompi_win_create_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_CREATE,
                           pmpi_win_create,
                           pmpi_win_create_,
                           pmpi_win_create__,
                           pompi_win_create_f,
                           (char *base, MPI_Aint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_CREATE = ompi_win_create_f
#pragma weak mpi_win_create = ompi_win_create_f
#pragma weak mpi_win_create_ = ompi_win_create_f
#pragma weak mpi_win_create__ = ompi_win_create_f

#pragma weak MPI_Win_create_f = ompi_win_create_f
#pragma weak MPI_Win_create_f08 = ompi_win_create_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_CREATE,
                           mpi_win_create,
                           mpi_win_create_,
                           mpi_win_create__,
                           ompi_win_create_f,
                           (char *base, MPI_Aint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_create_f(char *base, MPI_Aint *size, MPI_Fint *disp_unit,
		      MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win,
		      MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;
    MPI_Info c_info;
    MPI_Comm c_comm;

    c_comm = MPI_Comm_f2c(*comm);
    c_info = MPI_Info_f2c(*info);

    c_ierr = MPI_Win_create(base, *size,
                            OMPI_FINT_2_INT(*disp_unit),
                            c_info, c_comm, &c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
       *win = MPI_Win_c2f(c_win);
    }
}
