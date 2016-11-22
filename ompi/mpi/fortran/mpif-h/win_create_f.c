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
#pragma weak PMPI_WIN_CREATE = ompi_win_create_f
#pragma weak pmpi_win_create = ompi_win_create_f
#pragma weak pmpi_win_create_ = ompi_win_create_f
#pragma weak pmpi_win_create__ = ompi_win_create_f

#pragma weak PMPI_Win_create_f = ompi_win_create_f
#pragma weak PMPI_Win_create_f08 = ompi_win_create_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_CREATE,
                           pmpi_win_create,
                           pmpi_win_create_,
                           pmpi_win_create__,
                           pompi_win_create_f,
                           (char *base, MPI_Aint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_CREATE = ompi_win_create_f
#pragma weak mpi_win_create = ompi_win_create_f
#pragma weak mpi_win_create_ = ompi_win_create_f
#pragma weak mpi_win_create__ = ompi_win_create_f

#pragma weak MPI_Win_create_f = ompi_win_create_f
#pragma weak MPI_Win_create_f08 = ompi_win_create_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_CREATE,
                           mpi_win_create,
                           mpi_win_create_,
                           mpi_win_create__,
                           ompi_win_create_f,
                           (char *base, MPI_Aint *size, MPI_Fint *disp_unit, MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win, MPI_Fint *ierr),
                           (base, size, disp_unit, info, comm, win, ierr) )
#else
#define ompi_win_create_f pompi_win_create_f
#endif
#endif


void ompi_win_create_f(char *base, MPI_Aint *size, MPI_Fint *disp_unit,
		      MPI_Fint *info, MPI_Fint *comm, MPI_Fint *win,
		      MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win;
    MPI_Info c_info;
    MPI_Comm c_comm;

    c_comm = PMPI_Comm_f2c(*comm);
    c_info = PMPI_Info_f2c(*info);

    c_ierr = PMPI_Win_create(base, *size,
                            OMPI_FINT_2_INT(*disp_unit),
                            c_info, c_comm, &c_win);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
       *win = PMPI_Win_c2f(c_win);
    }
}
