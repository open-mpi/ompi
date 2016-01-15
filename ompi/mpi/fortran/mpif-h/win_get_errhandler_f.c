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
#pragma weak PMPI_WIN_GET_ERRHANDLER = ompi_win_get_errhandler_f
#pragma weak pmpi_win_get_errhandler = ompi_win_get_errhandler_f
#pragma weak pmpi_win_get_errhandler_ = ompi_win_get_errhandler_f
#pragma weak pmpi_win_get_errhandler__ = ompi_win_get_errhandler_f

#pragma weak PMPI_Win_get_errhandler_f = ompi_win_get_errhandler_f
#pragma weak PMPI_Win_get_errhandler_f08 = ompi_win_get_errhandler_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_ERRHANDLER,
                           pmpi_win_get_errhandler,
                           pmpi_win_get_errhandler_,
                           pmpi_win_get_errhandler__,
                           pompi_win_get_errhandler_f,
                           (MPI_Fint *win, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (win, errhandler, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_ERRHANDLER = ompi_win_get_errhandler_f
#pragma weak mpi_win_get_errhandler = ompi_win_get_errhandler_f
#pragma weak mpi_win_get_errhandler_ = ompi_win_get_errhandler_f
#pragma weak mpi_win_get_errhandler__ = ompi_win_get_errhandler_f

#pragma weak MPI_Win_get_errhandler_f = ompi_win_get_errhandler_f
#pragma weak MPI_Win_get_errhandler_f08 = ompi_win_get_errhandler_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_ERRHANDLER,
                           mpi_win_get_errhandler,
                           mpi_win_get_errhandler_,
                           mpi_win_get_errhandler__,
                           ompi_win_get_errhandler_f,
                           (MPI_Fint *win, MPI_Fint *errhandler, MPI_Fint *ierr),
                           (win, errhandler, ierr) )
#else
#define ompi_win_get_errhandler_f pompi_win_get_errhandler_f
#endif
#endif


void ompi_win_get_errhandler_f(MPI_Fint *win, MPI_Fint *errhandler,
			      MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Errhandler c_err;
    MPI_Win c_win = PMPI_Win_f2c(*win);

    c_ierr = PMPI_Win_get_errhandler(c_win, &c_err);
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        *errhandler = PMPI_Errhandler_c2f(c_err);
    }
}
