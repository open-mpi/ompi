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
 * Copyright (c) 2015-2016 Research Organization for Information Science
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
#pragma weak PMPI_WIN_FREE_KEYVAL = ompi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval = ompi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval_ = ompi_win_free_keyval_f
#pragma weak pmpi_win_free_keyval__ = ompi_win_free_keyval_f

#pragma weak PMPI_Win_free_keyval_f = ompi_win_free_keyval_f
#pragma weak PMPI_Win_free_keyval_f08 = ompi_win_free_keyval_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_FREE_KEYVAL,
                           pmpi_win_free_keyval,
                           pmpi_win_free_keyval_,
                           pmpi_win_free_keyval__,
                           pompi_win_free_keyval_f,
                           (MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win_keyval, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_FREE_KEYVAL = ompi_win_free_keyval_f
#pragma weak mpi_win_free_keyval = ompi_win_free_keyval_f
#pragma weak mpi_win_free_keyval_ = ompi_win_free_keyval_f
#pragma weak mpi_win_free_keyval__ = ompi_win_free_keyval_f

#pragma weak MPI_Win_free_keyval_f = ompi_win_free_keyval_f
#pragma weak MPI_Win_free_keyval_f08 = ompi_win_free_keyval_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_FREE_KEYVAL,
                           mpi_win_free_keyval,
                           mpi_win_free_keyval_,
                           mpi_win_free_keyval__,
                           ompi_win_free_keyval_f,
                           (MPI_Fint *win_keyval, MPI_Fint *ierr),
                           (win_keyval, ierr) )
#else
#define ompi_win_free_keyval_f pompi_win_free_keyval_f
#endif
#endif


void ompi_win_free_keyval_f(MPI_Fint *win_keyval, MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_SINGLE_NAME_DECL(win_keyval);

    OMPI_SINGLE_FINT_2_INT(win_keyval);

    c_ierr = PMPI_Win_free_keyval(OMPI_SINGLE_NAME_CONVERT(win_keyval));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(win_keyval);
    }
}
