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
#pragma weak PMPI_WIN_TEST = ompi_win_test_f
#pragma weak pmpi_win_test = ompi_win_test_f
#pragma weak pmpi_win_test_ = ompi_win_test_f
#pragma weak pmpi_win_test__ = ompi_win_test_f

#pragma weak PMPI_Win_test_f = ompi_win_test_f
#pragma weak PMPI_Win_test_f08 = ompi_win_test_f
#elif OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_TEST,
                           pmpi_win_test,
                           pmpi_win_test_,
                           pmpi_win_test__,
                           pompi_win_test_f,
                           (MPI_Fint *win, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (win, flag, ierr) )
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_TEST = ompi_win_test_f
#pragma weak mpi_win_test = ompi_win_test_f
#pragma weak mpi_win_test_ = ompi_win_test_f
#pragma weak mpi_win_test__ = ompi_win_test_f

#pragma weak MPI_Win_test_f = ompi_win_test_f
#pragma weak MPI_Win_test_f08 = ompi_win_test_f
#endif

#if ! OPAL_HAVE_WEAK_SYMBOLS && ! OMPI_PROFILE_LAYER
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_TEST,
                           mpi_win_test,
                           mpi_win_test_,
                           mpi_win_test__,
                           ompi_win_test_f,
                           (MPI_Fint *win, ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (win, flag, ierr) )
#endif


#if OMPI_PROFILE_LAYER && ! OPAL_HAVE_WEAK_SYMBOLS
#include "ompi/mpi/fortran/mpif-h/profile/defines.h"
#endif

void ompi_win_test_f(MPI_Fint *win, ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    MPI_Win c_win = MPI_Win_f2c(*win);
    OMPI_LOGICAL_NAME_DECL(flag);

    c_ierr = MPI_Win_test(c_win, OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
