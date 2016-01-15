/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/constants.h"
#include "ompi/mpi/fortran/base/strings.h"


#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PMPI_WIN_GET_NAME = ompi_win_get_name_f
#pragma weak pmpi_win_get_name = ompi_win_get_name_f
#pragma weak pmpi_win_get_name_ = ompi_win_get_name_f
#pragma weak pmpi_win_get_name__ = ompi_win_get_name_f

#pragma weak PMPI_Win_get_name_f = ompi_win_get_name_f
#pragma weak PMPI_Win_get_name_f08 = ompi_win_get_name_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_WIN_GET_NAME,
                            pmpi_win_get_name,
                            pmpi_win_get_name_,
                            pmpi_win_get_name__,
                            pompi_win_get_name_f,
                            (MPI_Fint *win, char *win_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (win, win_name, resultlen, ierr, name_len) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_WIN_GET_NAME = ompi_win_get_name_f
#pragma weak mpi_win_get_name = ompi_win_get_name_f
#pragma weak mpi_win_get_name_ = ompi_win_get_name_f
#pragma weak mpi_win_get_name__ = ompi_win_get_name_f

#pragma weak MPI_Win_get_name_f = ompi_win_get_name_f
#pragma weak MPI_Win_get_name_f08 = ompi_win_get_name_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_WIN_GET_NAME,
                            mpi_win_get_name,
                            mpi_win_get_name_,
                            mpi_win_get_name__,
                            ompi_win_get_name_f,
                            (MPI_Fint *win, char *win_name, MPI_Fint *resultlen, MPI_Fint *ierr, int name_len),
                            (win, win_name, resultlen, ierr, name_len) )
#else
#define ompi_win_get_name_f pompi_win_get_name_f
#endif
#endif


void ompi_win_get_name_f(MPI_Fint *win, char *win_name,
			MPI_Fint *resultlen, MPI_Fint *ierr, int name_len)
{
    int c_ierr, c_len;
    MPI_Win c_win = PMPI_Win_f2c(*win);
    char c_name[MPI_MAX_OBJECT_NAME];

    c_ierr = PMPI_Win_get_name(c_win, c_name, &c_len);
    if (MPI_SUCCESS == c_ierr) {
        ompi_fortran_string_c2f(c_name, win_name, name_len);
        *resultlen = OMPI_INT_2_FINT(c_len);
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(MPI_SUCCESS);
    } else {
        if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);
    }

}
