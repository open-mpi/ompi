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
#pragma weak PMPI_IS_THREAD_MAIN = ompi_is_thread_main_f
#pragma weak pmpi_is_thread_main = ompi_is_thread_main_f
#pragma weak pmpi_is_thread_main_ = ompi_is_thread_main_f
#pragma weak pmpi_is_thread_main__ = ompi_is_thread_main_f

#pragma weak PMPI_Is_thread_main_f = ompi_is_thread_main_f
#pragma weak PMPI_Is_thread_main_f08 = ompi_is_thread_main_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_IS_THREAD_MAIN,
                           pmpi_is_thread_main,
                           pmpi_is_thread_main_,
                           pmpi_is_thread_main__,
                           pompi_is_thread_main_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_IS_THREAD_MAIN = ompi_is_thread_main_f
#pragma weak mpi_is_thread_main = ompi_is_thread_main_f
#pragma weak mpi_is_thread_main_ = ompi_is_thread_main_f
#pragma weak mpi_is_thread_main__ = ompi_is_thread_main_f

#pragma weak MPI_Is_thread_main_f = ompi_is_thread_main_f
#pragma weak MPI_Is_thread_main_f08 = ompi_is_thread_main_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_IS_THREAD_MAIN,
                           mpi_is_thread_main,
                           mpi_is_thread_main_,
                           mpi_is_thread_main__,
                           ompi_is_thread_main_f,
                           (ompi_fortran_logical_t *flag, MPI_Fint *ierr),
                           (flag, ierr) )
#else
#define ompi_is_thread_main_f pompi_is_thread_main_f
#endif
#endif


void ompi_is_thread_main_f(ompi_fortran_logical_t *flag, MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_LOGICAL_NAME_DECL(flag);

    c_ierr = PMPI_Is_thread_main(OMPI_LOGICAL_SINGLE_NAME_CONVERT(flag));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_LOGICAL(flag);
    }
}
