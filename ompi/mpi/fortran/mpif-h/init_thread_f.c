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
#pragma weak PMPI_INIT_THREAD = ompi_init_thread_f
#pragma weak pmpi_init_thread = ompi_init_thread_f
#pragma weak pmpi_init_thread_ = ompi_init_thread_f
#pragma weak pmpi_init_thread__ = ompi_init_thread_f

#pragma weak PMPI_Init_thread_f = ompi_init_thread_f
#pragma weak PMPI_Init_thread_f08 = ompi_init_thread_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_INIT_THREAD,
                           pmpi_init_thread,
                           pmpi_init_thread_,
                           pmpi_init_thread__,
                           pompi_init_thread_f,
                           (MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (required, provided, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_INIT_THREAD = ompi_init_thread_f
#pragma weak mpi_init_thread = ompi_init_thread_f
#pragma weak mpi_init_thread_ = ompi_init_thread_f
#pragma weak mpi_init_thread__ = ompi_init_thread_f

#pragma weak MPI_Init_thread_f = ompi_init_thread_f
#pragma weak MPI_Init_thread_f08 = ompi_init_thread_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_INIT_THREAD,
                           mpi_init_thread,
                           mpi_init_thread_,
                           mpi_init_thread__,
                           ompi_init_thread_f,
                           (MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr),
                           (required, provided, ierr) )
#else
#define ompi_init_thread_f pompi_init_thread_f
#endif
#endif


void ompi_init_thread_f( MPI_Fint *required, MPI_Fint *provided, MPI_Fint *ierr )
{
    int c_ierr;
    int argc = 0;
    char** argv = NULL;
    OMPI_SINGLE_NAME_DECL(provided);

    c_ierr = PMPI_Init_thread(&argc, &argv,
                             OMPI_FINT_2_INT(*required),
                             OMPI_SINGLE_NAME_CONVERT(provided));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(provided);
    }
}
