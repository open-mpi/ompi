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
#pragma weak PMPI_QUERY_THREAD = ompi_query_thread_f
#pragma weak pmpi_query_thread = ompi_query_thread_f
#pragma weak pmpi_query_thread_ = ompi_query_thread_f
#pragma weak pmpi_query_thread__ = ompi_query_thread_f

#pragma weak PMPI_Query_thread_f = ompi_query_thread_f
#pragma weak PMPI_Query_thread_f08 = ompi_query_thread_f
#else
OMPI_GENERATE_F77_BINDINGS (PMPI_QUERY_THREAD,
                           pmpi_query_thread,
                           pmpi_query_thread_,
                           pmpi_query_thread__,
                           pompi_query_thread_f,
                           (MPI_Fint *provided, MPI_Fint *ierr),
                           (provided, ierr) )
#endif
#endif

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_QUERY_THREAD = ompi_query_thread_f
#pragma weak mpi_query_thread = ompi_query_thread_f
#pragma weak mpi_query_thread_ = ompi_query_thread_f
#pragma weak mpi_query_thread__ = ompi_query_thread_f

#pragma weak MPI_Query_thread_f = ompi_query_thread_f
#pragma weak MPI_Query_thread_f08 = ompi_query_thread_f
#else
#if ! OMPI_BUILD_MPI_PROFILING
OMPI_GENERATE_F77_BINDINGS (MPI_QUERY_THREAD,
                           mpi_query_thread,
                           mpi_query_thread_,
                           mpi_query_thread__,
                           ompi_query_thread_f,
                           (MPI_Fint *provided, MPI_Fint *ierr),
                           (provided, ierr) )
#else
#define ompi_query_thread_f pompi_query_thread_f
#endif
#endif


void ompi_query_thread_f(MPI_Fint *provided, MPI_Fint *ierr)
{
    int c_ierr;
    OMPI_SINGLE_NAME_DECL(provided);

    c_ierr = PMPI_Query_thread(OMPI_SINGLE_NAME_CONVERT(provided));
    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);

    if (MPI_SUCCESS == c_ierr) {
        OMPI_SINGLE_INT_2_FINT(provided);
    }
}
